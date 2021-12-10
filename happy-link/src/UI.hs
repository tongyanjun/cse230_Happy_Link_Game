{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import LinkState

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg, bg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  , clickable
  , withDefAttr
  )
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import Data.Char (ord)
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
-- import Lens.Micro ((^.))
import Lens.Micro
import Lens.Micro.TH

data Tick = Tick
data Cell = Empty | NoFocus Char | Focus Char

-- App definition

appCursor :: Game -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = appCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app g

-- Handling events
transformCoord :: (Int, Int) -> (Int, Int)
transformCoord (x, y) = (y, (x-1) `div` 3)

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ g & score .~ (read (unlines $ E.getEditContents $ g^.pos_x1)::Int)
handleEvent g (T.MouseDown n _ _ (T.Location l)) =
  case g^.lastReportedClick of
    Nothing -> continue $ g & lastReportedClick .~ Just (n, T.Location (transformCoord l))
    Just (name, T.Location last_l) -> continue $ link (transformCoord l) last_l g & lastReportedClick .~ (Just (n, T.Location (transformCoord l)))
-- handleEvent g T.MouseUp {} = continue $ g & lastReportedClick .~ Nothing
-- handleEvent g (VtyEvent (V.EvMouseDown col row button mods)) = continue $ g & click_pos .~ Just (col, row)
-- handleEvent g (VtyEvent V.EvMouseUp {}) = continue $ g & lastReportedClick .~ Nothing
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = liftIO (shuffleGame g) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> halt g
        V.EvKey (V.KChar '\t') [] -> continue $ g & focusRing %~ F.focusNext
        V.EvKey V.KBackTab [] -> continue $ g & focusRing %~ F.focusPrev
        V.EvKey V.KUp [] -> continue $ g & focusRing %~ F.focusPrev
        V.EvKey V.KDown [] -> continue $ g & focusRing %~ F.focusNext
        -- V.EvKey V.KEnter [] -> continue $ g & focusRing %~ F.focusNext

        _ -> continue =<< case F.focusGetCurrent (g^.focusRing) of
               Just PosX1 -> T.handleEventLensed g pos_x1 E.handleEditorEvent ev
               Just PosY1 -> T.handleEventLensed g pos_y1 E.handleEditorEvent ev
               Just PosX2 -> T.handleEventLensed g pos_x2 E.handleEditorEvent ev
               Just PosY2 -> T.handleEventLensed g pos_y2 E.handleEditorEvent ev
               Nothing -> return g
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> padRight (Pad 2) (drawGrid g) <+> drawRight g]


drawInstructions :: Game -> Widget Name
drawInstructions g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Instructions")
  $ C.hCenter
  $ padAll 1
  $ str "r -> restart\ns -> shuffle\nq -> quit"
  

drawStats :: Game -> Widget Name
drawStats g = hLimit 10
  $ vBox [ drawScore (g ^. score)] -- getter

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawOutput :: Game -> Widget Name
drawOutput g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Output")
  $ C.hCenter
  $ padAll 1
  $ str msg
  where
    msg = case g ^. lastReportedClick of
            Nothing -> "nothing"
            Just (name, T.Location l)  -> show name <> " at " <> show l

drawRight :: Game -> Widget Name
drawRight g = hLimit 20
  $ vBox [ drawInstructions g
         , padTop (Pad 1) $ drawGameOver (g ^. dead)
         , drawOutput g
         ]

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

gameOverAttr :: AttrName
gameOverAttr = "gameOver"


drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Happy Link")
  $ clickable Board
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c     = do
      case g ^. lastReportedClick of
            Nothing -> NoFocus $ (g ^. cells) !! (height - c ^._y - 1) !! (c ^._x)
            Just (name, T.Location l)  -> do
              if (snd l == (c ^._x)) && (fst l == (height - c ^._y - 1)) then
                Focus $ (g ^. cells) !! (height - c ^._y - 1) !! (c ^._x)
              else
                NoFocus $ (g ^. cells) !! (height - c ^._y - 1) !! (c ^._x)

getColorOfChar :: Char -> AttrName
getColorOfChar c = case c of
  ' ' -> emptyAttr
  otherwise -> [redAttr, greenAttr, yellowAttr, blueAttr, magentaAttr, cyanAttr] !! ((ord(c)  - 65) `mod` 6)

drawCell :: Cell -> Widget Name
drawCell (NoFocus c) = withAttr (getColorOfChar c) $ cw c
drawCell (Focus c) = withAttr E.editFocusedAttr $ cw c

cw :: Char -> Widget Name
cw c = str $ " " ++ (c : " ")

emptyAttr, redAttr, greenAttr, yellowAttr, blueAttr, magentaAttr, cyanAttr :: AttrName
emptyAttr = "emptyAttr"
redAttr = "redAttr"
greenAttr = "greenAttr"
yellowAttr = "yellowAttr"
blueAttr = "blueAttr"
magentaAttr = "magentaAttr"
cyanAttr = "cyanAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr $
  [ (gameOverAttr, fg V.red `V.withStyle` V.bold)
  , (emptyAttr, V.white `on` V.black)
  , (redAttr, V.black `on` V.red)
  , (greenAttr, V.black `on` V.green)
  , (yellowAttr, V.black `on`V.yellow)
  , (blueAttr, V.black `on` V.blue)
  , (magentaAttr, V.black `on` V.magenta)
  , (cyanAttr, V.black `on` V.cyan)
  , (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.white)
  ]
