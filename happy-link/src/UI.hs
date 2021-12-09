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
  , lookupExtent
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
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
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
-- import Lens.Micro ((^.))
import Lens.Micro
import Lens.Micro.TH

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
-- type Name = ()

data Cell = Snake | Food | Empty | Bg Char | Fg Char

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

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
-- handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
-- handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
-- handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
-- handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ g & score .~ (read (unlines $ E.getEditContents $ g^.pos_x1)::Int)
handleEvent g (T.MouseDown n _ _ loc) = continue $ g & lastReportedClick .~ Just (n, loc)
-- handleEvent g T.MouseUp {} = continue $ g & lastReportedClick .~ Nothing
-- handleEvent g (VtyEvent (V.EvMouseDown col row button mods)) = continue $ g & click_pos .~ Just (col, row)
-- handleEvent g (VtyEvent V.EvMouseUp {}) = continue $ g & lastReportedClick .~ Nothing
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 't') [])) = continue $ eliminate g
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

-- data Name = Edit1
--           | Edit2
--           deriving (Ord, Show, Eq)

-- data St =
--     St { _focusRing :: F.FocusRing Name
--        , _edit1 :: E.Editor String Name
--        , _edit2 :: E.Editor String Name
--        }

-- initialState :: St
-- initialState =
--     St (F.focusRing [Edit1, Edit2])
--        (E.editor Edit1 Nothing "")
--        (E.editor Edit2 (Just 2) "")

-- makeLenses ''St

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g <+> drawInput g]

drawInput :: Game -> Widget Name
drawInput g = hLimit 20
  $ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Position")
  $ C.hCenter
  $ padAll 1
  $ vBox [ str "X of Pos1: " <+> hLimit 30 (vLimit 5 px1)
         , str "Y of Pos1: " <+> hLimit 30 (vLimit 5 py1)
         , str "X of Pos2: " <+> hLimit 30 (vLimit 5 px2)
         , str "Y of Pos2: " <+> hLimit 30 (vLimit 5 py2)
         ]
  where
    px1 = F.withFocusRing (g ^. focusRing) (E.renderEditor (str . unlines)) (g ^. pos_x1)
    px2 = F.withFocusRing (g ^. focusRing) (E.renderEditor (str . unlines)) (g ^. pos_x2)
    py1 = F.withFocusRing (g ^. focusRing) (E.renderEditor (str . unlines)) (g ^. pos_y1)
    py2 = F.withFocusRing (g ^. focusRing) (E.renderEditor (str . unlines)) (g ^. pos_y2)

drawStats :: Game -> Widget Name
drawStats g = hLimit 30
  $ vBox [ drawScore (g ^. score) -- getter
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         , drawOutput msg
         ]
  where
    msg = case g ^. lastReportedClick of
            Nothing -> "nothing"
            Just (name, T.Location l)  -> show name <> " at " <> show l

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawOutput :: [Char] -> Widget Name
drawOutput s = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Output")
  $ C.hCenter
  $ padAll 1
  $ str s

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
            Nothing -> Bg $ (g ^. blocks) !! ((c ^._x) * width + (c ^._y))
            Just (name, T.Location l)  -> do
              if (((fst l - 1) `div` 3) == (c ^._x)) && (height - (snd l) - 1 == (c ^._y)) then
                Fg $ (g ^. blocks) !! ((c ^._x) * width + (c ^._y))
              else
                Bg $ (g ^. blocks) !! ((c ^._x) * width + (c ^._y))


drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw1
drawCell Food  = withAttr foodAttr cw1
drawCell Empty = withAttr emptyAttr cw1
drawCell (Bg c) = withAttr bgAttr $ cw c
drawCell (Fg c) = withAttr fgAttr $ cw c

cw :: Char -> Widget Name
cw c = str $ if c == ' ' then "   " else " " ++ (c : " ")

cw1 :: Widget Name
cw1 = str "   "

snakeAttr, foodAttr, emptyAttr, bgAttr, fgAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"
bgAttr = "bgAttr"
fgAttr = "fgAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr, V.blue `on` V.blue)
  , (foodAttr, V.red `on` V.red)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  , (fgAttr,            V.black `on` V.white)
  , (E.editAttr,                   V.white `on` V.black)
  , (E.editFocusedAttr,            V.black `on` V.white)
  ]