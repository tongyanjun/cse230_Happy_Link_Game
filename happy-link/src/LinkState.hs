{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module LinkState
  ( initGame
  , step
  -- , eliminate
  , Game(..)
  , dead, score, cells, info, linkable, countdown, paused, win
  , focusRing, pos_x1, pos_y1, pos_x2, pos_y2
  , lastReportedClick
  , height, width
  , Name(..)
  , link
  , isLinkable
  , isLinkable0
  , isLinkable1
  , isLinkable2
  , shuffleGame
  ) where

import Data.Array.IO
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Types as T
import Brick.Util (on)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM, forM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen, randomRIO)
import Prelude

-- Types

data Name = PosX1
          | PosY1
          | PosX2
          | PosY2
          | Board
          deriving (Ord, Show, Eq)

data Game = Game
  { _dead   :: Bool         -- ^ game over flag
  , _paused :: Bool         -- ^ paused flag
  , _score  :: Int          -- ^ score
  , _win :: Bool
  , _blocks :: [Char]
  , _cells  :: [[Char]]
  , _input  :: Bool
  , _countdown :: Int
  , _focusRing :: F.FocusRing Name
  , _pos_x1 :: E.Editor String Name
  , _pos_y1 :: E.Editor String Name
  , _pos_x2 :: E.Editor String Name
  , _pos_y2 :: E.Editor String Name
  , _lastReportedClick :: Maybe (Name, T.Location)
  , _linkable :: Bool
  , _info :: [Char]
  }

type Coord = V2 Int

data Stream a = a :| Stream a
  deriving (Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 10
width = 10
count = 10
linkscore = 10

-- Functions
-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use paused, use dead]
  MaybeT . fmap Just $ countdown .= ((s ^. countdown) - 1)
  die


die :: MaybeT (State Game) ()
die = do
  MaybeT . fmap guard $ (\x -> x == 0) <$> (use countdown)
  MaybeT . fmap Just $ dead .= True


replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls


link :: (Int, Int) -> (Int, Int) -> Game -> Game
link (x1, y1) (x2, y2) g@Game {_cells = cells_old, _blocks = blocks_old, _score = s}
  | isLinkable cells_old x1 y1 x2 y2 = do
    let blocks_new_1 = blocks_old & element (x1 * width + y1) .~ ' '
    let blocks_new_2 = blocks_new_1 & element (x2 * width + y2) .~ ' '
    let from_list = iterate (width+) 0
    let rb_list = map (dropFrom blocks_new_2) from_list
    let cells_new_2 = take height (map (take width) rb_list)
    let s_new = s + linkscore
    g & cells .~ cells_new_2
      & score .~ s_new
      & blocks .~ blocks_new_2
      & countdown .~ count
      & win .~ (s_new == ((length (g ^. blocks)) `div` 2) * linkscore)
  | otherwise = g

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

shuffleGame :: Game -> IO Game
shuffleGame g@Game {_blocks = blocks_old} = do
  b <- shuffle blocks_old
  let from_list = iterate (width+) 0
  let rb_list = map (dropFrom b) from_list
  let cells_new = take height (map (take width) rb_list)
  return (g & cells .~ cells_new
            & blocks .~ b)

dropFrom :: [Char] -> Int -> [Char]
dropFrom l f = drop f l


-- congratulations :: [[Char]]
-- congratulations = [
--   ['\\', '/','\\','/',' ','|', ' ', '|','\\','|'],
--   ['\\', '/','\\','/',' ','|', ' ', '|','\\','|'],
--   ['\\', '/','\\','/',' ','|', ' ', '|','\\','|'],
--   ['\\', '/','\\','/',' ','|', ' ', '|','\\','|'],
--   ['\\', '/','\\','/',' ','|', ' ', '|','\\','|'],
--   ['\\', '/','\\','/',' ','|', ' ', '|','\\','|'],
--   ['\\', '/','\\','/',' ','|', ' ', '|','\\','|'],
--   ['\\', '/','\\','/',' ','|', ' ', '|','\\','|'],
--   ['\\', '/','\\','/',' ','|', ' ', '|','\\','|'],
--   ['\\', '/','\\','/',' ','|', ' ', '|','\\','|']
-- ]

congratulations :: [[Char]]
congratulations = [
  ['Y', 'O', 'U', ' ', ' ', ' ', 'W', 'I', 'N', '!'],
  ['Y', 'O', 'U', ' ', ' ', ' ', 'W', 'I', 'N', '!'],
  ['Y', 'O', 'U', ' ', ' ', ' ', 'W', 'I', 'N', '!'],
  ['Y', 'O', 'U', ' ', ' ', ' ', 'W', 'I', 'N', '!'],
  ['Y', 'O', 'U', ' ', ' ', ' ', 'W', 'I', 'N', '!'],
  ['Y', 'O', 'U', ' ', ' ', ' ', 'W', 'I', 'N', '!'],
  ['Y', 'O', 'U', ' ', ' ', ' ', 'W', 'I', 'N', '!'],
  ['Y', 'O', 'U', ' ', ' ', ' ', 'W', 'I', 'N', '!'],
  ['Y', 'O', 'U', ' ', ' ', ' ', 'W', 'I', 'N', '!'],
  ['Y', 'O', 'U', ' ', ' ', ' ', 'W', 'I', 'N', '!']]

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  (f :| fs) <- fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  rb <- randomRs ('A', 'Z') <$> newStdGen
  let b = take (quot (width * height) 2) rb
  blocks <- shuffle $ b ++ b
  let from_list = iterate (width+) 0
  let rb_list = map (dropFrom blocks) from_list
  let cells = take height (map (take width) rb_list)
  -- blocks <- shuffle b
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _score  = 0
        , _dead   = False
        , _paused = False
        , _win = False
        , _blocks = blocks
        , _cells = cells
        , _countdown = count
        , _focusRing = F.focusRing [PosX1, PosY1, PosX2, PosY2]
        , _pos_x1 = E.editor PosX1 (Just 2) ""
        , _pos_y1 = E.editor PosY1 (Just 2) ""
        , _pos_x2 = E.editor PosX2 (Just 2) ""
        , _pos_y2 = E.editor PosY2 (Just 2) ""
        , _lastReportedClick = Nothing
        , _linkable = False
        , _info = "no"
        }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")

-- | Check if two points are linkable

isEmptyRow :: [Char] -> Int -> Int -> Bool
isEmptyRow nums l h =
  if l > h then True -- base
  else if nums !! l /= ' ' then False -- base
  else isEmptyRow nums (l + 1) h -- n-1 -> n

isEmptyCol :: [[Char]] -> Int -> Int -> Int -> Bool
isEmptyCol g col l h =
  if l > h then True -- base
  else if g !! l !! col /= ' ' then False -- base
  else isEmptyCol g col (l + 1) h -- n-1 -> n

-- >>> isLinkable0 [['A','A','B','F'],['B','C','C','F'],['D',' ',' ','D']] 2 0 2 3
-- True
--
isLinkable0 :: [[Char]] -> Int -> Int -> Int -> Int -> Bool
isLinkable0 g row1 col1 row2 col2 =
  if row1 == row2 && col1 == col2 then False -- filters on same non-space character placed outside
  else if row1 /= row2 && col1 /= col2 then False
  else if row1 == row2 then isEmptyRow (g !! row1) ((min col1 col2) + 1) ((max col1 col2) - 1)
  else isEmptyCol g col1 ((min row1 row2) + 1) ((max row1 row2) - 1)


-- >>> isLinkable1 [['A','A','B','F'],['B','C','C','D'],['D',' ',' ',' ']] 2 0 1 2
-- True
--
isLinkable1 :: [[Char]] -> Int -> Int -> Int -> Int -> Bool
isLinkable1 g row1 col1 row2 col2 =
  if row1 == row2 && col1 == col2 then False -- filters on same non-space character placed outside
  else if row1 == row2 || col1 == col2 then False
  else if g !! row1 !! col2 == ' ' && (isLinkable0 g row1 col1 row1 col2) && (isLinkable0 g row1 col2 row2 col2) then True
  else if g !! row2 !! col1 == ' ' && (isLinkable0 g row1 col1 row2 col1) && (isLinkable0 g row2 col1 row2 col2) then True
  else False


-- >>> isLinkable2 [['D',' ',' ',' '],['B','C','C','D'],['D',' ',' ',' ']] 0 0 2 0
-- False
--
-- >>> isLinkable2 [['D',' ',' ',' '],['B','C','C',' '],['D',' ',' ',' ']] 0 0 2 0
-- True
--
hasLinkable1Right :: [[Char]] -> Int -> Int -> Int -> Int -> Int -> Bool
hasLinkable1Right g row1 row2 col2 l h =
  if l > h then False -- base
  else if (g !! row1 !! l /= ' ') then False -- base
  else if (g !! row1 !! l == ' ') && isLinkable1 g row1 l row2 col2 then True -- base
  else hasLinkable1Right g row1 row2 col2 (l + 1) h -- n-1 -> n

hasLinkable1Left :: [[Char]] -> Int -> Int -> Int -> Int -> Int -> Bool
hasLinkable1Left g row1 row2 col2 l h =
  if l > h then False -- base
  else if (g !! row1 !! h /= ' ') then False -- base
  else if (g !! row1 !! h == ' ') && isLinkable1 g row1 h row2 col2 then True -- base
  else hasLinkable1Left g row1 row2 col2 l (h - 1) -- n-1 -> n

hasLinkable1Down :: [[Char]] -> Int -> Int -> Int -> Int -> Int -> Bool
hasLinkable1Down g col1 row2 col2 l h =
  if l > h then False -- base
  else if (g !! l !! col1 /= ' ') then False -- base
  else if (g !! l !! col1 == ' ') && isLinkable1 g l col1 row2 col2 then True -- base
  else hasLinkable1Down g col1 row2 col2 (l + 1) h -- n-1 -> n

hasLinkable1Up :: [[Char]] -> Int -> Int -> Int -> Int -> Int -> Bool
hasLinkable1Up g col1 row2 col2 l h =
  if l > h then False -- base
  else if (g !! h !! col1 /= ' ') then False -- base
  else if (g !! h !! col1 == ' ') && isLinkable1 g h col1 row2 col2 then True -- base
  else hasLinkable1Up g col1 row2 col2 l (h - 1) -- n-1 -> n

isLinkable2 :: [[Char]] -> Int -> Int -> Int -> Int -> Bool
isLinkable2 g row1 col1 row2 col2 =
  if row1 == row2 && col1 == col2 then False -- filters on same non-space character placed outside
  else hasLinkable1Right g row1 row2 col2 (col1 + 1) (length (g !! row1) - 1) ||
    hasLinkable1Left g row1 row2 col2 0 (col1 - 1) ||
    hasLinkable1Down g col1 row2 col2 (row1 + 1) (length g - 1) ||
    hasLinkable1Up g col1 row2 col2 0 (row1 - 1)


-- >>> isLinkable [[' ',' ',' ','D'],[' ','C','C','E'],[' ',' ',' ','D']] 0 3 2 3
-- True
--
-- >>> isLinkable [[' ',' ',' ','D'],['B','C','C','E'],[' ',' ',' ','D']] 0 3 2 3
-- False

-- >>> isLinkable [['D',' ',' ',' '],['B','C','C','D'],['D',' ',' ',' ']] 0 0 2 0
-- False
--
-- >>> isLinkable [['D',' ',' ',' '],['B','C','C',' '],['D',' ',' ',' ']] 0 0 2 0
-- True
--
isLinkable :: [[Char]] -> Int -> Int -> Int -> Int -> Bool
isLinkable gg rrow1 ccol1 rrow2 ccol2 =
  if row1 == row2 && col1 == col2 || ((g !! row1 !! col1) /= (g !! row2 !! col2)) || (g !! row1 !! col1 == ' ') then False
  else (isLinkable0 g row1 col1 row2 col2) || (isLinkable1 g row1 col1 row2 col2) || (isLinkable2 g row1 col1 row2 col2)
    where g = padding gg
          row1 = rrow1+1
          col1 = ccol1+1
          row2 = rrow2+1
          col2 = ccol2+1

isLinkableStub :: [[Char]] -> Int -> Int -> Int -> Int -> Bool
isLinkableStub g row1 col1 row2 col2 = True

padding :: [[Char]] -> [[Char]]
padding g@(cs:css) = (zeros : (map padSides g)) ++ [zeros]
  where zeros = buildZeros n
        n     = (length cs) + 2

padSides :: [Char] -> [Char]
padSides cs = (' ' : cs) ++ " "

buildZeros :: Int -> [Char]
buildZeros n = Prelude.replicate n ' '

-- >>> padding [['A', 'B', 'C'], ['A', 'B', 'C'], ['A', 'B', 'C']]