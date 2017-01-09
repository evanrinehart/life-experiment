module Life where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Control.Comonad
import System.Random

data Cell = On | Off deriving (Show)

data Grid a = Grid
  { width :: Int
  , height :: Int
  , cells :: Vector (Vector a) } deriving (Show)

data LifeCursor a = LifeCursor
  { lci :: Int
  , lcj :: Int
  , getGrid :: Grid a } deriving (Show)

instance Functor LifeCursor where
  fmap f (LifeCursor i j (Grid w h vv)) = LifeCursor i j (Grid w h (fmap (fmap f) vv))

instance Comonad LifeCursor where
  extract (LifeCursor i j (Grid w h vv)) = ix i j vv
  duplicate (LifeCursor i j g@(Grid w h vv)) = LifeCursor i j (range w h g)

ix :: Int -> Int -> (Vector (Vector a)) -> a
ix i j vv = vv ! i ! j

at :: Int -> Int -> LifeCursor a -> a
at dx dy (LifeCursor i j (Grid w h vv)) = ix i' j' vv where
  i' = (i + dx) `mod` w
  j' = (j + dy) `mod` h

range :: Int -> Int -> Grid a -> Grid (LifeCursor a)
range w h grid = Grid w h (V.generate w f) where
  f i = V.generate h (g i)
  g i j = LifeCursor i j grid

neighborhood :: LifeCursor Cell -> [Cell]
neighborhood c = let f i j = at i j c in
  [f (-1) (-1), f 0 (-1), f 1 (-1)
  ,f (-1) 0,              f 1 0
  ,f (-1) 1,    f 0 1,    f 1 1   ]

countLive :: [Cell] -> Int
countLive [] = 0
countLive (On:xs) = 1 + countLive xs
countLive (Off:xs) = countLive xs

lifeRule :: LifeCursor Cell -> Cell
lifeRule c = cell where
  live = countLive (neighborhood c)
  cell = case extract c of
    On  | live < 2 || live > 3 -> Off
        | otherwise -> On
    Off | live == 3 || live == 3 -> On
        | otherwise -> Off


generateBoard :: Int -> Int -> IO (LifeCursor Cell)
generateBoard w h = do
  vv <- V.replicateM w $ do
    V.replicateM h $ do
      heads <- randomIO
      return (if heads then On else Off)
  return (LifeCursor 0 0 (Grid w h vv))

generateLife :: Int -> Int -> IO [Grid Cell]
generateLife w h = do
  lc0 <- generateBoard w h
  let lcs = iterate (extend lifeRule) lc0
  return (map getGrid lcs)
