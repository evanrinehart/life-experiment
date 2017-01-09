{-# LANGUAGE ScopedTypeVariables #-}
module LifeIO where

import Prelude hiding (read)
import System.Random
import Control.Monad
import qualified Data.Vector.Mutable as P
import qualified Data.Vector.Unboxed.Mutable as UV

data Grid = Grid
  { width :: Int
  , height :: Int
  , cells :: UV.IOVector Float }

read :: Int -> Int -> Grid -> IO Float
read i j (Grid w h v) = do
  let i' = i `mod` w
  let j' = j `mod` h
  let k = j'*h + i'
  UV.unsafeRead v k

write :: Int -> Int -> Float -> Grid -> IO ()
write i j x (Grid w h v) = do
  let i' = i `mod` w
  let j' = j `mod` h
  let k = j'*h + i'
  UV.unsafeWrite v k x

countLiveAround :: Grid -> Int -> Int -> IO Float
countLiveAround g i j = do
  x0 <- read (i-1) (j-1) g
  x1 <- read (i) (j-1) g
  x2 <- read (i+1) (j-1) g
  x3 <- read (i-1) j g
  x5 <- read (i+1) j g
  x6 <- read (i-1) (j+1) g
  x7 <- read (i) (j+1) g
  x8 <- read (i+1) (j+1) g
  return (x0+x1+x2+x3+x5+x6+x7+x8)

rule :: Float -> Float -> Float
rule here live =
  if here > 0.5
    then if live < 2 || live > 3 then 0 else 1
    else if live == 3 then 1 else 0

executeTransition :: Grid -> Grid -> IO ()
executeTransition g0@(Grid w h v0) g1 = do
  forM_ [0..w-1] $ \i -> do
    forM_ [0..h-1] $ \j -> do
      live <- countLiveAround g0 i j
      here <- read i j g0
      let here' = rule here live
      write i j here' g1

clamp :: Float -> Float
clamp x | x > 0.5 = 1
        | otherwise = 0

generateSmoothGrids :: Int -> Int -> IO (Grid,Grid)
generateSmoothGrids w h = do
  g1 <- Grid w h <$> UV.replicateM (w*h) (fmap clamp randomIO)
  g2 <- Grid w h <$> UV.new (w*h)
  return (g1, g2)

gridFold :: forall b . Grid -> (Int -> Int -> Float -> b -> b) -> b -> IO b
gridFold (Grid w h v) f a0 = g 0 0 a0 where
  g :: Int -> Int -> b -> IO b
  g i j a = do
    x <- UV.unsafeRead v (j*h + i)
    let a' = f i j x a
    let (c,i') = (i+1) `divMod` w
    let (done,j') = (j+c) `divMod` h
    if done > 0 then return a' else g i' j' a'
