module Main where

import Graphics.Gloss
import qualified Data.Vector as V
import Data.Monoid
import Life

lifeW = 70
lifeH = 70
lifeCellSize = 10
screenW = 640
screenH = 480
shiftW = -(f lifeW * lifeCellSize)/2
shiftH = -(f lifeH * lifeCellSize)/2
f = fromIntegral

main = do
  lifeSteps <- generateLife lifeW lifeH
  let mode = (InWindow "Normal Life" (screenW,screenH) (0,0))
  simulate mode white 8 lifeSteps (showLife . head) (\_ _ -> tail)

showLife :: Grid Cell -> Picture
showLife (Grid _ _ vv) = translate shiftW shiftH pic where
  pic = mconcat . V.toList . V.concatMap f $ V.indexed vv
  s = lifeCellSize
  f (i,v) = V.imap (g i) v
  g i j Off = blank
  g i j On = translate (s*fromIntegral i) (s*fromIntegral j) $
    rectangleSolid s s <> color blue (rectangleWire s s)
