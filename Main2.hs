module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import qualified Data.Vector as V
import Data.Monoid
import LifeIO

lifeW = 100
lifeH = 100
lifeCellSize = 10
screenW = 640
screenH = 480
shiftW = -(f lifeW * lifeCellSize)/2
shiftH = -(f lifeH * lifeCellSize)/2
f = fromIntegral

main = do
  (g1,g2) <- generateSmoothGrids lifeW lifeH
  let mode = (InWindow "Normal Life" (screenW,screenH) (0,0))
  simulateIO mode white 10 (g1,g2) (showLife . fst) (\_ _ (g1,g2) -> executeTransition g1 g2 >> return (g2,g1))

showLife :: Grid -> IO Picture
showLife g = translate shiftW shiftH . mconcat <$> pic where
  s = lifeCellSize
  pic = gridFold g f [] 
  f i j x ps | x < 0.5 = ps
             | otherwise = p : ps where
                p = translate (s*fromIntegral i) (s*fromIntegral j) $
                       rectangleSolid s s 
