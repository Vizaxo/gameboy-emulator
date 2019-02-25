module Screen where

import Data.Array

import Utils

data Pixel = P0 | P1 | P2 | P3

data Screen = Screen (Array (Int, Int) Pixel)

type ScreenWidth = 160
type ScreenHeight = 144
screenWidth, screenHeight :: Num a => a
screenWidth = natValue @ScreenWidth
screenHeight = natValue @ScreenHeight

mapScreenM_ :: Monad m => ((Int, Int) -> Pixel -> m ()) -> Screen -> m ()
mapScreenM_ f (Screen arr) = mapM_ (uncurry f) (assocs arr)

blankScreen :: Screen
blankScreen = mkScreen (replicate (screenWidth * screenHeight) P0)

mkScreen :: [Pixel] -> Screen
mkScreen ps = Screen $ array ((0,0), (screenWidth-1, screenHeight-1)) $
  zip (range ((0,0), (screenWidth-1, screenHeight-1))) ps

mkPixel :: (Bool, Bool) -> Pixel
mkPixel (False, False) = P0
mkPixel (False, True) = P1
mkPixel (True, False) = P2
mkPixel (True, True) = P3
