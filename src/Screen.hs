module Screen where

import Data.Vector as V

import Utils

data Pixel = P0 | P1 | P2 | P3
  deriving Show

data Screen = Screen (Vector Pixel)
  deriving Show

type ScreenWidth = 160
type ScreenHeight = 144
screenWidth, screenHeight :: Num a => a
screenWidth = natValue @ScreenWidth
screenHeight = natValue @ScreenHeight

mkScreen :: [Pixel] -> Screen
mkScreen = Screen . V.fromList

blankScreen :: Screen
blankScreen = Screen (V.replicate (screenWidth * screenHeight) P0)

mkPixel :: (Bool, Bool) -> Pixel
mkPixel (False, False) = P0
mkPixel (False, True) = P1
mkPixel (True, False) = P2
mkPixel (True, True) = P3
