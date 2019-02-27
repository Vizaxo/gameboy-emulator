module Render where

import Control.Monad
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL as SDL hiding (Pixel)
import Data.Ix
import qualified Data.Vector as V

import Screen

pixelScale :: Num a => a
pixelScale = 4

initDisplay :: IO ()
initDisplay = do
  SDL.init [InitVideo]
  screen <- SDL.setVideoMode (screenWidth * pixelScale) (screenHeight * pixelScale) 16 [SWSurface]
  SDL.setCaption "Game Boy Emulator" ""

drawFrame :: (SDL.Surface -> IO ()) -> IO ()
drawFrame f = do
  screen <- getVideoSurface
  lockSurface screen
  f screen
  SDL.flip screen
  unlockSurface screen

pixelToColor :: Pixel -> SDL.PixelFormat -> IO SDL.Pixel
pixelToColor P0 fmt = mapRGB fmt 245 255 245
pixelToColor P1 fmt = mapRGB fmt 160 170 160
pixelToColor P2 fmt = mapRGB fmt 80 90 80
pixelToColor P3 fmt = mapRGB fmt 0 10 0

drawPixel :: SDL.Surface -> (Int, Int) -> Pixel -> IO ()
drawPixel surface (y, x) pix = do
  let format = surfaceGetPixelFormat surface
  p <- pixelToColor pix format
  void $ fillRect
    surface
    (Just (Rect (x * pixelScale) (y * pixelScale) pixelScale pixelScale))
    p

drawScreen :: Screen -> IO ()
drawScreen (Screen screen) = drawFrame $ \surface -> -- mapScreenM_ (drawPixel surface) screen
  Prelude.flip (mapM_ @[]) (range ((0,0),(screenHeight-1, screenWidth-1))) $ \(y,x) ->
    drawPixel surface (y,x) (screen V.! (y*screenWidth + x))

withSurface :: IO () -> IO ()
withSurface m = initDisplay >> m >> quit
