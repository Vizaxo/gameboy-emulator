module Input where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import qualified Graphics.UI.SDL as SDL
import Data.IORef

import Joypad

getJoypad :: (MonadReader (IORef Joypad) m, MonadIO m) => m Joypad
getJoypad = do
  stateRef <- ask
  state <- liftIO (readIORef stateRef)
  e <- liftIO SDL.pollEvent
  let newState = case e of
        SDL.KeyDown k -> Just $ set (getKey k) True state
        SDL.KeyUp k -> Just $ set (getKey k) False state
        _ -> Nothing
  case newState of
    Nothing -> pure state
    Just s -> liftIO (writeIORef stateRef s >> print s) >> pure s

getKey :: SDL.Keysym -> Traversal' Joypad Bool
getKey = f . SDL.symKey where
  f SDL.SDLK_RIGHT = right
  f SDL.SDLK_LEFT = left
  f SDL.SDLK_UP = up
  f SDL.SDLK_DOWN = down
  f SDL.SDLK_a = aBtn
  f SDL.SDLK_b = bBtn
  f SDL.SDLK_BACKSLASH = select
  f SDL.SDLK_RETURN = start
  f _ = ignored
