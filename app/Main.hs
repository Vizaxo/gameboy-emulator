module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Text (Text)
import qualified Graphics.UI.SDL as SDL
import System.Environment
import Data.IORef

import CPU
import CPUState
import Input
import Joypad
import Logger
import PPU
import ROM
import Render

loopCPU :: (MonadIO m, MonadCPU m, MonadReader (IORef Joypad) m) => m ()
loopCPU = forever $ do
  -- Temporary fixes while IO registers aren't properly implemented
  modify (over (memory 0xFF44) (+1))
  modify (set (memory 0xFF85) 1)

  joypad <- getJoypad
  modify (over (memory 0xFF00) (updateJoypadIOReg joypad))

  step

  st <- get
  when ((st ^. clocktime) - (st ^. lastDrawTime) >= 2000) $
    case vramToScreen (st ^. vram) of
      Nothing -> liftIO $ putStrLn "ppu error"
      Just s -> do
        liftIO $ drawScreen s
        modify (set lastDrawTime (st ^. clocktime))

printState :: (MonadIO m, MonadCPU m) => m ()
printState = do
  st <- get
  liftIO $ print (st ^. registers)
  liftIO $ void $ putStrLn $ "pc: " <> show (st ^. registers.pc)
  liftIO . print =<< lookupInst

main :: IO ()
main = do
  getArgs >>= \case
    [filepath] -> run filepath
    _ -> putStrLn "Usage: gb-emulator rom-path"

debug :: MonadIO m => FilePath -> m ()
debug path = do
  liftIO initDisplay
  runMaybeT (readRomFile path) >>= \case
    Nothing -> liftIO $ putStrLn "rom loading failed"
    Just rom -> do
      stateRef <- liftIO (newIORef defaultJoypad)
      res <- liftIO $ debugMonadCPU (initCPUState rom) (runReaderT loopCPU stateRef)
      liftIO $ SDL.quit
      liftIO $ print res

run :: MonadIO m => FilePath -> m ()
run path = do
  liftIO initDisplay
  runMaybeT (readRomFile path) >>= \case
    Nothing -> liftIO $ putStrLn "rom loading failed"
    Just rom -> do
      stateRef <- liftIO (newIORef defaultJoypad)
      res <- liftIO $ runMonadCPU (initCPUState rom) (runReaderT loopCPU stateRef)
      liftIO $ SDL.quit
      liftIO $ print res

runMonadCPU
  :: CPUState -> WriterT [(LogLevel, Text)] (StateT CPUState (ExceptT CPUError IO)) a
  -> IO (Either CPUError (a, CPUState))
runMonadCPU s = runExceptT . flip runStateT s . fmap fst . runWriterT

debugMonadCPU
  :: CPUState -> StateT CPUState (ExceptT CPUError IO) a
  -> IO (Either CPUError (a, CPUState))
debugMonadCPU s = runExceptT . flip runStateT s
