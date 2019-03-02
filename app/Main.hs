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
import Data.Time.Clock.POSIX (getPOSIXTime)

import CPU
import CPUState
import Input
import Joypad
import Logger
import PPU
import ROM
import Render
import Interrupts

loopCPU :: (MonadIO m, MonadCPU m, MonadReader (IORef Joypad) m) => m ()
loopCPU = forever $ do
  (joypad, updated) <- getJoypad
  modify (over (memory 0xFF00) (updateJoypadIOReg joypad))
  when updated $ do
    modify (set stopped False)
    fireInterrupt JOYPAD

  st <- get
  unless (st^.stopped) $ do
    -- Temporary fixes while IO registers aren't properly implemented
    modify (over (memory 0xFF44) (+1))
    modify (set (memory 0xFF85) 1)
    step

  st <- get
  when ((st ^. clocktime) - (st ^. lastDrawTime) >= 17000) $ do
    calculateMhz
    case vramToScreen (st ^. vram) of
      Nothing -> liftIO $ putStrLn "ppu error"
      Just s -> do
        liftIO $ drawScreen s
        modify (set lastDrawTime (st ^. clocktime))
        unless (st^.stopped) (fireInterrupt VBLANK)

calculateMhz :: (MonadCPU m, MonadIO m) => m ()
calculateMhz = do
  time <- (round . (* 1000)) <$> liftIO getPOSIXTime
  oldLastTime <- view lastDrawTimeMillis <$> get
  let tdiff = time - oldLastTime
  let speed = 17000 / (fromIntegral tdiff) / 1000
  liftIO $ putStrLn $ "Speed: " <> show speed <> " MHz"
  modify (set lastDrawTimeMillis time)

printState :: (MonadIO m, MonadCPU m) => m ()
printState = do
  st <- get
  liftIO $ print (st ^. registers)
  liftIO $ void $ putStrLn $ "pc: " <> show (st ^. registers.pc)
  liftIO . print =<< lookupInst

main :: IO ()
main = do
  getArgs >>= \case
    ["--debug", filepath] -> debug filepath
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
