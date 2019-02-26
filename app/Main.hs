module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Text (Text)
import qualified Graphics.UI.SDL as SDL
import System.Environment

import CPU
import CPUState
import Logger
import PPU
import ROM
import Render

loopCPU :: (MonadIO m, MonadCPU m) => m ()
loopCPU = forever $ do
  step
  st <- get
  when ((st ^. clocktime) - (st ^. lastDrawTime) >= 20000) $
    case vramToScreen (st ^. vram) of
      Nothing -> liftIO $ putStrLn "ppu error"
      Just s -> do
        liftIO $ drawScreen s
        liftIO $ putStrLn "Drawing"
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

run :: MonadIO m => FilePath -> m ()
run path = do
  liftIO initDisplay
  runMaybeT (readRomFile path) >>= \case
    Nothing -> liftIO $ putStrLn "rom loading failed"
    Just rom -> do
      res <- liftIO $ runMonadCPU (initCPUState rom) loopCPU
      liftIO $ SDL.quit
      liftIO $ print res

runMonadCPU
  :: CPUState -> WriterT [(LogLevel, Text)] (StateT CPUState (ExceptT CPUError IO)) a
  -> IO (Either CPUError (a, CPUState))
runMonadCPU s = runExceptT . flip runStateT s . fmap fst . runWriterT
