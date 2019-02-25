module Run where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.Except
import Control.Lens
import qualified Graphics.UI.SDL as SDL

import CPU
import CPUState
import PPU
import RAM
import ROM
import Render

loopCPU :: (MonadIO m, MonadCPU m) => m ()
loopCPU = forever $ do
  step
  st <- get
  liftIO $ drawScreen (vramToScreen (st ^. vram))

printState :: (MonadIO m, MonadCPU m) => m ()
printState = do
  st <- get
  liftIO $ print (st ^. registers)
  liftIO $ void $ putStrLn $ "pc: " <> show (st ^. registers.pc)
  liftIO . print =<< lookupInst

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
  :: CPUState -> StateT CPUState (ExceptT CPUError IO) a
  -> IO (Either CPUError (a, CPUState))
runMonadCPU s = runExceptT . flip runStateT s
