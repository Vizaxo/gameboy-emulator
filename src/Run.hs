module Run where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.Except
import Control.Lens

import CPU
import CPUState
import ROM

runCPU :: (MonadIO m, MonadCPU m) => m ()
runCPU = forever $ printState >> step

printState :: (MonadIO m, MonadCPU m) => m ()
printState = do
  st <- get
  liftIO $ print (st ^. registers)
  liftIO $ void $ putStrLn $ "pc: " <> show (st ^. registers.pc)
  liftIO . print =<< lookupInst

run :: MonadIO m => FilePath -> m ()
run path =
  runMaybeT (readRomFile path) >>= \case
    Nothing -> liftIO $ putStrLn "rom loading failed"
    Just rom -> do
      res <- runExceptT $ flip runStateT (initCPUState rom) $ (runCPU >> printState)
      liftIO $ print res
