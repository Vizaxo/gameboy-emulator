module Run where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.Except
import Control.Lens
import Data.Finite
import Numeric

import CPU
import CPUState
import ROM

runCPU :: (MonadIO m, MonadCPU m) => m ()
runCPU = forever $ printState >> step

printState :: (MonadIO m, MonadCPU m) => m ()
printState = do
  st <- get
  liftIO $ print (st ^. registers)
  let mbInst = st ^? rom . ix (finite (fromIntegral (st ^. registers.pc.reg16)))
  liftIO $ void $ putStr "[pc]: " >> mapM putStrLn (flip showHex "" <$> mbInst)

run :: MonadIO m => FilePath -> m ()
run path =
  runMaybeT (readRomFile path) >>= \case
    Nothing -> liftIO $ putStrLn "rom loading failed"
    Just rom -> do
      res <- runExceptT $ flip runStateT (initCPUState rom) $ (runCPU >> printState)
      liftIO $ print res
