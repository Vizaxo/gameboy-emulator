module Logger where

import Prelude hiding (log)
import Control.Monad.Writer
import Data.Text

import Utils

data LogLevel = Critical | Info | Debug
  deriving Show

class MonadLogger m where
  log :: LogLevel -> Text -> m ()

instance Monad m => MonadLogger (WriterT [(LogLevel, Text)] m) where
  log l m = tell [(l, m)]

instance {-# OVERLAPPABLE #-} MonadLogger IO where
  log l m = putStrLn $ show l <> ": " <> unpack m

instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadLogger m, Monad m) => MonadLogger (t m) where
  log = lift .: log
