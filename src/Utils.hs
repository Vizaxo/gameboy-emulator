{-# LANGUAGE AllowAmbiguousTypes #-}
module Utils where

import Control.Monad
import Control.Monad.Except
import Data.Proxy
import GHC.TypeLits

maybeMPlus :: MonadPlus m => Maybe a -> m a
maybeMPlus = maybe mzero pure

natValue :: forall n. KnownNat n => Int
natValue = fromInteger (natVal @n Proxy)

throwWhenNothing :: MonadError e m => e -> Maybe a -> m a
throwWhenNothing e Nothing = throwError e
throwWhenNothing e (Just x) = pure x

-- | Like @span@, but from the end of the list
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p xs = case span p (reverse xs) of
  (a, b) -> (reverse b, reverse a)
