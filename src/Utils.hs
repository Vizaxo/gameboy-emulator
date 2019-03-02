{-# LANGUAGE AllowAmbiguousTypes #-}
module Utils where

import Control.Monad
import Control.Monad.Except
import Data.Proxy
import qualified Data.Text as T
import GHC.TypeLits

maybeMPlus :: MonadPlus m => Maybe a -> m a
maybeMPlus = maybe mzero pure

natValue :: forall n a. (KnownNat n, Num a) => a
natValue = fromInteger (natVal @n Proxy)

throwWhenNothing :: MonadError e m => e -> Maybe a -> m a
throwWhenNothing e Nothing = throwError e
throwWhenNothing e (Just x) = pure x

-- | Like @span@, but from the end of the list
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p xs = case span p (reverse xs) of
  (a, b) -> (reverse b, reverse a)

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.).(.)

showT :: Show a => a -> T.Text
showT = T.pack . show

chunks :: Int -> [a] -> [[a]]
chunks i [] = []
chunks i xs | length xs < i = [xs]
chunks i xs = take i xs : (chunks i (drop i xs))

padList :: Int -> a -> [a] -> [a]
padList l x (take l -> xs) = xs <> replicate (l - length xs) x
