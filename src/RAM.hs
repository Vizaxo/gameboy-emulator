module RAM where

import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Proxy
import Data.Array.IO
import Data.Word
import GHC.TypeLits
import Text.Printf

import Utils

-- | RAM is a fixed-size vector of bytes
newtype RAM (size :: Nat) = RAM { unRAM :: (IOUArray Word16 Word8) }

-- Custom show instance to reduce large numbers of trailing zeroes for
-- uninitialized RAM.
instance KnownNat size => Show (RAM size) where
  show :: RAM size -> String
  show (RAM ram)
    = "RAM"
  {-
    = case spanEnd (== 0) (V.toList ram) of
      (content, xs) -> hexDump content <>
        if length xs > 0
          then "\n... followed by " <> show (length xs) <> "x 00"
          else ""
-}

hexDump :: [Word8] -> String
hexDump bs =
  let bytes = fmap (printf @(Word8 -> String) "%02hhx") $ bs
      cs = chunks 16 bytes
      byteLines = concat . intersperse " " <$> cs
      outputLines = zipWith (<>) (printf "%08x:  " <$> [0x0 :: Int,0x10..]) byteLines
  in concat $ intersperse "\n" outputLines

{-
type instance IxValue (RAM size) = Word8
type instance Index (RAM size) = Finite size
instance Ixed (RAM size) where
  ix e f s = RAM <$> ix e f (unRAM s)
-}

-- | Initialise RAM with zeroes
initRAM :: forall n m. (MonadIO m, KnownNat n) => m (RAM n)
initRAM = RAM <$> liftIO (newArray (0x0000, natValue @n) 0)

(!) :: forall size m. (KnownNat size, MonadIO m, MonadPlus m) => RAM size -> Word16 -> m Word8
(RAM arr) ! i | i < fromInteger (natVal @size Proxy) = liftIO (readArray arr i)
              | otherwise = mzero

writeRam :: MonadIO m => RAM size -> Word16 -> Word8 -> m ()
writeRam (RAM r) = liftIO .: writeArray r
