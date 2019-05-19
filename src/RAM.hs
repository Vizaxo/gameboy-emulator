module RAM where

import Control.Monad.Trans
import Data.List
import Data.Array.IO
import Data.Word
import GHC.TypeLits
import Text.Printf

import Utils

-- | RAM is a fixed-size vector of bytes
newtype RAM (size :: Nat) = RAM { unRAM :: (IOUArray Word16 Word8) }

instance Show (RAM size) where
  show r = "RAM"

hexDump :: [Word8] -> String
hexDump bs =
  let bytes = fmap (printf @(Word8 -> String) "%02hhx") $ bs
      cs = chunks 16 bytes
      byteLines = concat . intersperse " " <$> cs
      outputLines = zipWith (<>) (printf "%08x:  " <$> [0x0 :: Int,0x10..]) byteLines
  in concat $ intersperse "\n" outputLines

-- | Initialise RAM with zeroes
initRAM :: forall n m. (MonadIO m, KnownNat n) => m (RAM n)
initRAM = RAM <$> liftIO (newArray (0x0000, natValue @n) 0)

{-# INLINE (!) #-}
(!) :: forall size m. MonadIO m => RAM size -> Word16 -> m Word8
(RAM arr) ! i = liftIO (readArray arr i)

{-# INLINE writeRam #-}
writeRam :: MonadIO m => RAM size -> Word16 -> Word8 -> m ()
writeRam (RAM r) = liftIO .: writeArray r
