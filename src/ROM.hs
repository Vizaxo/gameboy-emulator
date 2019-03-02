module ROM where

import Control.Monad
import Control.Monad.Trans
import Data.Array.MArray
import qualified Data.ByteString as BS

import RAM
import Utils

type ROMSize = 32768
type ROM = RAM ROMSize

-- | Read a ROM from a file
readRomFile :: (MonadIO m, MonadPlus m) => FilePath -> m ROM
readRomFile path = do
  bytes <- liftIO $ BS.readFile path
  --TODO: determine cartridge layout; what's ROM, what's RAM, etc.
  --maybeMPlus $ padBS (BS.take (natValue @ROMSize) bytes)
  RAM <$> (liftIO $ newListArray (0x0000, 0x7FFF) (BS.unpack $ BS.take (natValue @ROMSize) bytes))

{-
-- | Pad a bytestring to the given length, and convert it to RAM
padBS :: forall n m. (KnownNat n, MonadIO m, MonadPlus m) => ByteString -> m (RAM n)
padBS bs
  | BS.length bs <= natValue @n
    = newListArray (0x0000, 0x7FFF) 
  -- = RAM <$> (fromList $ BS.unpack (bs <> BS.replicate (natValue @n - BS.length bs) 0))
  | otherwise = Nothing
-}
  {-
-- | Make a ROM from an n-tuple of bytes
mkRom :: forall n input pad.
  (IndexedListLiterals input n Word8, KnownNat n, KnownNat pad)
  => input -> RAM (n + pad)
mkRom = RAM . pad 0 . VS.fromTuple
-}
