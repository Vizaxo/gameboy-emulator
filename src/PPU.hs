module PPU where

import Data.Bits
import Data.Word
import qualified Data.Vector.Sized as VS

import RAM
import Screen


wordToPixs :: Word8 -> [Pixel]
wordToPixs byte = f <$> [0,2,4,6] where
  f n = mkPixel (testBit byte n, testBit byte (n+1))

-- This is not correct, but might be able to show the presence of the
-- VRAM changing.
vramToScreen :: RAM 0x2000 -> Screen
vramToScreen (RAM v) = mkScreen $ concatMap wordToPixs (VS.toList v)
