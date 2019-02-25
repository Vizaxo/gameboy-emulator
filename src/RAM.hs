module RAM (RAM(..), initRAM, VS.fromList, fromJust, indexRAM, (!)) where

import Control.Lens
import Data.Finite
import Data.List
import Data.Maybe
import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as VS
import Data.Word
import GHC.TypeLits
import Text.Printf

import Utils

-- | RAM is a fixed-size vector of bytes
newtype RAM (size :: Nat) = RAM { unRAM :: (Vector size Word8) }

-- Custom show instance to reduce large numbers of trailing zeroes for
-- uninitialized RAM.
instance KnownNat size => Show (RAM size) where
  show :: RAM size -> String
  show (RAM ram)
    = case spanEnd (== 0) (VS.toList ram) of
      (content, xs) -> hexDump content <>
        if length xs > 0
          then "\n... followed by " <> show (length xs) <> "x 00"
          else ""

hexDump :: [Word8] -> String
hexDump bs =
  let bytes = fmap (printf @(Word8 -> String) "%02hhx") $ bs
      cs = chunks 16 bytes
      byteLines = concat . intersperse " " <$> cs
      outputLines = zipWith (<>) (printf "%08x:  " <$> [0x0 :: Int,0x10..]) byteLines
  in concat $ intersperse "\n" outputLines

type instance IxValue (RAM size) = Word8
type instance Index (RAM size) = Finite size
instance Ixed (RAM size) where
  ix e f s = RAM <$> VS.ix e f (unRAM s)

-- | Initialise RAM with zeroes
initRAM :: KnownNat n => RAM n
initRAM = RAM (VS.replicate 0)

indexRAM (RAM r) = VS.index r

(!) :: forall size n. (KnownNat size, Integral n, Ord n) => RAM size -> n -> Maybe Word8
(RAM vs) ! i | i < natValue @size = Just (VS.index vs (finite (fromIntegral i)))
             | otherwise = Nothing
