module RAM (RAM(..), initRAM, VS.fromList, fromJust, indexRAM) where

import Control.Lens
import Data.Finite
import Data.Maybe
import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as VS
import Data.Word
import GHC.TypeLits

import Utils

-- | RAM is a fixed-size vector of bytes
data RAM (size :: Nat) = RAM { unRAM :: (Vector size Word8) }

-- Custom show instance to reduce large numbers of trailing zeroes for
-- uninitialized RAM.
instance KnownNat size => Show (RAM size) where
  show :: RAM size -> String
  show (RAM ram)
    = "RAM @" <> show (natValue @size) <> " (fromJust (fromList " <>
    case spanEnd (== 0) (VS.toList ram) of
      (content, []) -> show content
      (content, xs) -> "(" <> show content <> " <> replicate " <> show (length xs) <> " 0)"
    <> "))"

type instance IxValue (RAM size) = Word8
type instance Index (RAM size) = Finite size
instance Ixed (RAM size) where
  ix e f s = RAM <$> VS.ix e f (unRAM s)

-- | Initialise RAM with zeroes
initRAM :: KnownNat n => RAM n
initRAM = RAM (VS.replicate 0)

indexRAM (RAM r) = VS.index r
