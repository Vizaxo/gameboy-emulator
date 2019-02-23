module CPUState where

import Control.Lens
import Data.Finite
import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as VS
import Data.Word
import GHC.TypeLits
import Numeric.Natural

import ROM

newtype Register = Register Word8
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

newtype Register16 = Register16 Word16
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

data Registers = Registers
  { _a :: Register
  , _x :: Register
  , _y :: Register
  , _s :: Register
  , _pc :: Register16
  }
  deriving Show
makeLenses ''Registers

initRegisters :: Registers
initRegisters = Registers 0 0 0 0 0

data RAM (size :: Nat) = RAM { unRAM :: (Vector size Word8) }
  deriving Show

type instance IxValue (RAM size) = Word8
type instance Index (RAM size) = Finite size
instance Ixed (RAM size) where
  ix e f s = RAM <$> VS.ix e f (unRAM s)

initRAM :: KnownNat n => RAM n
initRAM = RAM (VS.replicate 0)

data CPUState = CPUState
  { _registers :: Registers
  , _clocktime :: Natural
  , _ram :: RAM 2048
  , _rom :: ROM
  }
  deriving Show
makeLenses ''CPUState

initCPUState :: ROM -> CPUState
initCPUState = CPUState initRegisters 0 initRAM
