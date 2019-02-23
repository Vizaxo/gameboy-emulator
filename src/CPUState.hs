module CPUState where

import Control.Lens
import Data.Finite
import Data.Word
import Numeric.Natural

import RAM
import ROM

-- | 8-bit register
newtype Register = Register { unRegister :: Word8 }
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

-- | 16-bit register
newtype Register16 = Register16 { unRegister16 :: Word16 }
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

-- | The CPU's registers
data Registers = Registers
  { _a :: Register
  , _x :: Register
  , _y :: Register
  , _s :: Register
  , _pc :: Register16
  }
  deriving Show
makeLenses ''Registers

-- | Initialise registers to zero
initRegisters :: Registers
initRegisters = Registers 0 0 0 0 0x100

-- | The entire CPU state
data CPUState = CPUState
  { _registers :: Registers
  , _clocktime :: Natural
  , _ram       :: RAM 2048
  , _ioregA    :: RAM 8
  , _ioregB    :: RAM 32
  , _sram      :: RAM 8192
  , _rom       :: RAM 32768
  , _handlers  :: RAM 6
  }
  deriving Show
makeLenses ''CPUState

-- TODO: make ROM un-writable
-- | A traversal over the NES' memory map, indexed by the address
-- Memory map description at https://en.wikibooks.org/wiki/NES_Programming
memory :: Word16 -> Traversal' CPUState Word8
memory addr
  | 0x0000 `to` 0x1FFF = ram      . ix (modulo @2048  (addr' - 0x0000))
  | 0x2000 `to` 0x3FFF = ioregA   . ix (modulo @8     (addr' - 0x2000))
  | 0x4000 `to` 0x401F = ioregB   . ix (finite @32    (addr' - 0x4000))
  | 0x4020 `to` 0x5FFF = ignored -- Expansion ROM
  | 0x6000 `to` 0x7FFF = sram     . ix (finite @8192  (addr' - 0x6000))
  | 0x8000 `to` 0xFFFD = rom      . ix (finite @32768 (addr' - 0x8000))
  | 0xFFFA `to` 0xFFFF = handlers . ix (finite @6     (addr' - 0xFFFA))
  | otherwise = error "Ram index out of range (should never occur)"
  where
    l `to` u = l <= addr && addr <= u
    addr' = fromIntegral addr

-- | Initialise the CPU state with the given ROM
initCPUState :: ROM -> CPUState
initCPUState rom = CPUState initRegisters 0 initRAM initRAM initRAM initRAM rom initRAM
