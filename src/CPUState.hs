module CPUState where

import Control.Lens
import Data.Finite
import Data.Word
import Numeric.Natural

import Bits
import RAM
import ROM

-- | The CPU's registers
data Registers = Registers
  { _af :: Word16
  , _bc :: Word16
  , _de :: Word16
  , _hl :: Word16
  , _pc :: Word16
  , _sp :: Word16
  }
  deriving Show
makeLenses ''Registers

a, f, b, c, d, e, h, l :: Lens' Registers Word8
a = af.upper
f = af.lower
b = bc.upper
c = bc.lower
d = de.upper
e = de.lower
h = hl.upper
l = hl.lower

-- | Initialise registers to zero
initRegisters :: Registers
initRegisters = Registers 0 0 0 0 0x100 0

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
