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
initRegisters = Registers 0 0 0 0 0x100 0xFFFE

-- | The entire CPU state
data CPUState = CPUState
  { _registers :: Registers
  , _clocktime :: Natural
  , _rom       :: RAM 0x8000
  , _ram       :: RAM 0x2000
  , _vram      :: RAM 0x2000
  , _cram      :: RAM 0x2000
  , _oam       :: RAM 0x100
  , _ioreg     :: RAM 0x80
  , _zeropg    :: RAM 0x7F
  , _ief       :: RAM 0x01
  }
  deriving Show
makeLenses ''CPUState

-- TODO: make ROM un-writable
-- | A traversal over the NES' memory map, indexed by the address
-- Memory map description at https://en.wikibooks.org/wiki/NES_Programming
memory :: Word16 -> Traversal' CPUState Word8
memory addr
  | 0x0000 `to` 0x7FFF = rom    . ix (finite @0x8000 (addr' - 0x0000))
  | 0x8000 `to` 0x9FFF = vram   . ix (finite @0x2000 (addr' - 0x8000))
  | 0xA000 `to` 0xBFFF = cram   . ix (finite @0x2000 (addr' - 0xA000))
  | 0xC000 `to` 0xDFFF = ram    . ix (finite @0x2000 (addr' - 0xC000))
  | 0xE000 `to` 0xFDFF = ram    . ix (modulo @0x2000 (addr' - 0xE000))
  | 0xFE00 `to` 0xFE9F = oam    . ix (modulo @0x0100 (addr' - 0xFE00))
  | 0xFEA0 `to` 0xFEFF = ignored
  | 0xFF00 `to` 0xFF7F = ioreg  . ix (modulo @0x80   (addr' - 0xFF00))
  | 0xFF80 `to` 0xFFFE = zeropg . ix (modulo @0x7F   (addr' - 0xFF80))
  | 0xFFFF `to` 0xFFFF = ief    . ix (modulo @0x01   (addr' - 0xFFFF))
  | otherwise = error $ "Ram index out of range (should never occur): " <> show addr
  where
    l `to` u = l <= addr && addr <= u
    addr' = fromIntegral addr

-- | Initialise the CPU state with the given ROM
initCPUState :: ROM -> CPUState
initCPUState rom = CPUState
  { _registers = initRegisters
  , _clocktime = 0
  , _rom = rom
  , _ram = initRAM
  , _vram = initRAM
  , _cram = initRAM
  , _oam = initRAM
  , _ioreg = initRAM
  , _zeropg = initRAM
  , _ief = initRAM
  }
