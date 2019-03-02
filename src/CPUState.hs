module CPUState where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.State
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
  { _registers :: !Registers
  , _clocktime :: !Natural
  , _rom       :: !(RAM 0x8000)
  , _ram       :: !(RAM 0x2000)
  , _vram      :: !(RAM 0x2000)
  , _cram      :: !(RAM 0x2000)
  , _oam       :: !(RAM 0x100)
  , _ioreg     :: !(RAM 0x80)
  , _zeropg    :: !(RAM 0x7F)
  , _ief       :: !Word8
  , _lastDrawTime :: !Natural
  , _lastDrawTimeMillis :: Integer
  , _mie        :: !Bool
  , _stopped   :: !Bool
  }
makeLenses ''CPUState

readMem' :: (MonadState CPUState m, MonadIO m) => Word16 -> m Word8
readMem' addr = f =<< get
  where
    f CPUState{..}
      | 0x0000 `to` 0x7FFF = _rom    ! (addr' - 0x0000)
      | 0x8000 `to` 0x9FFF = _vram   ! (addr' - 0x8000)
      | 0xA000 `to` 0xBFFF = _cram   ! (addr' - 0xA000)
      | 0xC000 `to` 0xDFFF = _ram    ! (addr' - 0xC000)
      | 0xE000 `to` 0xFDFF = _ram    ! ((addr' - 0xE000) `mod` 0x2000)
      | 0xFE00 `to` 0xFE9F = _oam    ! ((addr' - 0xFE00) `mod` 0x0100)
      | 0xFEA0 `to` 0xFEFF = pure 0xFF
      | 0xFF00 `to` 0xFF7F = _ioreg  ! ((addr' - 0xFF00) `mod` 0x80)
      | 0xFF80 `to` 0xFFFE = _zeropg ! ((addr' - 0xFF80) `mod` 0x7F)
      | 0xFFFF `to` 0xFFFF = pure _ief
      | otherwise = error $ "Ram index out of range (should never occur): " <> show addr
    l `to` u = l <= addr && addr <= u
    addr' = fromIntegral addr

writeMem :: (MonadState CPUState m, MonadIO m) => Word16 -> Word8 -> m ()
writeMem addr v = f =<< get
  where
    f CPUState{..}
      | 0x0000 `to` 0x7FFF = pure ()
      | 0x8000 `to` 0x9FFF = writeRam _vram   (addr' - 0x8000) v
      | 0xA000 `to` 0xBFFF = writeRam _cram   (addr' - 0xA000) v
      | 0xC000 `to` 0xDFFF = writeRam _ram    (addr' - 0xC000) v
      | 0xE000 `to` 0xFDFF = writeRam _ram    ((addr' - 0xE000) `mod` 0x2000) v
      | 0xFE00 `to` 0xFE9F = writeRam _oam    ((addr' - 0xFE00) `mod` 0x0100) v
      | 0xFEA0 `to` 0xFEFF = pure ()
      | 0xFF00 `to` 0xFF7F = writeRam _ioreg  ((addr' - 0xFF00) `mod` 0x80) v
      | 0xFF80 `to` 0xFFFE = writeRam _zeropg ((addr' - 0xFF80) `mod` 0x7F) v
      | 0xFFFF `to` 0xFFFF = modify (set ief v)
      | otherwise = error $ "Ram index out of range (should never occur): " <> show addr
    l `to` u = l <= addr && addr <= u
    addr' = fromIntegral addr

-- | Initialise the CPU state with the given ROM
initCPUState :: MonadIO m => ROM -> m CPUState
initCPUState rom = CPUState
  <$> pure initRegisters
  <*> pure 0
  <*> pure rom
  <*> initRAM
  <*> initRAM
  <*> initRAM
  <*> initRAM
  <*> initRAM
  <*> initRAM
  <*> pure 0
  <*> pure 0
  <*> pure 0
  <*> pure False
  <*> pure False

-- -- | Temporarily set LY register to make game think it's in VBLANK
-- initIoReg :: RAM 0x80
-- initIoReg = RAM $ fromJust $ VS.fromList $ padList 0x80 0 ((replicate 0x44 0) <> [148])

data Flag = FlagZ | FlagN | FlagH | FlagC
  deriving Show
