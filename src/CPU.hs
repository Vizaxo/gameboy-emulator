-- Emulation of the NES' 6502-derived CPU
module CPU where

import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Data.Finite
import Data.Map (Map, fromList)
import qualified Data.Map as M
import Data.Word
import Numeric.Natural

import CPUState
import Utils
import ROM

-- | Errors the CPU can throw
data CPUError
  = CPUEInstNotImplemented OpCode AddrMode [Word8]
  | CPUEInstLookupFailed Word8
  | CPUEInstFetchFailed Word16
  deriving Show

-- | The monad class for CPU operations
type MonadCPU m = (MonadState CPUState m, MonadError CPUError m)

-- | 6502 instruction opcodes
data OpCode
  = NOP
  deriving Show

-- | Addressing modes for 6502 instructions
data AddrMode
  = Implied | Absolute | IndexedAbs | ZeroPage
  | Relative | Accumulator
  | IndirX | IndirY | Immediate
  deriving Show

-- | Length (in bytes) of an instruction, including operands
data InstLen = L1 | L2
  deriving Show

-- | Increase a number (probably the program counter) by the given
-- instruction length
plusInstLen :: Integral n => InstLen -> n -> n
plusInstLen L1 = (+1)
plusInstLen L2 = (+2)

-- | The time (in cycles) an operation takes to complete
data OpTime = OpTime
  { _optime :: Natural
  }
  deriving Show

-- TODO: implement page boundry cross detection
-- | Increase a number (the clock time) by the given time
plusOpTime :: Integral n => OpTime -> n -> n
plusOpTime (OpTime t) pc = pc + fromIntegral t

-- | A complete specification of a single 6502 instruction
data Inst = Inst OpCode AddrMode InstLen OpTime

-- | A mapping from opcodes to instructions
instructions :: Map Word8 Inst
instructions = fromList
  [ (0x00, Inst NOP Immediate L1 (OpTime 4))
  ]

-- | Perform a single step of CPU execution
step :: MonadCPU m => m ()
step = do
  st <- get
  let (Register16 pc_) = st ^. registers . pc

  instByte <- throwWhenNothing (CPUEInstFetchFailed pc_)
    $ st ^? rom . ix (finite @ROMSize (fromIntegral pc_))
  inst <- throwWhenNothing (CPUEInstLookupFailed instByte)
    $ M.lookup instByte instructions
  exec pc_ inst

-- | Traversal of a given number of bytes starting from an offset
ixBytes :: forall m. (Ixed m, Num (Index m))
  => Word16 -> InstLen -> Traversal' m (IxValue m)
ixBytes pc_ L1 = ignored
ixBytes pc_ L2 = ix (fromIntegral pc_ + 1)

-- | Execute a CPU instruction
exec :: MonadCPU m => Word16 -> Inst -> m ()
exec pc_ (Inst opcode addrmode len time) = do
  modify (registers.pc %~ (plusInstLen len))
  args <- get <&> (^.. rom . ixBytes pc_ len)
  execInst opcode addrmode args
  modify (clocktime %~ (plusOpTime time))

-- | Dispatch execution of each CPU instruction
execInst :: MonadCPU m => OpCode -> AddrMode -> [Word8] -> m ()
execInst NOP Immediate [] = nop
execInst op addr args = throwError (CPUEInstNotImplemented op addr args)

nop :: MonadCPU m => m ()
nop = pure ()

-- | Execute ADC with immediate address
adcImm :: MonadCPU m => Word8 -> m ()
adcImm imm = modify (registers.a %~ (+Register imm))
