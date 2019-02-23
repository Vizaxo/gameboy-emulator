-- Emulation of the NES' 6502-derived CPU
module CPU where

import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Data.Map (Map, fromList)
import qualified Data.Map as M
import Data.Word
import Numeric.Natural

import CPUState
import ROM

-- | Errors the CPU can throw
data CPUError
  = CPUECantGetOpArgs [Word8]
  | CPUEInstNotImplemented OpCode AddrMode [Word8]
  deriving Show

-- | The monad class for CPU operations
type MonadCPU m = (MonadState CPUState m, MonadError CPUError m)

-- | 6502 instruction opcodes
data OpCode
  = BRK | ORA | ASL | PHP | BPL | CLC | JSR | AND | BIT
  | ROL | PLP | BMI | SEC | RTI | EOR | LSR | PHA | JMP
  | BVC | CLI | RTS | ADC | ROR | PLA | BVS | SEI | STA
  | STY | STX | DEY | TXA | BCC | TYA | TXS | LDY | LDA
  | LDX | TAY | TAX | BCS | CLV | TSX | CPY | CMP | DEC
  | INY | DEX | BNE | CLD | CPX | SBC | INC | INX | NOP
  | BEQ | SED
  deriving Show

-- | Addressing modes for 6502 instructions
data AddrMode
  = Implied | Absolute | IndexedAbs | ZeroPage
  | Relative | Accumulator
  | IndirX | IndirY | Immediate
  deriving Show

-- | Length (in bytes) of instruction
data InstLen = L1 | L2
  deriving Show

-- | Increase a number (the program counter) by the given instruction length
plusInstLen :: Integral n => InstLen -> n -> n
plusInstLen L1 = (+1)
plusInstLen L2 = (+2)

data OpTime = OpTime
  { _optime :: Natural
  , _pbcrossed :: Bool
  }
  deriving Show

-- TODO: implement page boundry cross detection
-- | Increase a number (the clock time) by the given time
plusOpTime :: Integral n => OpTime -> n -> n
plusOpTime (OpTime t _) pc = pc + fromIntegral t


-- | A complete specification of a single 6502 instruction
data Inst = Inst OpCode AddrMode InstLen OpTime

-- | A mapping from opcodes to instructions
instructions :: Map Word8 Inst
instructions = fromList
  [ (0x29, Inst ADC Immediate L2 (OpTime 2 False))
  ]

-- | Perform a single step of CPU execution
step :: MonadCPU m => m ()
step = do
  st <- get
  let
    (Register16 pc_) = st ^. registers . pc
    instByte = st ^? rom . ix pc_
    inst = flip M.lookup instructions =<< instByte
  mapM_ (exec pc_) inst

-- | Traversal of a given number of bytes starting from an offset
ixBytes :: forall m. (Ixed m, Num (Index m)) => Word16 -> InstLen -> Traversal' m (IxValue m)
ixBytes pc_ L1 = ignored
ixBytes pc_ L2 = ix ((fromIntegral pc_ :: Index m) + 1)

-- | Execute a CPU instruction
exec :: MonadCPU m => Word16 -> Inst -> m ()
exec pc_ (Inst opcode addrmode len time) = do
  modify (clocktime %~ (plusOpTime time))
  modify (registers.pc %~ (plusInstLen len))
  args <- get <&> (^.. rom . ixBytes pc_ len)
  execInst opcode addrmode args

-- | Dispatch execution of each CPU instruction
execInst :: MonadCPU m => OpCode -> AddrMode -> [Word8] -> m ()
execInst ADC Immediate [imm] = adcImm imm
execInst ADC Immediate args = throwError (CPUECantGetOpArgs args)
execInst op addr args = throwError (CPUEInstNotImplemented op addr args)

-- | Execute ADC with immediate address
adcImm :: MonadCPU m => Word8 -> m ()
adcImm imm = modify (registers.a %~ (+Register imm))

testAdcImm :: Bool
testAdcImm
  = runExcept (execStateT (replicateM 2 step) (initCPUState (mkRom [0x29, 55, 0x29, 20])))
  ^? _Right.registers.a == Just (55 + 20)
