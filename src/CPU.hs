-- Emulation of the NES' 6502-derived CPU
module CPU where

import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Data.Bits
import Data.Finite hiding (shift)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word
import Numeric.Natural

import CPUState
import Utils
import RAM
import ROM

-- | Errors the CPU can throw
data CPUError
  = CPUEInstNotImplemented OpCode [Parameter] [Word8]
  | CPUEInstLookupFailed Word8
  | CPUEInstFetchFailed Word16
  deriving Show

-- | The monad class for CPU operations
type MonadCPU m = (MonadState CPUState m, MonadError CPUError m)

-- | 6502 instruction opcodes
data OpCode
  = NOP | SET | JP | ADD
  deriving Show

-- | Addressing modes for 6502 instructions
data Parameter
  = Imm1 | Imm2 | NZ | Z | NC | C | A | B
  deriving Show

-- | The time (in cycles) an operation takes to complete
newtype OpTime = OpTime
  { _optime :: Natural
  }
  deriving (Num, Show)

-- TODO: implement page boundry cross detection
-- | Increase a number (the clock time) by the given time
plusOpTime :: Integral n => OpTime -> n -> n
plusOpTime (OpTime t) pc = pc + fromIntegral t

-- | A complete specification of a single 6502 instruction
data Inst = Inst OpCode [Parameter] Word16 OpTime

type Insts = Map Word8 (Either Inst (Map Word8 Inst))

instLookup :: Word8 -> Insts -> Maybe (Either Inst (Word8 -> Maybe Inst))
instLookup k m = M.lookup k m >>= \case
  Left v -> pure $ Left v
  Right m' -> pure $ Right (\k -> M.lookup k m')

-- | A mapping from opcodes to instructions
instructions :: Insts
instructions = M.fromList
  [ (0x00, Left (Inst NOP [] 1 4))
  , (0x80, Left (Inst ADD [A, B] 1 4))
  , (0xC3, Left (Inst JP [Imm2] 3 12))
  , (0xCB, Right extendedInstrs)
  ]

extendedInstrs :: Map Word8 Inst
extendedInstrs = M.fromList
  [ (0xC3, Inst SET undefined undefined undefined)
  ]

-- | Perform a single step of CPU execution
step :: MonadCPU m => m ()
step = do
  st <- get
  let (Register16 pc_) = st ^. registers . pc

  instByte <- throwWhenNothing (CPUEInstFetchFailed pc_)
    $ st ^? rom . ix (finite @ROMSize (fromIntegral pc_))
  inst <- lookupInst
  exec pc_ inst

lookupInst :: MonadCPU m => m Inst
lookupInst = do
  st <- get
  let (Register16 pc_) = st ^. registers . pc

  instByte <- throwWhenNothing (CPUEInstFetchFailed pc_)
    $ st ^? rom . ix (finite @ROMSize (fromIntegral pc_))
  inst <- throwWhenNothing (CPUEInstLookupFailed instByte) $ instLookup instByte instructions
  case inst of
    Left i -> pure i
    Right f -> do
      instByte2 <- throwWhenNothing (CPUEInstFetchFailed pc_)
        $ st ^? rom . ix (finite @ROMSize (fromIntegral (pc_ + 1)))
      throwWhenNothing (CPUEInstFetchFailed pc_) $ f instByte2

byteAtPCOffset :: MonadCPU m => Word16 -> m Word8
byteAtPCOffset offset = do
  st <- get
  let pcOffset = (st ^.registers.pc) + fromIntegral offset
  let i = finite @32768 (fromIntegral pcOffset)
  pure (indexRAM (st ^. rom) i)

fetchParam :: MonadCPU m => Parameter -> m [Word8]
fetchParam Imm1 = pure <$> byteAtPCOffset 1
fetchParam Imm2 = (\a b -> [a,b]) <$> byteAtPCOffset 1 <*> byteAtPCOffset 2
fetchParam NZ = pure []
fetchParam Z = pure []
fetchParam NC = pure []
fetchParam C = pure []
fetchParam A = getRegister a
fetchParam B = getRegister b

getRegister :: MonadCPU m => Lens' Registers Register -> m [Word8]
getRegister r = get <&> (^.. registers.r.reg)

fetchParams :: MonadCPU m => [Parameter] -> m [Word8]
fetchParams ps = concat <$> mapM fetchParam ps

-- | Execute a CPU instruction
exec :: MonadCPU m => Word16 -> Inst -> m ()
exec pc_ (Inst opcode params len time) = do
  modify (registers.pc %~ (+Register16 len))
  args <- fetchParams params
  execInst opcode params args
  modify (clocktime %~ (plusOpTime time))

-- | Dispatch execution of each CPU instruction
execInst :: MonadCPU m => OpCode -> [Parameter] -> [Word8] -> m ()
execInst NOP [] [] = nop
execInst JP [Imm2] [l, u] = jp l u
execInst ADD [A, reg] [x, y] = addA x y
execInst op addr args = throwError (CPUEInstNotImplemented op addr args)

nop :: MonadCPU m => m ()
nop = pure ()

jp :: MonadCPU m => Word8 -> Word8 -> m ()
jp (fromIntegral -> l) (fromIntegral -> u)
  = let addr = shift @Word16 u 8 .|. l
    in modify (set (registers.pc.reg16) addr)

addA :: MonadCPU m => Word8 -> Word8 -> m ()
addA x y = void (get <&> set (registers.a.reg) (x + y))
