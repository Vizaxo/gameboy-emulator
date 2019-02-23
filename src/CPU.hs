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

import Bits
import CPUState
import Utils
import RAM
import ROM

-- | Errors the CPU can throw
data CPUError
  = CPUEInstNotImplemented OpCode [Parameter] [Word8]
  | CPUEInstLookupFailed Word8
  | CPUEInstFetchFailed Word16
  | CPUEMemoryLookupFailed Word16
  deriving Show

-- | The monad class for CPU operations
type MonadCPU m = (MonadState CPUState m, MonadError CPUError m)

-- | 6502 instruction opcodes
data OpCode
  = NOP | SET | JP | ADD | OR
  deriving Show

data Flag = FNZ | FZ | FNC | FC
  deriving Show

-- | Addressing modes for 6502 instructions
data Parameter
  = Imm1 | Imm2 | Flag Flag
  | A | B | C | D | E | F | H | L
  | AddrHL
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
  , (0x87, Left (Inst ADD [A] 1 4))
  , (0x80, Left (Inst ADD [B] 1 4))
  , (0x81, Left (Inst ADD [C] 1 4))
  , (0x82, Left (Inst ADD [D] 1 4))
  , (0x83, Left (Inst ADD [E] 1 4))
  , (0x84, Left (Inst ADD [H] 1 4))
  , (0x85, Left (Inst ADD [L] 1 4))
  , (0x86, Left (Inst ADD [AddrHL] 1 8))
  , (0xC6, Left (Inst ADD [Imm1] 2 8))
  , (0xB2, Left (Inst OR [D] 1 4))
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
  let pc_ = st ^. registers . pc

  instByte <- throwWhenNothing (CPUEInstFetchFailed pc_)
    $ st ^? rom . ix (finite @ROMSize (fromIntegral pc_))
  inst <- lookupInst
  exec pc_ inst

lookupInst :: MonadCPU m => m Inst
lookupInst = do
  st <- get
  let pc_ = st ^. registers . pc

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
fetchParam (Flag f) = pure []
fetchParam A = pure <$> getRegister a
fetchParam B = pure <$> getRegister b
fetchParam C = pure <$> getRegister c
fetchParam D = pure <$> getRegister d
fetchParam E = pure <$> getRegister e
fetchParam F = pure <$> getRegister f
fetchParam H = pure <$> getRegister h
fetchParam L = pure <$> getRegister l
fetchParam AddrHL = pure <$> (lookupAddr =<< getRegister16 hl)

getRegister :: MonadCPU m => Lens' Registers Word8 -> m Word8
getRegister r = get <&> (^. registers.r)

getRegister16 :: MonadCPU m => Lens' Registers Word16 -> m Word16
getRegister16 r = get <&> (^. registers.r)

fetchParams :: MonadCPU m => [Parameter] -> m [Word8]
fetchParams ps = concat <$> mapM fetchParam ps

-- | Execute a CPU instruction
exec :: MonadCPU m => Word16 -> Inst -> m ()
exec pc_ (Inst opcode params len time) = do
  modify (registers.pc %~ (+len))
  args <- fetchParams params
  execInst opcode params args
  modify (clocktime %~ (plusOpTime time))

-- | Dispatch execution of each CPU instruction
execInst :: MonadCPU m => OpCode -> [Parameter] -> [Word8] -> m ()
execInst NOP [] [] = pure ()
execInst JP [Imm2] [l, u] = jp l u
execInst OR [r] [arg] = aluOp (.|.) arg
execInst ADD [AddrHL] [l, u] = aluOp (+) =<< lookupAddr (twoBytes l u)
execInst ADD [reg] [arg] = aluOp (+) arg
execInst op addr args = throwError (CPUEInstNotImplemented op addr args)

lookupAddr :: MonadCPU m => Word16 -> m Word8
lookupAddr addr = do
  st <- get
  throwWhenNothing (CPUEMemoryLookupFailed addr) (st ^? memory addr)

jp :: MonadCPU m => Word8 -> Word8 -> m ()
jp l u = modify (set (registers.pc) (twoBytes l u))

aluOp :: MonadCPU m => (Word8 -> Word8 -> Word8) -> Word8 -> m ()
aluOp f arg = do
  acc <- getRegister a
  modify (set (registers.a) (f acc arg))
