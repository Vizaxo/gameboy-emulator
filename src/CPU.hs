-- | Emulation of the Game Boy's Z80/8080-inspired CPU
module CPU where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Finite hiding (shift)
import Data.Word

import Bits
import CPUState
import Instructions
import ROM
import Utils

-- | Errors the CPU can throw
data CPUError
  = CPUEInstNotImplemented Inst
  | CPUEInstLookupFailed Word8
  | CPUEInstFetchFailed Word16
  | CPUEMemoryLookupFailed Word16
  | CPUEBadLdInst
  deriving Show

-- | The monad class for CPU operations
type MonadCPU m = (MonadState CPUState m, MonadError CPUError m)

-- | Perform a single step of CPU execution
step :: MonadCPU m => m ()
step = do
  st <- get
  let pc_ = st ^. registers . pc

  instByte <- throwWhenNothing (CPUEInstFetchFailed pc_)
    $ st ^? rom . ix (finite @ROMSize (fromIntegral pc_))
  inst <- lookupInst
  execInst pc_ inst

-- | Lookup the next instruction to run
lookupInst :: MonadCPU m => m Inst
lookupInst = do
  st <- get
  let pc_ = st ^. registers . pc

  instByte <- throwWhenNothing (CPUEInstFetchFailed pc_)
    $ st ^? rom . ix (finite @ROMSize (fromIntegral pc_))
  throwWhenNothing (CPUEInstLookupFailed instByte) $ lookup (Opcode instByte) instructions

-- | Execute a CPU instruction, including updating the PC and clock
execInst :: MonadCPU m => Word16 -> Inst -> m ()
execInst pc_ (Inst op cycles) = do
  execOp op
  modify (registers.pc %~ (+ opLen op))
  modify (clocktime %~ (+ cycles))

-- | Execute the CPU operation
execOp :: MonadCPU m => Op -> m ()
execOp (Add dest src) = aluOp (+) dest src
execOp (Sub src) = aluOp (-) (Reg A) src
execOp Nop = pure ()
execOp (Extended i) = execOp i

-- TODO: set flags
aluOp :: (RegLens size, MonadCPU m, DispatchSizeTy size) => (SizeTy size -> SizeTy size -> SizeTy size)
      -> Param size -> Param size -> m ()
aluOp f dest src = do
    dest' <- getParam dest
    src' <- getParam src
    case dest of
      Reg r -> modify (set (registers . regLens r) (f dest' src'))
      AddrOf p -> undefined
      Imm -> undefined

getParam :: (MonadCPU m, RegLens size, DispatchSizeTy size) => Param size -> m (SizeTy size)
getParam (Reg r) = get <&> view (registers.(regLens r))
getParam (AddrOf p) = (lookupAddr =<< getParam p)
getParam Imm = pcPlusOffset 1

getBytesAt :: forall size m. (MonadCPU m, DispatchSizeTy size) => Word16 -> m (SizeTy size)
getBytesAt addr = dispatchSize f8 f16 where
  f8 :: m Word8
  f8 = get <&> (^? (memory addr))
    >>= throwWhenNothing (CPUEMemoryLookupFailed addr)
  f16 :: m Word16
  f16 = do
    --TODO: check endian-ness
    low <- getBytesAt @S8 addr
    high <- getBytesAt @S8 (addr +1)
    pure (twoBytes low high)

pcPlusOffset :: (MonadCPU m, DispatchSizeTy size) => Word16 -> m (SizeTy size)
pcPlusOffset offset = do
  st <- get
  let pcOffset = (st ^.registers.pc) + fromIntegral offset
  getBytesAt pcOffset

lookupAddr :: MonadCPU m => Word16 -> m Word8
lookupAddr addr = do
  st <- get
  throwWhenNothing (CPUEMemoryLookupFailed addr) (st ^? memory addr)
