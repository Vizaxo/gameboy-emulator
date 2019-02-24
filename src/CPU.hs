-- | Emulation of the Game Boy's Z80/8080-inspired CPU
module CPU where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Bits hiding (bit)
import qualified Data.Map as M
import Data.Word
import Data.Tuple

import Bits
import CPUState
import Instructions
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
  atPc <- getBytesAt @S8 (st ^. registers.pc)
  execInst pc_ =<< lookupInst

-- | Lookup the next instruction to run
lookupInst :: MonadCPU m => m Inst
lookupInst = do
  st <- get
  let pc_ = st ^. registers . pc

  instByte <- throwWhenNothing (CPUEInstFetchFailed pc_)
    $ st ^? memory pc_
  throwWhenNothing (CPUEInstLookupFailed instByte) $ M.lookup (Opcode instByte) instructions

-- | Execute a CPU instruction, including updating the PC and clock
execInst :: MonadCPU m => Word16 -> Inst -> m ()
execInst pc_ (Inst op cycles) = do
  execOp op >>= \case
    True -> modify (registers.pc %~ (+ opLen op))
    False -> pure ()
  modify (clocktime %~ (+ cycles))

-- | Execute the CPU operation. Returns whether the PC should be
-- updated (i.e. False if this instruction was a jump).
execOp :: MonadCPU m => Op -> m Bool
execOp (Add dest src) = aluOp aluPlus dest src
execOp (Sub src) = aluOp aluSub A src
execOp (Xor src) = aluOp (liftAlu xor) A src
execOp (Jp flag dest) = withParam 0 dest (jumpTo flag)
execOp (Jr flag dest) = withParam 0 dest (jumpRel flag)
execOp Nop = pure True
execOp (Rst p) = do
  st <- get
  push (st ^. registers.pc)
  jumpTo Nothing (fromIntegral p)
execOp (Ld dest src) = True <$ (withParam 0 src $ \src' -> setParam 0 dest src')
execOp (Inc p) = True <$ (withParam 0 p $ \p' -> setParam 0 p (p'+1))
execOp (Dec p) = True <$ (withParam 0 p $ \p' -> setParam 0 p (p'-1))
execOp (Extended i) = execOp i
execOp Di = pure True --TODO: interrupts
execOp Ei = pure True --TODO: interrupts

aluPlus :: forall size. (DispatchSizeTy size, Num (SizeTy size), Ord (SizeTy size))
  => SizeTy size -> SizeTy size -> ([Flag], SizeTy size)
aluPlus a b = swap $ runWriter $ do
  let res = a + b
  when (res < a) (tell [FlagC])
  --TODO: BCD flags
  pure res

aluSub :: forall size. (DispatchSizeTy size, Num (SizeTy size), Ord (SizeTy size))
  => SizeTy size -> SizeTy size -> ([Flag], SizeTy size)
aluSub a b = swap $ runWriter $ do
  let res = a - b
  when (b > a) (tell [FlagC])
  --TODO: BCD flags
  pure res

liftAlu :: (SizeTy size -> SizeTy size -> SizeTy size)
  -> SizeTy size -> SizeTy size -> ([Flag], SizeTy size)
liftAlu op x y = ([], op x y)

push :: MonadCPU m => Word16 -> m ()
push v = do
  st <- get
  decrement SP
  modify (set (memory (st ^. registers.sp)) (st ^. registers.pc.upper))
  decrement SP
  modify (set (memory (st ^. registers.sp)) (st ^. registers.pc.lower))

decrement :: (RegLens size, MonadCPU m, DispatchSizeTy size, Num (SizeTy size))
  => Register size -> m ()
decrement r = modify (over (registers.regLens r) (subtract 1))

increment :: (RegLens size, MonadCPU m, DispatchSizeTy size, Num (SizeTy size))
  => Register size -> m ()
increment r = modify (over (registers.regLens r) (+1))

testCond :: MonadCPU m => Cond -> m Bool
testCond CondNZ = not <$> testCond CondZ
testCond CondZ = flip testBit 7 . view (registers.f) <$> get
testCond CondNC = not <$> testCond CondC
testCond CondC = flip testBit 4 . view (registers.f) <$> get

setFlag :: MonadCPU m => Flag -> m ()
setFlag FlagZ = modify (set (registers.f.bit 7) True)
setFlag FlagN = modify (set (registers.f.bit 6) True)
setFlag FlagH = modify (set (registers.f.bit 5) True)
setFlag FlagC = modify (set (registers.f.bit 4) True)

evalFlag :: MonadCPU m => Maybe Cond -> m Bool
evalFlag Nothing = pure True
evalFlag (Just f) = testCond f

jumpTo :: MonadCPU m => Maybe Cond -> Word16 -> m Bool
jumpTo cond addr = evalFlag cond >>= \case
  True -> False <$ modify (set (registers.pc) addr)
  False -> pure True

jumpRel :: MonadCPU m => Maybe Cond -> Word8 -> m Bool
jumpRel cond offset = evalFlag cond >>= \case
  True -> False <$ modify (over (registers.pc) (+ fromIntegral offset))
  False -> pure True

aluOp :: (RegLens size, MonadCPU m, DispatchSizeTy size, Num (SizeTy size), Eq (SizeTy size))
  => (SizeTy size -> SizeTy size -> ([Flag], SizeTy size))
  -> Register size -> Param size -> m Bool
aluOp op dest src =
  withParam 0 src $ \src' -> do
    st <- get
    let dest' = st ^. registers . regLens dest
    let (flags, res) = op dest' src'
    modify (set (registers . f) 0)
    when (res == 0) (setFlag FlagZ)
    mapM setFlag flags
    modify (set (registers . regLens dest) res)
    pure True

--TODO: refactor index argument. Seems very easy to mess up.
withParam :: (MonadCPU m, RegLens size, DispatchSizeTy size, Num (SizeTy size))
  => Word16 -> Param size -> (SizeTy size -> m a) -> m a
withParam _ (Reg r) f = f =<< (get <&> view (registers.(regLens r)))
withParam i (AddrOf p) f = withParam i p (f <=< lookupAddr)
withParam i Imm f = f =<< pcPlusOffset (i + 1)
withParam i (PostDec p) f = withParam i (Reg p) f <* decrement p
withParam i (PostInc p) f = withParam i (Reg p) f <* increment p

--TODO: refactor index argument. Seems very easy to mess up.
setParam :: (MonadCPU m, RegLens size, DispatchSizeTy size, Num (SizeTy size))
  => Word16 -> Param size -> SizeTy size -> m ()
setParam _ (Reg r) v = modify (set (registers . regLens r) v)
setParam i (AddrOf p) v = withParam i p $ \p' -> modify (set (memory p') v)
setParam i Imm v = pure ()
setParam i (PostDec p) v = setParam i (Reg p) v <* decrement p
setParam i (PostInc p) v = setParam i (Reg p) v <* increment p

getBytesAt :: forall size m. (MonadCPU m, DispatchSizeTy size) => Word16 -> m (SizeTy size)
getBytesAt addr = dispatchSize f8 f16 where
  f8 :: m Word8
  f8 = get <&> (^? (memory addr))
    >>= throwWhenNothing (CPUEMemoryLookupFailed addr)
  f16 :: m Word16
  f16 = do
    low <- getBytesAt @S8 addr
    high <- getBytesAt @S8 (addr +1)
    st <- get
    atPc <- getBytesAt @S8 (st ^. registers.pc)
    pure (twoBytes low high)

pcPlusOffset :: (MonadCPU m, DispatchSizeTy size) => Word16 -> m (SizeTy size)
pcPlusOffset offset = do
  st <- get
  let pcOffset = (st ^.registers.pc) + offset
  getBytesAt pcOffset

lookupAddr :: MonadCPU m => Word16 -> m Word8
lookupAddr addr = do
  st <- get
  throwWhenNothing (CPUEMemoryLookupFailed addr) (st ^? memory addr)
