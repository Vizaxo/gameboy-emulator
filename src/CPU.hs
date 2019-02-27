-- | Emulation of the Game Boy's Z80/8080-inspired CPU
module CPU where

import Prelude hiding (log)
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Bits hiding (bit)
import qualified Data.Map as M
import Data.Int
import Data.Word
import Data.Tuple

import Bits
import CPUState
import Instructions
import Logger
import Utils

-- | Errors the CPU can throw
data CPUError
  = CPUEInstNotImplemented Inst
  | CPUEInstLookupFailed Word8 (Maybe Word8)
  | CPUEInstFetchFailed Word16
  | CPUEMemoryLookupFailed Word16
  | CPUEBadLdInst
  | CPUEFFLoop
  deriving Show

-- | The monad class for CPU operations
type MonadCPU m = (MonadState CPUState m, MonadError CPUError m, MonadLogger m)

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
  log Debug $ "Decoding inst at " <> showT pc_
  byte <- throwWhenNothing (CPUEInstFetchFailed pc_)
    $ st ^? memory pc_
  when (byte == 0xFF) (throwError CPUEFFLoop)
  case M.lookup (Opcode byte) instructions of
    Just (Left i) -> pure i
    Just (Right prefix) -> do
      byte2 <- throwWhenNothing (CPUEInstFetchFailed (pc_ + 1))
        $ st ^? memory (pc_ +1)
      case M.lookup (Opcode byte2) prefix of
        Just i -> pure i
        Nothing -> throwError (CPUEInstLookupFailed byte (Just byte2))
    Nothing -> throwError (CPUEInstLookupFailed byte Nothing)

-- | Execute a CPU instruction, including updating the PC and clock
execInst :: MonadCPU m => Word16 -> Inst -> m ()
execInst pc_ (Inst op cycles) = do
  log Debug $ "Executing " <> showT op
  execOp op >>= \case
    True -> modify (registers.pc %~ (+ opLen op))
    False -> pure ()
  modify (clocktime %~ (+ cycles))

-- | Execute the CPU operation. Returns whether the PC should be
-- updated (i.e. False if this instruction was a jump).
execOp :: MonadCPU m => Op -> m Bool
execOp (Add dest src) = aluOp aluPlus (Reg dest) src
execOp (Sub src) = aluOp aluSub (Reg A) src
execOp (Cp src) = aluOpDiscard aluSub (Reg A) src
execOp (And src) = aluOp (liftAlu (.&.)) (Reg A) src
execOp (Or src) = aluOp (liftAlu (.|.)) (Reg A) src
execOp (Xor src) = aluOp (liftAlu xor) (Reg A) src
execOp (Jp cond dest) = withParam 0 dest (whenCond cond . jumpTo)
execOp (Jr cond dest) = withParam 0 dest (whenCond cond . jumpRel . fromIntegral)
execOp Nop = pure True
execOp (Rst p) = do
  st <- get
  push (st ^. registers.pc + 1)
  jumpTo (fromIntegral p)
execOp (Ld dest src) = True <$ (withParam 0 src $ \src' -> setParam 0 dest src')
execOp (Push p) = True <$ withParam 0 p push
execOp (Pop r) = do
  v <- pop
  modify (set (registers.regLens r) v)
  pure True
execOp (Inc p) = True <$ (withParam 0 p $ \p' -> setParam 0 p (p'+1))
execOp (Dec p) = True <$ (withParam 0 p $ \p' -> setParam 0 p (p'-1))
execOp Rrca = aluOp rrca (Reg A) (Reg A)
execOp Di = pure True --TODO: interrupts
execOp Ei = pure True --TODO: interrupts
execOp (Call cond dest) = withParam 0 dest $ whenCond cond . call
execOp (Ret cond) = whenCond cond ret
execOp Daa = pure True --TODO: BCD
execOp Cpl = cpl
execOp Ccf = do
  mapM resetFlag [FlagN, FlagH]
  modify (over (registers.f.bit 4) not)
  pure True
execOp Scf = True <$ (setFlag FlagC >> mapM resetFlag [FlagN, FlagH])
execOp (Set n p) = True <$ (withParam 0 p $ \p' -> setParam 0 p (setBit p' n))
execOp (Res n p) = True <$ (withParam 0 p $ \p' -> setParam 0 p (clearBit p' n))
execOp (Swap p) = aluOp (liftAluUnary (flip rotate 4)) p p

rrca :: Word8 -> Word8 -> ([Flag], Word8)
rrca _ a = swap $ runWriter $ do
  when (testBit a 0) (tell [FlagC])
  pure (rotateR a 1)

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

liftAluUnary :: (SizeTy size -> SizeTy size)
  -> SizeTy size -> SizeTy size -> ([Flag], SizeTy size)
liftAluUnary op x y = ([], op y)

push :: MonadCPU m => Word16 -> m ()
push v = do
  log Debug $ "Pushing " <> showT v
  decrement SP
  st <- get
  modify (set (memory (st ^. registers.sp)) (v ^. upper))
  decrement SP
  st <- get
  modify (set (memory (st ^. registers.sp)) (v ^. lower))

memoryLookup addr = do
  st <- get
  throwWhenNothing (CPUEMemoryLookupFailed addr) (st ^? memory addr)

pop :: MonadCPU m => m Word16
pop = do
  st <- get
  lower <- memoryLookup (st ^. registers.sp)
  increment SP
  st <- get
  upper <- memoryLookup (st ^. registers.sp)
  increment SP
  log Debug $ "Popped " <> showT (twoBytes lower upper)
  pure (twoBytes lower upper)

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

resetFlag :: MonadCPU m => Flag -> m ()
resetFlag FlagZ = modify (set (registers.f.bit 7) False)
resetFlag FlagN = modify (set (registers.f.bit 6) False)
resetFlag FlagH = modify (set (registers.f.bit 5) False)
resetFlag FlagC = modify (set (registers.f.bit 4) False)

evalFlag :: MonadCPU m => Maybe Cond -> m Bool
evalFlag Nothing = pure True
evalFlag (Just f) = testCond f

whenCond :: MonadCPU m => Maybe Cond -> m Bool -> m Bool
whenCond cond ma = evalFlag cond >>= \case
  True -> ma
  False -> pure True

jumpTo :: MonadCPU m => Word16 -> m Bool
jumpTo addr = False <$ modify (set (registers.pc) addr)

jumpRel :: MonadCPU m => Int8 -> m Bool
jumpRel offset = False <$ modify (over (registers.pc) (+ fromIntegral offset))

call :: MonadCPU m => Word16 -> m Bool
call dest = do
  st <- get
  push (st ^. registers.pc + 3)
  log Debug $ "Pushing " <> showT (st ^. registers.pc + 2)
  jumpTo dest

ret :: MonadCPU m => m Bool
ret = do
  retAddr <- pop
  log Debug $ "Returning to " <> showT retAddr
  jumpTo retAddr

cpl :: MonadCPU m => m Bool
cpl = do
  mapM setFlag [FlagN, FlagH]
  modify (over (registers.a) complement)
  pure True

aluOp :: (RegLens size, MonadCPU m, DispatchSizeTy size, Num (SizeTy size), Eq (SizeTy size))
  => (SizeTy size -> SizeTy size -> ([Flag], SizeTy size))
  -> Param size -> Param size -> m Bool
aluOp = aluOp' True

aluOpDiscard :: (RegLens size, MonadCPU m, DispatchSizeTy size, Num (SizeTy size), Eq (SizeTy size))
  => (SizeTy size -> SizeTy size -> ([Flag], SizeTy size))
  -> Param size -> Param size -> m Bool
aluOpDiscard = aluOp' False

aluOp' :: (RegLens size, MonadCPU m, DispatchSizeTy size, Num (SizeTy size), Eq (SizeTy size))
  => Bool -> (SizeTy size -> SizeTy size -> ([Flag], SizeTy size))
  -> Param size -> Param size -> m Bool
aluOp' update op dest src =
  withParam 0 src $ \src' -> do
    withParam 0 dest $ \dest' -> do
      st <- get
      let (flags, res) = op dest' src'
      modify (set (registers . f) 0)
      when (res == 0) (setFlag FlagZ)
      mapM setFlag flags
      when update (setParam 0 dest res)
      pure True

--TODO: refactor index argument. Seems very easy to mess up.
withParam :: (MonadCPU m, RegLens size, DispatchSizeTy size, Num (SizeTy size))
  => Word16 -> Param size -> (SizeTy size -> m a) -> m a
withParam _ (Reg r) f = f =<< (get <&> view (registers.regLens r))
withParam i (AddrOf p) f = withParam i p (f <=< lookupAddr)
withParam i (AddrOfH p) f
  = withParam i p ((f <=< lookupAddr) . (+0xFF00) . fromIntegral)
withParam i (RegPlus r p) f = withParam i p
  (\p' -> f =<< ((+ fromIntegral p') . view (registers.regLens r) <$> get))
withParam i Imm f = f =<< pcPlusOffset 1
withParam i (PostDec p) f = withParam i (Reg p) f <* decrement p
withParam i (PostInc p) f = withParam i (Reg p) f <* increment p

--TODO: refactor index argument. Seems very easy to mess up.
setParam :: (MonadCPU m, RegLens size, DispatchSizeTy size, Num (SizeTy size))
  => Word16 -> Param size -> SizeTy size -> m ()
setParam _ (Reg r) v = modify (set (registers . regLens r) v)
setParam i (AddrOf p) v = withParam i p $ \p' -> modify (set (memory p') v)
setParam i (AddrOfH p) v = withParam i p $ \p' ->
  modify (set (memory (0xFF00 + fromIntegral  p')) v)
setParam i (RegPlus _ _) v = pure ()
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

pcPlusOffset :: forall size m. (MonadCPU m, DispatchSizeTy size) => Word16 -> m (SizeTy size)
pcPlusOffset offset = do
  st <- get
  let pcOffset = (st ^.registers.pc) + offset
  getBytesAt pcOffset

lookupAddr :: MonadCPU m => Word16 -> m Word8
lookupAddr addr = do
  st <- get
  throwWhenNothing (CPUEMemoryLookupFailed addr) (st ^? memory addr)
