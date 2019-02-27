-- | Emulation of the Game Boy's Z80/8080-inspired CPU
module CPU where

import Prelude hiding (log)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
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
      log Debug $ "CB PREFIX: " <> showT byte <> " " <> showT byte2
      case M.lookup (Opcode byte2) prefix of
        Just i -> pure i
        Nothing -> throwError (CPUEInstLookupFailed byte (Just byte2))
    Nothing -> throwError (CPUEInstLookupFailed byte Nothing)

-- | Execute a CPU instruction, including updating the PC and clock
execInst :: MonadCPU m => Word16 -> Inst -> m ()
execInst pc_ (Inst op cycles) = do
  log Debug $ "Executing " <> showT op
  -- The PC is always updated to point to the *next* instruction
  oldPC <- view (registers.pc) <$> get
  modify (registers.pc %~ (+ opLen op))
  runReaderT (execOp op) oldPC
  st <- get
  log Debug $ "State: " <> showT (st ^. registers) <> "\n" <> showT (st ^? memory (0xffe1))
  modify (clocktime %~ (+ cycles))

-- | Execute the CPU operation. Reader parameter is the previous value
-- of the program counter (poniting at the currently executing
-- instruction).
execOp :: (MonadCPU m, MonadReader Word16 m) => Op -> m ()
execOp (Add dest src) = aluOp aluPlus (Reg dest) src
execOp (Sub src) = aluOp aluSub (Reg A) src
execOp (Cp src) = aluOpDiscard aluSub (Reg A) src
execOp (And src) = aluOp (liftAlu (.&.)) (Reg A) src
execOp (Or src) = aluOp (liftAlu (.|.)) (Reg A) src
execOp (Xor src) = aluOp (liftAlu xor) (Reg A) src
execOp (Jp cond dest) = withParam dest (whenCond cond . jumpTo)
execOp (Jr cond dest) = withParam dest (whenCond cond . jumpRel . fromIntegral)
execOp Nop = pure ()
execOp (Rst p) = do
  st <- get
  push (st ^. registers.pc)
  jumpTo (fromIntegral p)
execOp (Ld dest src) = withParam src $ \src' -> setParam dest src'
execOp (Push p) = withParam p push
execOp (Pop r) = do
  v <- pop
  modify (set (registers.regLens r) v)
execOp (Inc p) = aluOp (liftAluUnary (+ 1)) p p
execOp (Dec p) = aluOp (liftAluUnary (subtract 1)) p p
execOp Rrca = aluOp rrca (Reg A) (Reg A)
execOp Di = pure () --TODO: interrupts
execOp Ei = pure () --TODO: interrupts
execOp (Call cond dest) = withParam dest $ whenCond cond . call
execOp (Ret cond) = whenCond cond ret
execOp Daa = pure () --TODO: BCD
execOp Cpl = cpl
execOp Ccf = do
  mapM resetFlag [FlagN, FlagH]
  modify (over (registers.f.bit 4) not)
execOp Scf = setFlag FlagC >> mapM_ resetFlag [FlagN, FlagH]
execOp (Set n p) = withParam p $ \p' -> setParam p (setBit p' n)
execOp (Res n p) = withParam p $ \p' -> setParam p (clearBit p' n)
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

whenCond :: MonadCPU m => Maybe Cond -> m () -> m ()
whenCond cond ma = evalFlag cond >>= \case
  True -> ma
  False -> pure ()

jumpTo :: MonadCPU m => Word16 -> m ()
jumpTo addr = modify (set (registers.pc) addr)

-- TODO: I think negative jumps are broken
jumpRel :: MonadCPU m => Int8 -> m ()
jumpRel offset = modify (over (registers.pc) (+ fromIntegral offset))

call :: MonadCPU m => Word16 -> m ()
call dest = do
  st <- get
  push (st ^. registers.pc)
  log Debug $ "Pushing " <> showT (st ^. registers.pc)
  jumpTo dest

ret :: MonadCPU m => m ()
ret = do
  retAddr <- pop
  log Debug $ "Returning to " <> showT retAddr
  jumpTo retAddr

cpl :: MonadCPU m => m ()
cpl = do
  mapM setFlag [FlagN, FlagH]
  modify (over (registers.a) complement)

aluOp :: (RegLens size, MonadCPU m, MonadReader Word16 m
        , DispatchSizeTy size, Num (SizeTy size), Eq (SizeTy size))
  => (SizeTy size -> SizeTy size -> ([Flag], SizeTy size))
  -> Param size -> Param size -> m ()
aluOp = aluOp' True

aluOpDiscard :: (RegLens size, MonadCPU m, MonadReader Word16 m
               , DispatchSizeTy size, Num (SizeTy size), Eq (SizeTy size))
  => (SizeTy size -> SizeTy size -> ([Flag], SizeTy size))
  -> Param size -> Param size -> m ()
aluOpDiscard = aluOp' False

aluOp' :: (RegLens size, MonadCPU m, MonadReader Word16 m
         , DispatchSizeTy size , Num (SizeTy size), Eq (SizeTy size))
  => Bool -> (SizeTy size -> SizeTy size -> ([Flag], SizeTy size))
  -> Param size -> Param size -> m ()
aluOp' update op dest src =
  withParam src $ \src' -> do
    withParam dest $ \dest' -> do
      st <- get
      let (flags, res) = op dest' src'
      modify (set (registers . f) 0)
      when (res == 0) (setFlag FlagZ)
      mapM setFlag flags
      when update (setParam dest res)

--TODO: refactor index argument. Seems very easy to mess up.
withParam :: (MonadCPU m, MonadReader Word16 m, RegLens size
            , DispatchSizeTy size, Num (SizeTy size))
  => Param size -> (SizeTy size -> m a) -> m a
withParam (Reg r) f = f =<< (get <&> view (registers.regLens r))
withParam (AddrOf p) f = withParam p (f <=< lookupAddr)
withParam (AddrOfH p) f
  = withParam p ((f <=< lookupAddr) . (+0xFF00) . fromIntegral)
withParam (RegPlus r p) f = withParam p
  (\p' -> f =<< ((+ fromIntegral p') . view (registers.regLens r) <$> get))
withParam Imm f = f =<< pcPlusOffset 1
withParam (PostDec p) f = withParam (Reg p) f <* decrement p
withParam (PostInc p) f = withParam (Reg p) f <* increment p

--TODO: refactor index argument. Seems very easy to mess up.
setParam :: (MonadCPU m, MonadReader Word16 m, RegLens size
           , DispatchSizeTy size, Num (SizeTy size))
  => Param size -> SizeTy size -> m ()
setParam (Reg r) v = modify (set (registers . regLens r) v)
setParam (AddrOf p) v = withParam p $ \p' -> modify (set (memory p') v)
setParam (AddrOfH p) v = withParam p $ \p' ->
  modify (set (memory (0xFF00 + fromIntegral  p')) v)
setParam (RegPlus _ _) v = pure ()
setParam Imm v = pure ()
setParam (PostDec p) v = setParam (Reg p) v <* decrement p
setParam (PostInc p) v = setParam (Reg p) v <* increment p

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
    pure (twoBytes low high)

pcPlusOffset :: forall size m. (MonadCPU m, MonadReader Word16 m, DispatchSizeTy size)
  => Word16 -> m (SizeTy size)
pcPlusOffset offset = do
  oldPC <- ask
  getBytesAt (oldPC + offset)

lookupAddr :: MonadCPU m => Word16 -> m Word8
lookupAddr addr = do
  st <- get
  throwWhenNothing (CPUEMemoryLookupFailed addr) (st ^? memory addr)
