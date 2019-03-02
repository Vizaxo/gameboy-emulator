-- | Emulation of the Game Boy's Z80/8080-inspired CPU
module CPU where

import Prelude hiding (log)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Data.Bits hiding (bit)
import qualified Data.Map as M
import Data.Int
import Data.Text as T
import Data.Word
import Data.Tuple
import Numeric (showHex)

import Bits
import CPUState
import Instructions
import Interrupts
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
type MonadCPU m = (MonadState CPUState m, MonadError CPUError m, MonadLogger m, MonadIO m)

-- | Perform a single step of CPU execution
step :: (MonadCPU m) => m ()
step = execInst =<< lookupInst

-- | Lookup the next instruction to run
lookupInst :: (MonadCPU m) => m Inst
lookupInst = do
  st <- get
  let pc_ = st ^. registers . pc
  byte <- throwWhenNothing (CPUEInstFetchFailed pc_)
    =<< runMaybeT (readMem pc_)
  -- when (byte == 0xFF && st^?memory 0x28 == Just 0xFF) (throwError CPUEFFLoop)
  case M.lookup (Opcode byte) instructions of
    Just (Left i) -> pure i
    Just (Right prefix) -> do
      byte2 <- readMem (pc_+1)
      case M.lookup (Opcode byte2) prefix of
        Just i -> pure i
        Nothing -> throwError (CPUEInstLookupFailed byte (Just byte2))
    Nothing -> throwError (CPUEInstLookupFailed byte Nothing)

readMem addr = throwWhenNothing (CPUEMemoryLookupFailed addr) =<< runMaybeT (readMem' addr)

-- | Execute a CPU instruction, including updating the PC and clock
execInst :: MonadCPU m => Inst -> m ()
execInst (Inst op cycles) = do
  -- The PC is always updated to point to the *next* instruction
  -- log Debug $ T.pack (showHex oldPC "") <> ": " <> showT op
  modify (registers.pc %~ (+ opLen op))
  oldPC <- view (registers.pc) <$> get
  runReaderT (execOp op) oldPC
  modify (clocktime %~ (+ cycles))

-- | Execute the CPU operation. Reader parameter is the previous value
-- of the program counter (poniting at the currently executing
-- instruction).
execOp :: (MonadCPU m, MonadReader Word16 m) => Op -> m ()
execOp (Add dest src) = aluOp aluPlus (Reg dest) src
execOp (Adc dest src) = aluOp aluPlusCarry (Reg dest) src
execOp (Sub src) = aluOp aluSub (Reg A) src
execOp (Sbc src) = aluOp aluSubCarry (Reg A) src
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
execOp (Ld dest src) = withParam src (setParam dest)
execOp (LdImmSP dest) = withParam dest $ \dest' -> do
  st <- get
  writeMem dest' (st ^. registers.sp.lower)
  writeMem (dest' + 1) (st ^. registers.sp.upper)
execOp (Push p) = withParam p push
execOp (Pop r) = do
  v <- pop
  modify (set (registers.regLens r) v)
execOp (Inc p) = aluOp (liftAluUnary (+ 1)) p p
execOp (Dec p) = aluOp (liftAluUnary (subtract 1)) p p
execOp Rrca = aluOp rrc (Reg A) (Reg A)
execOp Rlca = aluOp rlc (Reg A) (Reg A)
execOp Rra = aluOp rr (Reg A) (Reg A)
execOp Rla = aluOp rl (Reg A) (Reg A)
execOp (Rrc p) = aluOp rrc (Reg A) p
execOp (Rlc p) = aluOp rlc (Reg A) p
execOp (Rr p) = aluOp rr (Reg A) p
execOp (Rl p) = aluOp rl (Reg A) p
execOp Di = disableInterrupts --TODO: 1 instruction delay
execOp Ei = enableInterrupts --TODO: 1 instruction delay
execOp (Call cond dest) = withParam dest $ whenCond cond . call
execOp (Ret cond) = whenCond cond ret
execOp Reti = ret >> enableInterrupts
execOp Daa = pure () --TODO: BCD
execOp Cpl = cpl
execOp Ccf = do
  mapM resetFlag [FlagN, FlagH]
  modify (over (registers.f.bit 4) not)
execOp Scf = setFlag FlagC >> mapM_ resetFlag [FlagN, FlagH]
execOp (Set n p) = withParam p $ \p' -> setParam p (setBit p' n)
execOp (Res n p) = withParam p $ \p' -> setParam p (clearBit p' n)
execOp (Bit n p) = withParam p $ \p' -> mapM_ (uncurry setFlagTo)
  [(not (testBit p' n), FlagZ), (False, FlagN), (True, FlagH)]
execOp (Swap p) = aluOp (liftAluUnary (flip rotate 4)) p p
execOp (Sla p)
  = aluOp (\_ p' -> pure (if testBit p' 7 then [FlagC] else [], shiftL p' 1)) p p
execOp (Sra p)
  = aluOp (\_ p' -> pure (if testBit p' 0 then [FlagC] else [], shiftRArithmetic p')) p p
execOp (Srl p)
  = aluOp (\_ p' -> pure (if testBit p' 0 then [FlagC] else [], shiftR p' 1)) p p
execOp Stop = modify (set stopped True)

rrc :: Applicative m => Word8 -> Word8 -> m ([Flag], Word8)
rrc _ a = pure $ swap $ runWriter $ do
  when (testBit a 0) (tell [FlagC])
  pure (rotateR a 1)

rlc :: Applicative m => Word8 -> Word8 -> m ([Flag], Word8)
rlc _ a = pure $ swap $ runWriter $ do
  when (testBit a 7) (tell [FlagC])
  pure (rotateL a 1)

rr :: MonadCPU m => Word8 -> Word8 -> m ([Flag], Word8)
rr _ a = fmap swap $ runWriterT $ do
  c <- (^. registers.f.bit 4) <$> get
  when (testBit a 0) (tell [FlagC])
  pure (set (bit 7) c (rotateR a 1))

rl :: MonadCPU m => Word8 -> Word8 -> m ([Flag], Word8)
rl _ a = fmap swap $ runWriterT $ do
  c <- (^. registers.f.bit 4) <$> get
  when (testBit a 7) (tell [FlagC])
  pure (set (bit 0) c (rotateL a 1))

aluPlus :: (Applicative m, DispatchSizeTy size, Ord (SizeTy size), Integral (SizeTy size))
  => SizeTy size -> SizeTy size -> m ([Flag], SizeTy size)
aluPlus a b = pure $ swap $ runWriter $ do
  let res = a + b
  when (fromIntegral res < (fromIntegral a + fromIntegral b)) (tell [FlagC])
  --TODO: BCD flags
  pure res

aluPlusCarry :: (MonadCPU m, DispatchSizeTy size, Integral (SizeTy size), Ord (SizeTy size))
  => SizeTy size -> SizeTy size -> m ([Flag], SizeTy size)
aluPlusCarry a b = fmap swap $ runWriterT $ do
  c <- view (registers.f.bit 4) <$> get
  let res = a + b + (if c then 1 else 0)
  when (fromIntegral res < (fromIntegral a + fromIntegral b)) (tell [FlagC])
  --TODO: BCD flags
  pure res

aluSub :: (MonadCPU m, DispatchSizeTy size, Num (SizeTy size), Ord (SizeTy size))
  => SizeTy size -> SizeTy size -> m ([Flag], SizeTy size)
aluSub a b = pure $ swap $ runWriter $ do
  let res = a - b
  when (b > a) (tell [FlagC])
  --TODO: BCD flags
  pure res

aluSubCarry :: (MonadCPU m, DispatchSizeTy size, Num (SizeTy size), Ord (SizeTy size))
  => SizeTy size -> SizeTy size -> m ([Flag], SizeTy size)
aluSubCarry a b = fmap swap $ runWriterT $ do
  c <- view (registers.f.bit 4) <$> get
  let res = a - b - (if c then 1 else 0)
  when (b > a) (tell [FlagC])
  --TODO: BCD flags
  pure res

liftAlu :: Applicative m => (SizeTy size -> SizeTy size -> SizeTy size)
  -> SizeTy size -> SizeTy size -> m ([Flag], SizeTy size)
liftAlu op x y = pure ([], op x y)

liftAluUnary :: Applicative m => (SizeTy size -> SizeTy size)
  -> SizeTy size -> SizeTy size -> m ([Flag], SizeTy size)
liftAluUnary op x y = pure ([], op y)

push :: MonadCPU m => Word16 -> m ()
push v = do
  log Debug $ "Pushing " <> T.pack (showHex v "")
  decrement SP
  st <- get
  writeMem (st ^. registers.sp) (v ^. upper)
  decrement SP
  st <- get
  writeMem (st ^. registers.sp) (v ^. lower)

pop :: MonadCPU m => m Word16
pop = do
  st <- get
  lower <- readMem (st ^. registers.sp)
  increment SP
  st <- get
  upper <- readMem (st ^. registers.sp)
  increment SP
  log Debug $ "Popping " <> T.pack (showHex (twoBytes lower upper) "")
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

setFlagTo :: MonadCPU m => Bool -> Flag -> m ()
setFlagTo b FlagZ = modify (set (registers.f.bit 7) b)
setFlagTo b FlagN = modify (set (registers.f.bit 6) b)
setFlagTo b FlagH = modify (set (registers.f.bit 5) b)
setFlagTo b FlagC = modify (set (registers.f.bit 4) b)

setFlag :: MonadCPU m => Flag -> m ()
setFlag = setFlagTo True

resetFlag :: MonadCPU m => Flag -> m ()
resetFlag = setFlagTo False

evalFlag :: MonadCPU m => Maybe Cond -> m Bool
evalFlag Nothing = pure True
evalFlag (Just f) = testCond f

whenCond :: MonadCPU m => Maybe Cond -> m () -> m ()
whenCond cond ma = evalFlag cond >>= \case
  True -> ma
  False -> pure ()

jumpTo :: MonadCPU m => Word16 -> m ()
jumpTo addr = modify (set (registers.pc) addr)

jumpRel :: MonadCPU m => Int8 -> m ()
jumpRel offset = modify (over (registers.pc) (+ fromIntegral offset))

call :: MonadCPU m => Word16 -> m ()
call dest = do
  st <- get
  push (st ^. registers.pc)
  jumpTo dest

ret :: MonadCPU m => m ()
ret = do
  retAddr <- pop
  jumpTo retAddr

cpl :: MonadCPU m => m ()
cpl = do
  mapM setFlag [FlagN, FlagH]
  modify (over (registers.a) complement)

aluOp :: (RegLens size, MonadCPU m, MonadReader Word16 m
        , DispatchSizeTy size, Num (SizeTy size), Eq (SizeTy size))
  => (SizeTy size -> SizeTy size -> m ([Flag], SizeTy size))
  -> Param size -> Param size -> m ()
aluOp = aluOp' True

aluOpDiscard :: (RegLens size, MonadCPU m, MonadReader Word16 m
               , DispatchSizeTy size, Num (SizeTy size), Eq (SizeTy size))
  => (SizeTy size -> SizeTy size -> m ([Flag], SizeTy size))
  -> Param size -> Param size -> m ()
aluOpDiscard = aluOp' False

aluOp' :: (RegLens size, MonadCPU m, MonadReader Word16 m
         , DispatchSizeTy size , Num (SizeTy size), Eq (SizeTy size))
  => Bool -> (SizeTy size -> SizeTy size -> m ([Flag], SizeTy size))
  -> Param size -> Param size -> m ()
aluOp' update op dest src =
  withParam src $ \src' -> do
    withParam dest $ \dest' -> do
      st <- get
      (flags, res) <- op dest' src'
      modify (set (registers . f) 0)
      when (res == 0) (setFlag FlagZ)
      mapM setFlag flags
      when update (setParam dest res)

withParam :: (MonadCPU m, MonadReader Word16 m, RegLens size
            , DispatchSizeTy size, Num (SizeTy size))
  => Param size -> (SizeTy size -> m a) -> m a
withParam (Reg r) f = f =<< (get <&> view (registers.regLens r))
withParam (AddrOf p) f = withParam p (f <=< readMem)
withParam (AddrOfH p) f
  = withParam p ((f <=< readMem) . (+0xFF00) . fromIntegral)
withParam (RegPlus r p) f = withParam p
  (\p' -> f =<< ((+ fromIntegral p') . view (registers.regLens r) <$> get))
withParam Imm f = f =<< pcPlusOffset 1
withParam (PostDec p) f = withParam (Reg p) f <* decrement p
withParam (PostInc p) f = withParam (Reg p) f <* increment p

setParam :: (MonadCPU m, MonadReader Word16 m, RegLens size
           , DispatchSizeTy size, Num (SizeTy size))
  => Param size -> SizeTy size -> m ()
setParam (Reg r) v = modify (set (registers . regLens r) v)
setParam (AddrOf p) v = withParam p $ \p' -> writeMem p' v
setParam (AddrOfH p) v = withParam p $ \p' ->
  writeMem (0xFF00 + fromIntegral  p') v
setParam (RegPlus _ _) v = pure ()
setParam Imm v = pure ()
setParam (PostDec p) v = setParam (Reg p) v <* decrement p
setParam (PostInc p) v = setParam (Reg p) v <* increment p

getBytesAt :: forall size m. (MonadCPU m, DispatchSizeTy size) => Word16 -> m (SizeTy size)
getBytesAt addr = dispatchSize f8 f16 where
  f8 :: m Word8
  f8 = readMem addr
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

disableInterrupts :: MonadCPU m => m ()
disableInterrupts = modify (set mie False)

enableInterrupts :: MonadCPU m => m ()
enableInterrupts = modify (set mie True)

fireInterrupt :: MonadCPU m => Interrupt -> m ()
fireInterrupt i = do
  st <- get
  when (st^.mie && st^?ifBit i == Just True) $ do
    disableInterrupts
    push (st^.registers.pc)
    overMem 0xFF0F (set (bit (interruptBit i)) True)
    modify (set (registers.pc) (irqAddr i))

overMem addr f = do
  mem <- readMem addr
  writeMem addr (f mem)
