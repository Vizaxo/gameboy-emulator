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
  = CPUEInstNotImplemented Op [Source] [Word8]
  | CPUEInstLookupFailed Word8
  | CPUEInstFetchFailed Word16
  | CPUEMemoryLookupFailed Word16
  | CPUEBadLdInst
  deriving Show

-- | The monad class for CPU operations
type MonadCPU m = (MonadState CPUState m, MonadError CPUError m)

-- | CPU instruction opcodes
data Op
  = LD
  | ADD  | ADC | SUB | SBC | AND | XOR | OR | CP | INC | DEC
  | ADD16
  | NOP | SET | SWAP | DAA | CPL | CCF | SCF | HALT | STOP | DI | EI
  | JP
  deriving Show

data Flag = FNZ | FZ | FNC | FC
  deriving Show

type Reg8 = Lens' Registers Word8
type Reg16 = Lens' Registers Word16

data Source
  = Imm1 | Imm2
  | Flag Flag
  | Reg8 Reg8 | Reg16 Reg16
  | AddrOf Reg16
  | AddrOfC

instance Show Source where
  show Imm1 = "Imm1"
  show Imm2 = "Imm2"
  show (Flag f) = "Flag " <> show f
  show (Reg8 r) = "Reg8"
  show (Reg16 r) = "Reg16"
  show (AddrOf p) = "AddrOf"
  show AddrOfC = "AddrOfC"

data Dest
  = DReg8 Reg8
  | DReg16 Reg16
  | DAddrOf16 Reg16
  | DAddrOfImm16
  | DAddrOfC
  | DAddrOfImmH

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
data Inst = Inst Op (Maybe Dest) [Source] Word16 OpTime

type Insts = Map Word8 (Either Inst (Map Word8 Inst))

instLookup :: Word8 -> Insts -> Maybe (Either Inst (Word8 -> Maybe Inst))
instLookup k m = M.lookup k m >>= \case
  Left v -> pure $ Left v
  Right m' -> pure $ Right (\k -> M.lookup k m')

-- | A mapping from opcodes to instructions
instructions :: Insts
instructions = M.fromList
  -- LD nn, n
  [ (0x06, Left (Inst LD (Just (DReg8 b)) [Imm1] 2 8))
  , (0x0E, Left (Inst LD (Just (DReg8 c)) [Imm1] 2 8))
  , (0x16, Left (Inst LD (Just (DReg8 d)) [Imm1] 2 8))
  , (0x1E, Left (Inst LD (Just (DReg8 e)) [Imm1] 2 8))
  , (0x26, Left (Inst LD (Just (DReg8 h)) [Imm1] 2 8))
  , (0x2E, Left (Inst LD (Just (DReg8 l)) [Imm1] 2 8))

  -- LD r1, r2
  , (0x7F, Left (Inst LD (Just (DReg8 a)) [Reg8 a] 1 4))
  , (0x78, Left (Inst LD (Just (DReg8 a)) [Reg8 b] 1 4))
  , (0x79, Left (Inst LD (Just (DReg8 a)) [Reg8 c] 1 4))
  , (0x7A, Left (Inst LD (Just (DReg8 a)) [Reg8 d] 1 4))
  , (0x7B, Left (Inst LD (Just (DReg8 a)) [Reg8 e] 1 4))
  , (0x7C, Left (Inst LD (Just (DReg8 a)) [Reg8 h] 1 4))
  , (0x7D, Left (Inst LD (Just (DReg8 a)) [Reg8 l] 1 4))
  , (0x7E, Left (Inst LD (Just (DReg8 a)) [AddrOf hl] 1 8))
  , (0x40, Left (Inst LD (Just (DReg8 b)) [Reg8 b] 1 4))
  , (0x41, Left (Inst LD (Just (DReg8 b)) [Reg8 c] 1 4))
  , (0x42, Left (Inst LD (Just (DReg8 b)) [Reg8 d] 1 4))
  , (0x43, Left (Inst LD (Just (DReg8 b)) [Reg8 e] 1 4))
  , (0x44, Left (Inst LD (Just (DReg8 b)) [Reg8 h] 1 4))
  , (0x45, Left (Inst LD (Just (DReg8 b)) [Reg8 l] 1 4))
  , (0x46, Left (Inst LD (Just (DReg8 b)) [AddrOf hl] 1 8))
  , (0x48, Left (Inst LD (Just (DReg8 c)) [Reg8 b] 1 4))
  , (0x49, Left (Inst LD (Just (DReg8 c)) [Reg8 c] 1 4))
  , (0x4A, Left (Inst LD (Just (DReg8 c)) [Reg8 d] 1 4))
  , (0x4B, Left (Inst LD (Just (DReg8 c)) [Reg8 e] 1 4))
  , (0x4C, Left (Inst LD (Just (DReg8 c)) [Reg8 h] 1 4))
  , (0x4D, Left (Inst LD (Just (DReg8 c)) [Reg8 l] 1 4))
  , (0x4E, Left (Inst LD (Just (DReg8 c)) [AddrOf hl] 1 8))
  , (0x50, Left (Inst LD (Just (DReg8 d)) [Reg8 b] 1 4))
  , (0x51, Left (Inst LD (Just (DReg8 d)) [Reg8 c] 1 4))
  , (0x52, Left (Inst LD (Just (DReg8 d)) [Reg8 d] 1 4))
  , (0x53, Left (Inst LD (Just (DReg8 d)) [Reg8 e] 1 4))
  , (0x54, Left (Inst LD (Just (DReg8 d)) [Reg8 h] 1 4))
  , (0x55, Left (Inst LD (Just (DReg8 d)) [Reg8 l] 1 4))
  , (0x56, Left (Inst LD (Just (DReg8 d)) [AddrOf hl] 1 8))
  , (0x58, Left (Inst LD (Just (DReg8 e)) [Reg8 b] 1 4))
  , (0x59, Left (Inst LD (Just (DReg8 e)) [Reg8 c] 1 4))
  , (0x5A, Left (Inst LD (Just (DReg8 e)) [Reg8 d] 1 4))
  , (0x5B, Left (Inst LD (Just (DReg8 e)) [Reg8 e] 1 4))
  , (0x5C, Left (Inst LD (Just (DReg8 e)) [Reg8 h] 1 4))
  , (0x5D, Left (Inst LD (Just (DReg8 e)) [Reg8 l] 1 4))
  , (0x5E, Left (Inst LD (Just (DReg8 e)) [AddrOf hl] 1 8))
  , (0x60, Left (Inst LD (Just (DReg8 h)) [Reg8 b] 1 4))
  , (0x61, Left (Inst LD (Just (DReg8 h)) [Reg8 c] 1 4))
  , (0x62, Left (Inst LD (Just (DReg8 h)) [Reg8 d] 1 4))
  , (0x63, Left (Inst LD (Just (DReg8 h)) [Reg8 e] 1 4))
  , (0x64, Left (Inst LD (Just (DReg8 h)) [Reg8 h] 1 4))
  , (0x65, Left (Inst LD (Just (DReg8 h)) [Reg8 l] 1 4))
  , (0x66, Left (Inst LD (Just (DReg8 h)) [AddrOf hl] 1 8))
  , (0x68, Left (Inst LD (Just (DReg8 l)) [Reg8 b] 1 4))
  , (0x69, Left (Inst LD (Just (DReg8 l)) [Reg8 c] 1 4))
  , (0x6A, Left (Inst LD (Just (DReg8 l)) [Reg8 d] 1 4))
  , (0x6B, Left (Inst LD (Just (DReg8 l)) [Reg8 e] 1 4))
  , (0x6C, Left (Inst LD (Just (DReg8 l)) [Reg8 h] 1 4))
  , (0x6D, Left (Inst LD (Just (DReg8 l)) [Reg8 l] 1 4))
  , (0x6E, Left (Inst LD (Just (DReg8 l)) [AddrOf hl] 1 8))
  , (0x70, Left (Inst LD (Just (DAddrOf16 hl)) [Reg8 b] 1 8))
  , (0x71, Left (Inst LD (Just (DAddrOf16 hl)) [Reg8 c] 1 8))
  , (0x72, Left (Inst LD (Just (DAddrOf16 hl)) [Reg8 d] 1 8))
  , (0x73, Left (Inst LD (Just (DAddrOf16 hl)) [Reg8 e] 1 8))
  , (0x74, Left (Inst LD (Just (DAddrOf16 hl)) [Reg8 h] 1 8))
  , (0x75, Left (Inst LD (Just (DAddrOf16 hl)) [Reg8 l] 1 8))
  , (0x36, Left (Inst LD (Just (DAddrOf16 hl)) [Imm1] 2 12))

  -- LD A, n
  , (0x0A, Left (Inst LD (Just (DReg8 a)) [Reg16 bc] 1 8))
  , (0x1A, Left (Inst LD (Just (DReg8 a)) [Reg16 de] 1 8))
  , (0x7E, Left (Inst LD (Just (DReg8 a)) [Reg16 hl] 1 8))
  , (0xFA, Left (Inst LD (Just (DReg8 a)) [Imm2] 3 16))
  , (0x3E, Left (Inst LD (Just (DReg8 a)) [Imm1] 2 8))

  -- LD n, A
  , (0x7F, Left (Inst LD (Just (DReg8 a)) [Reg8 a] 1 4))
  , (0x47, Left (Inst LD (Just (DReg8 b)) [Reg8 a] 1 4))
  , (0x4F, Left (Inst LD (Just (DReg8 c)) [Reg8 a] 1 4))
  , (0x57, Left (Inst LD (Just (DReg8 d)) [Reg8 a] 1 4))
  , (0x5F, Left (Inst LD (Just (DReg8 e)) [Reg8 a] 1 4))
  , (0x67, Left (Inst LD (Just (DReg8 h)) [Reg8 a] 1 4))
  , (0x6F, Left (Inst LD (Just (DReg8 l)) [Reg8 a] 1 4))
  , (0x02, Left (Inst LD (Just (DAddrOf16 bc)) [Reg8 a] 1 8))
  , (0x12, Left (Inst LD (Just (DAddrOf16 de)) [Reg8 a] 1 8))
  , (0x77, Left (Inst LD (Just (DAddrOf16 hl)) [Reg8 a] 1 8))
  , (0xEA, Left (Inst LD (Just DAddrOfImm16) [Reg8 a] 3 16))

  -- LD A, (C)
  , (0xF2, Left (Inst LD (Just (DReg8 a)) [AddrOfC] 1 16))

  -- LD (C), A
  , (0xE2, Left (Inst LD (Just DAddrOfC) [Reg8 a] 3 16))

  -- LDH (n), A
  , (0xE0, Left (Inst LD (Just DAddrOfImmH) [Reg8 a] 2 12))

  -- 8-bit ALU
  , (0x87, Left (Inst ADD Nothing [Reg8 a] 1 4))
  , (0x80, Left (Inst ADD Nothing [Reg8 b] 1 4))
  , (0x81, Left (Inst ADD Nothing [Reg8 c] 1 4))
  , (0x82, Left (Inst ADD Nothing [Reg8 d] 1 4))
  , (0x83, Left (Inst ADD Nothing [Reg8 e] 1 4))
  , (0x84, Left (Inst ADD Nothing [Reg8 h] 1 4))
  , (0x85, Left (Inst ADD Nothing [Reg8 l] 1 4))
  , (0x86, Left (Inst ADD Nothing [AddrOf hl] 1 8))
  , (0xC6, Left (Inst ADD Nothing [Imm1] 2 8))

  , (0x8F, Left (Inst ADC Nothing [Reg8 a] 1 4))
  , (0x88, Left (Inst ADC Nothing [Reg8 b] 1 4))
  , (0x89, Left (Inst ADC Nothing [Reg8 c] 1 4))
  , (0x8A, Left (Inst ADC Nothing [Reg8 d] 1 4))
  , (0x8B, Left (Inst ADC Nothing [Reg8 e] 1 4))
  , (0x8C, Left (Inst ADC Nothing [Reg8 h] 1 4))
  , (0x8D, Left (Inst ADC Nothing [Reg8 l] 1 4))
  , (0x8E, Left (Inst ADC Nothing [AddrOf hl] 1 8))
  , (0xCE, Left (Inst ADC Nothing [Imm1] 2 8))

  , (0x97, Left (Inst SUB Nothing [Reg8 a] 1 4))
  , (0x90, Left (Inst SUB Nothing [Reg8 b] 1 4))
  , (0x91, Left (Inst SUB Nothing [Reg8 c] 1 4))
  , (0x92, Left (Inst SUB Nothing [Reg8 d] 1 4))
  , (0x93, Left (Inst SUB Nothing [Reg8 e] 1 4))
  , (0x94, Left (Inst SUB Nothing [Reg8 h] 1 4))
  , (0x95, Left (Inst SUB Nothing [Reg8 l] 1 4))
  , (0x96, Left (Inst SUB Nothing [AddrOf hl] 1 8))
  , (0xD6, Left (Inst SUB Nothing [Imm1] 2 8))

  , (0x9F, Left (Inst SBC Nothing [Reg8 a] 1 4))
  , (0x98, Left (Inst SBC Nothing [Reg8 b] 1 4))
  , (0x99, Left (Inst SBC Nothing [Reg8 c] 1 4))
  , (0x9A, Left (Inst SBC Nothing [Reg8 d] 1 4))
  , (0x9B, Left (Inst SBC Nothing [Reg8 e] 1 4))
  , (0x9C, Left (Inst SBC Nothing [Reg8 h] 1 4))
  , (0x9D, Left (Inst SBC Nothing [Reg8 l] 1 4))
  , (0x9E, Left (Inst SBC Nothing [AddrOf hl] 1 8))
  , (0xDE, Left (Inst SBC Nothing [Imm1] 2 8))

  , (0xA7, Left (Inst AND Nothing [Reg8 a] 1 4))
  , (0xA0, Left (Inst AND Nothing [Reg8 b] 1 4))
  , (0xA1, Left (Inst AND Nothing [Reg8 c] 1 4))
  , (0xA2, Left (Inst AND Nothing [Reg8 d] 1 4))
  , (0xA3, Left (Inst AND Nothing [Reg8 e] 1 4))
  , (0xA4, Left (Inst AND Nothing [Reg8 h] 1 4))
  , (0xA5, Left (Inst AND Nothing [Reg8 l] 1 4))
  , (0xA6, Left (Inst AND Nothing [AddrOf hl] 1 8))
  , (0xE6, Left (Inst AND Nothing [Imm1] 2 8))

  , (0xAF, Left (Inst XOR Nothing [Reg8 a] 1 4))
  , (0xA8, Left (Inst XOR Nothing [Reg8 b] 1 4))
  , (0xA9, Left (Inst XOR Nothing [Reg8 c] 1 4))
  , (0xAA, Left (Inst XOR Nothing [Reg8 d] 1 4))
  , (0xAB, Left (Inst XOR Nothing [Reg8 e] 1 4))
  , (0xAC, Left (Inst XOR Nothing [Reg8 h] 1 4))
  , (0xAD, Left (Inst XOR Nothing [Reg8 l] 1 4))
  , (0xAE, Left (Inst XOR Nothing [AddrOf hl] 1 8))
  , (0xEE, Left (Inst XOR Nothing [Imm1] 2 8))

  , (0xB7, Left (Inst OR Nothing [Reg8 a] 1 4))
  , (0xB0, Left (Inst OR Nothing [Reg8 b] 1 4))
  , (0xB1, Left (Inst OR Nothing [Reg8 c] 1 4))
  , (0xB2, Left (Inst OR Nothing [Reg8 d] 1 4))
  , (0xB3, Left (Inst OR Nothing [Reg8 e] 1 4))
  , (0xB4, Left (Inst OR Nothing [Reg8 h] 1 4))
  , (0xB5, Left (Inst OR Nothing [Reg8 l] 1 4))
  , (0xB6, Left (Inst OR Nothing [AddrOf hl] 1 8))
  , (0xF6, Left (Inst OR Nothing [Imm1] 2 8))

  , (0xBF, Left (Inst CP Nothing [Reg8 a] 1 4))
  , (0xB8, Left (Inst CP Nothing [Reg8 b] 1 4))
  , (0xB9, Left (Inst CP Nothing [Reg8 c] 1 4))
  , (0xBA, Left (Inst CP Nothing [Reg8 d] 1 4))
  , (0xBB, Left (Inst CP Nothing [Reg8 e] 1 4))
  , (0xBC, Left (Inst CP Nothing [Reg8 h] 1 4))
  , (0xBD, Left (Inst CP Nothing [Reg8 l] 1 4))
  , (0xBE, Left (Inst CP Nothing [AddrOf hl] 1 8))
  , (0xFE, Left (Inst CP Nothing [Imm1] 2 8))

  , (0x3C, Left (Inst INC Nothing [Reg8 a] 1 4))
  , (0x04, Left (Inst INC Nothing [Reg8 b] 1 4))
  , (0x0C, Left (Inst INC Nothing [Reg8 c] 1 4))
  , (0x14, Left (Inst INC Nothing [Reg8 d] 1 4))
  , (0x1C, Left (Inst INC Nothing [Reg8 e] 1 4))
  , (0x24, Left (Inst INC Nothing [Reg8 h] 1 4))
  , (0x2C, Left (Inst INC Nothing [Reg8 l] 1 4))
  , (0x34, Left (Inst INC Nothing [AddrOf hl] 1 12))

  , (0x3D, Left (Inst DEC Nothing [Reg8 a] 1 4))
  , (0x05, Left (Inst DEC Nothing [Reg8 b] 1 4))
  , (0x0D, Left (Inst DEC Nothing [Reg8 c] 1 4))
  , (0x15, Left (Inst DEC Nothing [Reg8 d] 1 4))
  , (0x1D, Left (Inst DEC Nothing [Reg8 e] 1 4))
  , (0x25, Left (Inst DEC Nothing [Reg8 h] 1 4))
  , (0x2D, Left (Inst DEC Nothing [Reg8 l] 1 4))
  , (0x35, Left (Inst DEC Nothing [AddrOf hl] 1 12))

  -- 16-bit arith
  , (0x09, Left (Inst ADD16 (Just (DReg16 hl)) [Reg16 bc] 1 8))
  , (0x19, Left (Inst ADD16 (Just (DReg16 hl)) [Reg16 de] 1 8))
  , (0x29, Left (Inst ADD16 (Just (DReg16 hl)) [Reg16 hl] 1 8))
  , (0x39, Left (Inst ADD16 (Just (DReg16 hl)) [Reg16 sp] 1 8))
  , (0xE8, Left (Inst ADD16 (Just (DReg16 sp)) [Imm1] 1 16))

  , (0x03, Left (Inst INC Nothing [Reg16 bc] 1 8))
  , (0x13, Left (Inst INC Nothing [Reg16 de] 1 8))
  , (0x23, Left (Inst INC Nothing [Reg16 hl] 1 8))
  , (0x33, Left (Inst INC Nothing [Reg16 sp] 1 8))

  , (0x0B, Left (Inst DEC Nothing [Reg16 bc] 1 8))
  , (0x1B, Left (Inst DEC Nothing [Reg16 de] 1 8))
  , (0x2B, Left (Inst DEC Nothing [Reg16 hl] 1 8))
  , (0x3B, Left (Inst DEC Nothing [Reg16 sp] 1 8))

  -- misc
  , (0x27, Left (Inst DAA Nothing [] 1 4))
  , (0x2F, Left (Inst CPL Nothing [] 1 4))
  , (0x3F, Left (Inst CCF Nothing [] 1 4))
  , (0x37, Left (Inst SCF Nothing [] 1 4))
  , (0x00, Left (Inst NOP Nothing [] 1 4))
  , (0x76, Left (Inst HALT Nothing [] 1 4))
  , (0xC3, Left (Inst JP Nothing [Imm2] 3 12))
  , (0x10, Left (Inst STOP Nothing [Imm1] 2 12))
  , (0xF3, Left (Inst DI Nothing [] 1 12))
  , (0xFB, Left (Inst EI Nothing [] 1 12))


  -- rotates and shifts

  , (0xCB, Right extendedInstrs)
  ]

extendedInstrs :: Map Word8 Inst
extendedInstrs = M.fromList
  [ (0xC3, Inst SET undefined undefined undefined undefined)

  -- misc
  , (0x37, Inst SWAP Nothing [Reg8 a] 1 8)
  , (0x30, Inst SWAP Nothing [Reg8 b] 1 8)
  , (0x31, Inst SWAP Nothing [Reg8 c] 1 8)
  , (0x32, Inst SWAP Nothing [Reg8 d] 1 8)
  , (0x33, Inst SWAP Nothing [Reg8 e] 1 8)
  , (0x34, Inst SWAP Nothing [Reg8 h] 1 8)
  , (0x35, Inst SWAP Nothing [Reg8 l] 1 8)
  , (0x36, Inst SWAP Nothing [AddrOf hl] 1 16)
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

fetchSource :: MonadCPU m => Source -> m [Word8]
fetchSource Imm1 = pure <$> byteAtPCOffset 1
fetchSource Imm2 = (\a b -> [a,b]) <$> byteAtPCOffset 1 <*> byteAtPCOffset 2
fetchSource (Flag f) = pure []
fetchSource (Reg8 r) = pure <$> getRegister r
fetchSource (Reg16 r) = split16' <$> getRegister16 r
fetchSource (AddrOf r) = pure <$> (lookupAddr =<< getRegister16 r)
fetchSource AddrOfC = pure <$> (lookupAddr =<< ((+ 0xFF00) . fromIntegral) <$> getRegister c)

getRegister :: MonadCPU m => Lens' Registers Word8 -> m Word8
getRegister r = get <&> (^. registers.r)

getRegister16 :: MonadCPU m => Lens' Registers Word16 -> m Word16
getRegister16 r = get <&> (^. registers.r)

fetchSources :: MonadCPU m => [Source] -> m [Word8]
fetchSources ps = concat <$> mapM fetchSource ps

-- | Execute a CPU instruction
exec :: MonadCPU m => Word16 -> Inst -> m ()
exec pc_ (Inst opcode dest sources len time) = do
  modify (registers.pc %~ (+len))
  args <- fetchSources sources
  execInst opcode dest sources args
  modify (clocktime %~ (plusOpTime time))

-- | Dispatch execution of each CPU instruction
execInst :: MonadCPU m => Op -> Maybe Dest -> [Source] -> [Word8] -> m ()
execInst NOP Nothing [] [] = pure ()
execInst JP Nothing [Imm2] [l, u] = jp l u
execInst OR Nothing [r] [arg] = aluOp (.|.) arg
execInst ADD Nothing [AddrOf r] [l, u] = aluOp (+) =<< lookupAddr (twoBytes l u)
execInst ADD Nothing [Reg8 reg] [arg] = aluOp (+) arg
execInst LD (Just dest) sources args = ld dest sources args
execInst op dest addr args = throwError (CPUEInstNotImplemented op addr args)

lookupAddr :: MonadCPU m => Word16 -> m Word8
lookupAddr addr = do
  st <- get
  throwWhenNothing (CPUEMemoryLookupFailed addr) (st ^? memory addr)

jp :: MonadCPU m => Word8 -> Word8 -> m ()
jp l u = modify (set (registers.pc) (twoBytes l u))

-- TODO: set flags
aluOp :: MonadCPU m => (Word8 -> Word8 -> Word8) -> Word8 -> m ()
aluOp f arg = do
  acc <- getRegister a
  modify (set (registers.a) (f acc arg))

ld DAddrOfC _ [imm, source] = ldImmH imm source
ld (DAddrOf16 r) _ [source] = ldAddrOf16 r source
ld (DReg8 r) _ [source] = ldReg r source
ld DAddrOfImmH _ [imm, source] = ldImmH imm source
ld DAddrOfImm16 _ [l, h, source] = ldImm16 (twoBytes l h) source
ld _ _ _ = throwError CPUEBadLdInst

ldReg :: MonadCPU m => Reg8 -> Word8 -> m ()
ldReg r v = modify (set (registers.r) v)

ldAddrOf16 :: MonadCPU m => Reg16 -> Word8 -> m ()
ldAddrOf16 r v = do
  addr <- getRegister16 r
  modify (set (memory addr) v)

ldAddrOfC :: MonadCPU m => Word8 -> m ()
ldAddrOfC v = do
  addr <- (0xFF00 +) . fromIntegral <$> getRegister c
  modify (set (memory addr) v)

ldImmH :: MonadCPU m => Word8 -> Word8 -> m ()
ldImmH imm v = modify (set (memory (0xFF00 + fromIntegral imm)) v)

ldImm16 :: MonadCPU m => Word16 -> Word8 -> m ()
ldImm16 imm v = modify (set (memory imm) v)
