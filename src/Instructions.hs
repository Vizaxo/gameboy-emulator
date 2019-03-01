{-# LANGUAGE UndecidableInstances #-}
module Instructions (Inst(..), Opcode(..), instructions, SizeTy, DispatchSizeTy(..), Size(..), Param(..), RegLens(..), Op(..), Register(..), opLen, Cond(..)) where

import Control.Lens hiding (sets)
import Data.Bits hiding (xor)
import Data.Ix
import Data.Kind
import Data.Map (fromList, Map)
import Data.Word
import Numeric.Natural

import CPUState

-- TODO: easy type-safe version of this?
type Word4 = Word8
newtype Opcode = Opcode Word8
  deriving (Show, Num, Eq, Ord, Enum)

-- Treat opcodes as a 16x16 grid of nibbles
rangeOc :: (Opcode, Opcode) -> [Opcode]
rangeOc (Opcode tl, Opcode br)
  = uncurry mkOpcode <$> range ((highNib tl, lowNib tl), (highNib br, lowNib br))

mkOpcode :: Word4 -> Word4 -> Opcode
mkOpcode h l = Opcode (l .|. shiftL h 4)

highNib, lowNib :: Word8 -> Word4
highNib = flip shiftR 4
lowNib = (.&. 0x0F)

data Size = S8 | S16
  deriving Show

class ParamLen size where
  paramLen :: Param size -> Word16
instance (ParamLen S8) where
  paramLen Imm = 1
  paramLen (AddrOfH p) = paramLen p
  paramLen (AddrOf p) = paramLen p
  paramLen _ = 0
instance (ParamLen S16) where
  paramLen Imm = 2
  paramLen (RegPlus r p) = paramLen p
  paramLen _ = 0

class RegLens size where
  regLens :: (Register size) -> Lens' Registers (SizeTy size)
instance RegLens S8 where
  regLens A = a
  regLens B = b
  regLens C = c
  regLens D = d
  regLens E = e
  regLens H = h
  regLens L = l
instance RegLens S16 where
  regLens AF = af
  regLens BC = bc
  regLens DE = de
  regLens HL = hl
  regLens SP = sp

class DispatchSizeTy (size :: Size) where
  dispatchSize :: (f Word8) -> (f Word16) -> f (SizeTy size)
instance DispatchSizeTy S8 where
  dispatchSize f g = f
instance DispatchSizeTy S16 where
  dispatchSize f g = g

data Cond = CondNZ | CondZ | CondNC | CondC
  deriving Show

type SizeConstraint size
  = (Show (SizeTy size), Integral (SizeTy size)
    , ParamLen size, RegLens size, DispatchSizeTy size
    , Ord (SizeTy size))

data Op where
  Add  :: SizeConstraint size => Register size -> Param size -> Op
  Adc  :: SizeConstraint size => Register size -> Param size -> Op
  Sub  :: Param S8 -> Op
  Sbc  :: Param S8 -> Op
  Cp   :: Param S8 -> Op
  And  :: Param S8 -> Op
  Or   :: Param S8 -> Op
  Xor  :: Param S8 -> Op
  Nop  :: Op
  Rst  :: Word8 -> Op
  Jp   :: Maybe Cond -> Param S16 -> Op
  Jr   :: Maybe Cond -> Param S8 -> Op
  Ld   :: SizeConstraint size => Param size -> Param size -> Op
  Push :: Param S16 -> Op
  Pop  :: Register S16 -> Op
  Inc  :: SizeConstraint size => Param size -> Op
  Dec  :: SizeConstraint size => Param size -> Op
  Rrca :: Op
  Rlca :: Op
  Di   :: Op
  Ei   :: Op
  Call :: Maybe Cond -> Param S16 -> Op
  Ret  :: Maybe Cond -> Op
  Reti :: Op
  Daa  :: Op
  Cpl  :: Op
  Ccf  :: Op
  Scf  :: Op
  Set  :: Int -> Param S8 -> Op
  Res  :: Int -> Param S8 -> Op
  Bit  :: Int -> Param S8 -> Op
  Swap :: Param S8 -> Op
  Sla  :: Param S8 -> Op
  Sra  :: Param S8 -> Op
  Srl  :: Param S8 -> Op
  Stop :: Op
deriving instance Show Op

type family SizeTy (s :: Size) = (out :: Type) | out -> s where
  SizeTy S8 = Word8
  SizeTy S16 = Word16

data Register size where
  A , B , C , D , E , H , L :: Register S8
  AF , BC , DE , HL , SP :: Register S16
deriving instance Show (Register size)

data Param size where
  Reg :: Register size -> Param size
  AddrOf :: Param S16 -> Param S8
  AddrOfH :: Param S8 -> Param S8
  RegPlus :: Register S16 -> Param S8 -> Param S16
  Imm :: Param size
  PostInc :: Register size -> Param size
  PostDec :: Register size -> Param size
deriving instance Show (SizeTy size) => Show (Param size)

class ParamSize size where
  paramSize :: Param size -> Int
instance ParamSize S8 where
  paramSize p = 1
instance ParamSize S16 where
  paramSize p = 2

data Inst = Inst
  { _op :: Op
  , _cycles :: Natural --TODO: derive cycle lengths from memory accesses
  }
  deriving Show
makeLenses ''Inst

opLen :: Op -> Word16
opLen (Add dest src) = 1 + paramLen src
opLen (Adc dest src) = 1 + paramLen src
opLen (Sub src) = 1 + paramLen src
opLen (Sbc src) = 1 + paramLen src
opLen (Cp src) = 1 + paramLen src
opLen (And src) = 1 + paramLen src
opLen (Or src) = 1 + paramLen src
opLen (Xor src) = 1 + paramLen src
opLen (Jp cond dest) = 1 + paramLen dest
opLen (Jr cond dest) = 2
opLen Nop = 1
opLen (Rst p) = 1
opLen (Inc p) = 1 + paramLen p
opLen (Dec p) = 1 + paramLen p
opLen (Ld dest src) = 1 + paramLen dest + paramLen src
opLen (Push p) = 1 + paramLen p
opLen (Pop r) = 1
opLen Rrca = 1
opLen Rlca = 1
opLen Di = 1
opLen Ei = 1
opLen (Call cond dest) = 1 + paramLen dest
opLen (Ret cond) = 1
opLen Reti = 1
opLen Daa = 1
opLen Cpl = 1
opLen Ccf = 1
opLen Scf = 1
opLen (Set _ p) = 2 + paramLen p
opLen (Res _ p) = 2 + paramLen p
opLen (Bit _ p) = 2 + paramLen p
opLen (Swap p) = 2 + paramLen p
opLen (Sla p) = 2 + paramLen p
opLen (Sra p) = 2 + paramLen p
opLen (Srl p) = 2 + paramLen p
opLen Stop = 2

opcodeRange :: (a -> Op) -> [[(a, Natural)]] -> (Opcode, Opcode) -> [(Opcode, Inst)]
opcodeRange op ps rng = zip (rangeOc rng) ((\(p, c) -> Inst (op p) c) <$> (concat ps))

regs8 :: [Param S8]
regs8 = [Reg B, Reg C, Reg D, Reg E, Reg H, Reg L, AddrOf (Reg HL), Reg A]

aluParams :: [(Param S8, Natural)]
aluParams = zip regs8 (replicate 6 4 <> [8] <> [4])

regs16 :: [(Register S16)]
regs16 = [BC, DE, HL, SP]

addA :: [(Opcode, Inst)]
addA = opcodeRange (Add A)
  [aluParams]
  (Opcode 0x80, Opcode 0x87)

adcA :: [(Opcode, Inst)]
adcA = opcodeRange (Adc A)
  [aluParams]
  (Opcode 0x88, Opcode 0x8F)

add16 :: [(Opcode, Inst)]
add16 = zip
  (rangeOc (0x09, 0x39))
  ((\r -> Inst (Add HL (Reg r)) 8) <$> regs16)

sub :: [(Opcode, Inst)]
sub = (0xD6, Inst (Cp Imm) 8) : opcodeRange Sub
  [aluParams]
  (Opcode 0x90, Opcode 0x97)

sbc :: [(Opcode, Inst)]
sbc = opcodeRange Sbc
  [aluParams]
  (Opcode 0x98, Opcode 0x9F)

aluImm :: [(Opcode, Inst)]
aluImm = zip [0xC6,0xCE..]
  ((\i -> Inst (i Imm) 8) <$> [Add A, Adc A, Sub, Sbc, And, Xor, Or, Cp])

cp :: [(Opcode, Inst)]
cp = (0xFE, Inst (Cp Imm) 8) : opcodeRange Cp
  [aluParams]
  (Opcode 0xB8, Opcode 0xBF)

ands :: [(Opcode, Inst)]
ands = (0xE6, Inst (And Imm) 8) :
  opcodeRange And
  [aluParams]
  (0xA0, 0xA7)

ors :: [(Opcode, Inst)]
ors = (0xF6, Inst (Or Imm) 8) :
  opcodeRange Or
  [aluParams]
  (0xB0, 0xB7)

xors :: [(Opcode, Inst)]
xors = (0xEE, Inst (Xor Imm) 8) :
  opcodeRange Xor
  [aluParams]
  (0xA8, 0xAF)

misc :: [(Opcode, Inst)]
misc = [(0x00, Inst Nop 4)]

jump :: [(Opcode, Inst)]
jump =
  [ (0xC3, Inst (Jp Nothing Imm) 12)
  , (0xC2, Inst (Jp (Just CondNZ) Imm) 12)
  , (0xCA, Inst (Jp (Just CondZ) Imm) 12)
  , (0xD2, Inst (Jp (Just CondNC) Imm) 12)
  , (0xDA, Inst (Jp (Just CondC) Imm) 12)
  , (0xE9, Inst (Jp Nothing (Reg HL)) 4)
  ]

jrcc :: [(Opcode, Inst)]
jrcc = zip
  [0x20,0x28..]
  ((\cond -> Inst (Jr (Just cond) Imm) 8) <$> [CondNZ, CondZ, CondNC, CondC])
  <> [(0x18, Inst (Jr Nothing Imm) 8)]

rst :: [(Opcode, Inst)]
rst = zip
  [0xC7, 0xCF, 0xD7, 0xDF, 0xE7, 0xEF, 0xF7, 0xFF]
  ((\p -> Inst (Rst p) 32) <$> [0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38])

ld16 :: [(Opcode, Inst)]
ld16 = zip
  (rangeOc (0x01,0x31))
  ((\r -> Inst (Ld (Reg r) Imm) 12) <$> regs16)

ldAn :: [(Opcode, Inst)]
ldAn = over (mapped._2) (\(src,c) -> Inst (Ld (Reg A) (AddrOf src)) c)
  [ (0x0A, (Reg BC, 8))
  , (0x1A, (Reg DE, 8))
  , (0xFA, (Imm, 16))
  ]

ldnA :: [(Opcode, Inst)]
ldnA = over (mapped._2) (\(src,c) -> Inst (Ld (AddrOf src) (Reg A)) c)
  [ (0x02, (Reg BC, 8))
  , (0x12, (Reg DE, 8))
  , (0xEA, (Imm, 16))
  ]
ldrn :: [(Opcode, Inst)]
ldrn = zip
  [0x06,0x0E..]
  (mkInst <$> aluParams)
  where
    mkInst (p, c) = Inst (Ld p Imm) (c+4)

ldr1r2 :: [(Opcode, Inst)] --TODO: replace LD (hl) (hl) with HALT
ldr1r2 = zip
  (rangeOc (0x40, 0x7F))
  ((\r1 (r2,c) -> Inst (Ld r1 r2) c) <$> (fst <$> aluParams) <*> aluParams)

lddi :: [(Opcode, Inst)]
lddi =
  [ (0x22, Inst (Ld (AddrOf (PostInc HL)) (Reg A)) 8)
  , (0x32, Inst (Ld (AddrOf (PostDec HL)) (Reg A)) 8)
  , (0x2A, Inst (Ld (Reg A) (AddrOf (PostInc HL))) 8)
  , (0x3A, Inst (Ld (Reg A) (AddrOf (PostDec HL))) 8)
  ]

ldh :: [(Opcode, Inst)]
ldh =
  [ (0xE0, Inst (Ld (AddrOfH Imm) (Reg A)) 12)
  , (0xF0, Inst (Ld (Reg A) (AddrOfH Imm)) 12)
  , (0xE2, Inst (Ld (AddrOfH (Reg C)) (Reg A)) 8)
  , (0xF2, Inst (Ld (Reg A) (AddrOfH (Reg C))) 8)
  , (0xF8, Inst (Ld (Reg HL) (RegPlus SP Imm)) 12)
  ]

push :: [(Opcode, Inst)]
push = zip
  (rangeOc (0xC5, 0xF5))
  ((\r -> Inst (Push (Reg r)) 16) <$> regs16)

pop :: [(Opcode, Inst)]
pop = zip
  (rangeOc (0xC1, 0xF1))
  ((\r -> Inst (Pop r) 12) <$> regs16)

inc8 :: [(Opcode, Inst)]
inc8 = zip
  [0x04,0x0C..]
  (mkInst <$> zip regs8 (replicate 6 4 <> [12] <> [4]))
  where
    mkInst (p, c) = Inst (Inc p) c

inc16 :: [(Opcode, Inst)]
inc16 = zip
  (rangeOc (0x03, 0x33))
  ((\r -> Inst (Inc (Reg r)) 8) <$> regs16)

dec8 :: [(Opcode, Inst)]
dec8 = zip
  [0x05,0x0D..]
  (mkInst <$> zip regs8 (replicate 6 4 <> [12] <> [4]))
  where
    mkInst (p, c) = Inst (Dec p) c

dec16 :: [(Opcode, Inst)]
dec16 = zip
  (rangeOc (0x0B, 0x3B))
  ((\r -> Inst (Dec (Reg r)) 8) <$> regs16)

rotates :: [(Opcode, Inst)]
rotates =
  [ (0x0F, Inst Rrca 4)
  , (0x07, Inst Rlca 4)
  ]

interrupts :: [(Opcode, Inst)]
interrupts = [(0xF3, Inst Di 4), (0xFB, Inst Ei 4)]

call :: [(Opcode, Inst)]
call = over (mapped._2) (\cond -> Inst (Call cond Imm) 12)
  [ (0xCD, Nothing)
  , (0xC4, Just CondNZ)
  , (0xCC, Just CondZ)
  , (0xD4, Just CondNC)
  , (0xDC, Just CondC)
  ]

ret :: [(Opcode, Inst)]
ret = over (mapped._2) (\cond -> Inst (Ret cond) 8)
  [ (0xC9, Nothing)
  , (0xC0, Just CondNZ)
  , (0xC8, Just CondZ)
  , (0xD0, Just CondNC)
  , (0xD8, Just CondC)
  ]
  <> [(0xD9, Inst Reti 16)]

daa :: [(Opcode, Inst)]
daa = [(0x27, Inst Daa 4)]

cpl :: [(Opcode, Inst)]
cpl = [(0x2F, Inst Cpl 4)]

cf :: [(Opcode, Inst)]
cf =
  [ (0x3F, Inst Ccf 4)
  , (0x37, Inst Scf 4)
  ]

stop :: [(Opcode, Inst)]
stop = [(0x10, Inst Stop 4)]

cbParams = zip regs8 (replicate 6 8 <> [16] <> [8])

sets :: [(Opcode, Inst)]
sets = zip
  (rangeOc (0xC0, 0xFF))
  (cycle (zipWith (\b (r, c) -> Inst (Set b r) c) [0..7] cbParams))

resets :: [(Opcode, Inst)]
resets = zip
  (rangeOc (0x80, 0xBF))
  (cycle (zipWith (\b (r, c) -> Inst (Res b r) c) [0..7] cbParams))

bits :: [(Opcode, Inst)]
bits = zip
  (rangeOc (0x40, 0x7F))
  (cycle (zipWith (\b (r, c) -> Inst (Bit b r) c) [0..7] cbParams))

swaps :: [(Opcode, Inst)]
swaps = zip
  (rangeOc (0x30, 0x37))
  (zipWith (\r c -> Inst (Swap r) c) regs8 (replicate 6 8 <> [16,8]))

shifts :: [(Opcode, Inst)]
shifts = mkShift (0x20,0x27) Sla <> mkShift (0x28,0x2F) Sra <> mkShift (0x38,0x3F) Srl
  where
    mkShift range op = zip (rangeOc range)
      (zipWith (\r c -> Inst (op r) c) regs8 (replicate 6 8 <> [16,8]))

cbPrefix :: Map Opcode Inst
cbPrefix = fromList $ sets <> resets <> swaps <> shifts <> bits

instructions :: Map Opcode (Either Inst (Map Opcode Inst))
instructions = fromList $
  (0xCB, Right cbPrefix):
  (over (mapped._2) Left $
   addA <> adcA <> add16 <> sub <> sbc <> aluImm <> cp <> misc <> jump <> jrcc <> ands <> ors <> xors <> rst
   <> ld16 <> ldAn <> ldnA <> ldrn <> ldr1r2 <> lddi <> ldh <> push <> pop
   <> inc8 <> inc16 <> dec8 <> dec16 <> rotates
   <> interrupts <> call <> ret <> daa <> cpl <> cf <> stop)
