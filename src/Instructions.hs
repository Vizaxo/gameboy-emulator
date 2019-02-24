{-# LANGUAGE UndecidableInstances #-}
module Instructions (Inst(..), Opcode(..), instructions, SizeTy, DispatchSizeTy(..), Size(..), Param(..), RegLens(..), Op(..), Register(..), opLen, Flag(..)) where

import Control.Lens
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
  paramLen _ = 0
instance (ParamLen S16) where
  paramLen Imm = 2
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

data Flag = FlagNZ | FlagZ | FlagNC | FlagC
  deriving Show

type SizeConstraint size
  = (Show (SizeTy size), Num (SizeTy size)
    , ParamLen size, RegLens size, DispatchSizeTy size)

data Op where
  Add  ::  SizeConstraint size => Register size -> Param size -> Op
  Sub  :: Param S8 -> Op
  Xor  :: Param S8 -> Op
  Nop  :: Op
  Rst  :: Word8 -> Op
  Jp   :: Maybe Flag -> Param S16 -> Op
  Jr   :: Maybe Flag -> Param S8 -> Op
  Ld   :: SizeConstraint size => Param size -> Param size -> Op
  Inc  :: SizeConstraint size => Param size -> Op
  Dec  :: SizeConstraint size => Param size -> Op
  Extended :: Op -> Op
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
opLen (Sub src) = 1 + paramLen src
opLen (Xor src) = 1 + paramLen src
opLen (Jp cond dest) = 1 + paramLen dest
opLen (Jr cond dest) = 2
opLen (Extended op) = 1 + opLen op
opLen Nop = 1
opLen (Rst p) = 1
opLen (Inc p) = 1 + paramLen p
opLen (Dec p) = 1 + paramLen p
opLen (Ld dest src) = 1 + paramLen dest + paramLen src

opcodeRange :: (a -> Op) -> [[(a, Natural)]] -> (Opcode, Opcode) -> [(Opcode, Inst)]
opcodeRange op ps rng = zip (rangeOc rng) ((\(p, c) -> Inst (op p) c) <$> (concat ps))

aluParams' :: [Param S8]
aluParams' = [Reg B, Reg C, Reg D, Reg E, Reg H, Reg L, AddrOf (Reg HL), Reg A]

aluParams :: [(Param S8, Natural)]
aluParams = zip aluParams' (replicate 6 4 <> [8] <> [4])

addA :: [(Opcode, Inst)]
addA = opcodeRange (Add A)
  [aluParams]
  (Opcode 0x80, Opcode 0x87)

sub :: [(Opcode, Inst)]
sub = opcodeRange Sub
  [aluParams]
  (Opcode 0x90, Opcode 0x97)

xor :: [(Opcode, Inst)]
xor = (0xEE, Inst (Xor Imm) 8) :
  opcodeRange Xor
  [aluParams]
  (0xA8, 0xAF)

misc :: [(Opcode, Inst)]
misc = [(0x00, Inst Nop 4)]

jump :: [(Opcode, Inst)]
jump = [(0xC3, Inst (Jp Nothing Imm) 12)]

jrcc :: [(Opcode, Inst)]
jrcc = zip
  [0x20,0x28..]
  ((\cond -> Inst (Jr (Just cond) Imm) 8) <$> [FlagNZ, FlagZ, FlagNC, FlagC])

rst :: [(Opcode, Inst)]
rst = zip
  [0xC7, 0xCF, 0xD7, 0xDF, 0xE7, 0xEF, 0xF7, 0xFF]
  ((\p -> Inst (Rst p) 32) <$> [0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38])

ld16 :: [(Opcode, Inst)]
ld16 = zip
  (rangeOc (0x01,0x31))
  ((\r -> Inst (Ld (Reg r) Imm) 12) <$> [BC, DE, HL, SP])

ldrn :: [(Opcode, Inst)]
ldrn = zip
  [0x06,0x0E..]
  (mkInst <$> aluParams)
  where
    mkInst (p, c) = Inst (Ld p Imm) (c+4)

ldr1r2 :: [(Opcode, Inst)]
ldr1r2 = zip
  (rangeOc (0x40, 0x7F))
  ((\r1 (r2,c) -> Inst (Ld r1 r2) c) <$> (fst <$> aluParams) <*> aluParams)

lddi :: [(Opcode, Inst)]
lddi =
  [ (0x22, Inst (Ld (AddrOf (PostInc HL)) (Reg A)) 8)
  , (0x32, Inst (Ld (AddrOf (PostDec HL)) (Reg A)) 8)
  ]

inc8 :: [(Opcode, Inst)]
inc8 = zip
  [0x04,0x0C..]
  (mkInst <$> zip aluParams' (replicate 6 4 <> [12] <> [4]))
  where
    mkInst (p, c) = Inst (Inc p) c

dec8 :: [(Opcode, Inst)]
dec8 = zip
  [0x05,0x0D..]
  (mkInst <$> zip aluParams' (replicate 6 4 <> [12] <> [4]))
  where
    mkInst (p, c) = Inst (Dec p) c

instructions :: Map Opcode Inst
instructions = fromList
  (addA <> sub <> misc <> jump <> jrcc <> xor <> rst
   <> ld16 <> ldrn <> ldr1r2 <> lddi <> inc8 <> dec8)
