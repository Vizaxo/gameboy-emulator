{-# LANGUAGE UndecidableInstances #-}
module Instructions where

import Control.Lens
import Data.Bits
import Data.Ix
import Data.Kind
import Data.Map (fromList, Map)
import Data.Word
import Numeric.Natural

import CPUState

-- TODO: easy type-safe version of this?
type Word4 = Word8
newtype Opcode = Opcode Word8
  deriving (Show, Num, Eq, Ord)

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

data AddDest size where
  ADA :: AddDest S8
  ADHL :: AddDest S16
deriving instance Show (AddDest size)

class ParamLen size where
  paramLen :: Param size -> Word16
instance (ParamLen S8) where
  paramLen Imm = 2
  paramLen _ = 1
instance (ParamLen S16) where
  paramLen Imm = 3
  paramLen _ = 1

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

data Op where
  Add :: forall (size :: Size). (Show (SizeTy size), Num (SizeTy size), ParamLen size, RegLens size, DispatchSizeTy size) => Param size -> Param size -> Op
  Sub :: Param S8 -> Op
  Nop :: Op
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
deriving instance Show (SizeTy size) => Show (Param size)

class ParamSize size where
  paramSize :: Param size -> Int
instance ParamSize S8 where
  paramSize p = 1
instance ParamSize S16 where
  paramSize p = 2

data Inst = Inst
  { _op :: Op
  , _cycles :: Natural
  }
  deriving Show
makeLenses ''Inst

opLen :: Op -> Word16
opLen (Add dest src) = 1 + paramLen src
opLen (Sub src) = 1 + paramLen src
opLen (Extended op) = 1 + opLen op
opLen Nop = 1

opcodeRange :: (a -> Op) -> [[(a, Natural)]] -> (Opcode, Opcode) -> [(Opcode, Inst)]
opcodeRange op ps rng = zip (rangeOc rng) ((\(p, c) -> Inst (op p) c) <$> (concat ps))

aluParams :: [(Param S8, Natural)]
aluParams = ((,4) . Reg <$> [B, C, D, E, H, L]) <> [(AddrOf (Reg HL), 8), (Reg A, 4)]

addA :: [(Opcode, Inst)]
addA = opcodeRange (Add (Reg A))
  [aluParams]
  (Opcode 0x80, Opcode 0x87)

sub :: [(Opcode, Inst)]
sub = opcodeRange Sub
  [aluParams]
  (Opcode 0x90, Opcode 0x97)

misc = [(0x00, Inst Nop 4)]

instructions :: [(Opcode, Inst)]
instructions = addA <> sub <> misc

instrMap :: Map Opcode Inst
instrMap = fromList instructions
