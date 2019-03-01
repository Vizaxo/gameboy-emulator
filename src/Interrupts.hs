module Interrupts where

import Control.Lens
import Data.Word

import Bits
import CPUState

data Interrupt = VBLANK | JOYPAD | TIMER | LCDC | SERIAL
  deriving Show

irqAddr :: Interrupt -> Word16
irqAddr VBLANK = 0x40
irqAddr LCDC = 0x48
irqAddr TIMER = 0x50
irqAddr SERIAL = 0x58
irqAddr JOYPAD = 0x60

ifBit :: Interrupt -> Traversal' CPUState Bool
ifBit i = memory 0xFFFF . bit (interruptBit i) where

interruptBit :: Interrupt -> Int
interruptBit VBLANK = 0
interruptBit LCDC = 1
interruptBit TIMER = 2
interruptBit SERIAL = 3
interruptBit JOYPAD = 4
