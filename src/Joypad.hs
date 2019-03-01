module Joypad where

import Control.Lens
import Data.Bits hiding (bit)
import Data.Word
import Bits

data Joypad = Joypad
  { _right :: Bool
  , _left :: Bool
  , _up :: Bool
  , _down :: Bool
  , _aBtn :: Bool
  , _bBtn :: Bool
  , _select :: Bool
  , _start :: Bool
  }
  deriving Show
makeLenses ''Joypad

defaultJoypad = Joypad False False False False False False False False

updateJoypadIOReg :: Joypad -> Word8 -> Word8
updateJoypadIOReg j byte =
  (if testBit byte 4 then p14 else id) $
  (if testBit byte 5 then p15 else id) $
  (byte .|. 0x0F)
  where
    p14 = setInput 0 right . setInput 1 left . setInput 2 up . setInput 3 down
    p15 = setInput 0 aBtn . setInput 1 bBtn . setInput 2 select . setInput 3 start
    setInput :: Int -> Lens' Joypad Bool -> Word8 -> Word8
    setInput n btn b = over (bit n) (if (j ^. btn) then const False else id) b

