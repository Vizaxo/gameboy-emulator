module Bits where

import Control.Lens
import Data.Bits
import Data.Word


lower :: Lens' Word16 Word8
lower = lens getLower setLower

upper :: Lens' Word16 Word8
upper = lens getUpper setUpper

getLower :: Word16 -> Word8
getLower = fromIntegral

getUpper :: Word16 -> Word8
getUpper = fromIntegral . flip shiftR 8

setLower :: Word16 -> Word8 -> Word16
setLower w l = (shiftL (fromIntegral (getUpper w)) 8) .|. fromIntegral l

setUpper :: Word16 -> Word8 -> Word16
setUpper w u = (shiftL (fromIntegral u) 8) .|. fromIntegral (getLower w)

twoBytes :: Word8 -> Word8 -> Word16
twoBytes l u = set upper u (set lower l 0)
