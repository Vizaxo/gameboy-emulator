module ROM where

import Control.Lens
import Data.Word
import Data.Ix
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

newtype ROM = ROM ByteString
  deriving (Show, Eq, Ord)

type instance IxValue ROM = Word8
type instance Index ROM = Word16
instance Ixed ROM where
  ix e f s = case mapBSRom (BS.splitAt (fromIntegral e)) s of
     (l, mr) -> case BS.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> f c <&> \d -> ROM (BS.concat [l, BS.singleton d, xs])
  {-# INLINE ix #-}

mapBSRom :: (ByteString -> a) -> ROM -> a
mapBSRom f (ROM bs) = f bs

readRomFile :: FilePath -> IO ROM
readRomFile = (ROM <$>) . BS.readFile

mkRom :: [Word8] -> ROM
mkRom = ROM . BS.pack
