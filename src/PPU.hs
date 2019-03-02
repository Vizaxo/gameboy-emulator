module PPU where

import Control.Monad
import Control.Monad.Trans
import Data.Bits
import Data.Ix
import Data.Word

import RAM
import Screen
import Utils

type VRAM = RAM 0x2000
data Line = Line Word8 Word8
  deriving Show
data Tile = Tile Line Line Line Line Line Line Line Line
  deriving Show

data AddrMode = AM8000 | AM8800

type MonadPPU m = (MonadIO m, MonadPlus m)

tileSize :: Word16
tileSize = 16

wordToPixs :: Word8 -> [Pixel]
wordToPixs byte = f <$> [0,2,4,6] where
  f n = mkPixel (testBit byte n, testBit byte (n+1))

pairs :: MonadPlus m => [a] -> m [(a,a)]
pairs [] = pure []
pairs (a:b:xs) = ((a,b):) <$> pairs xs
pairs [x] = mzero

mkTile :: MonadPlus m => [Word8] -> m Tile
mkTile = f <=< pure . fmap (uncurry Line) <=< pairs where
  f [a,b,c,d,e,f,g,h] = pure (Tile a b c d e f g h)
  f _ = mzero

fetchTile :: MonadPPU m => AddrMode -> VRAM -> Word8 -> m Tile
fetchTile AM8000 vram tnum = getTileAtAddr (fromIntegral tnum * tileSize) vram
fetchTile AM8800 vram tnum
  | tnum < 128 = getTileAtAddr ((fromIntegral tnum * tileSize) + 0x1000) vram
  | otherwise  = getTileAtAddr ((fromIntegral tnum * tileSize) - 128 + 0x800) vram

getTileAtAddr :: MonadPPU m => Word16 -> VRAM -> m Tile
getTileAtAddr addr vram = mkTile =<< mapM (vram !) (range (addr, addr + tileSize - 1))

getBGTileMap :: MonadPPU m => VRAM -> m [Word8]
getBGTileMap vram = mapM (vram !) (range (0x1800, 0x1BFF))

tileToPixels :: Tile -> [[Pixel]]
tileToPixels (Tile a b c d e f g h) = ltp <$> [a,b,c,d,e,f,g,h]
  where
    ltp (Line a b) = zipWith (curry mkPixel) (testBit a <$> [7,6..0]) (testBit b <$> [7,6..0])

type Grid a = [[a]]
unNestGrid :: Grid (Grid a) -> Grid a
unNestGrid = concat . fmap f

f :: [Grid a] -> Grid a
f [] = undefined -- ?
f [x] = x
f (x:xs) = zipWith (<>) x (f xs)

tilesToScreen :: [Tile] -> Screen
tilesToScreen ts = let foo = tileToPixels <$> ts
                       bar = unNestGrid (chunks 32 foo)
                   in mkScreen (concat $ take screenHeight (take screenWidth <$> bar))

vramToScreen :: MonadPPU m => VRAM -> m Screen
vramToScreen vram = tilesToScreen <$> (mapM (fetchTile AM8000 vram) =<< getBGTileMap vram)

{-
PPU TODO:
- Get addressing mode from register
- Scroll X/Y
- Window
- Sprites
- Run once per pixel rather than once per frame
-}
