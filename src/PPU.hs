module PPU where

import Control.Monad
import Data.Bits
import Data.Ix
import Data.Word
import qualified Data.Vector.Sized as VS

import RAM
import Screen
import Utils

type VRAM = RAM 0x2000
data Line = Line Word8 Word8
  deriving Show
data Tile = Tile Line Line Line Line Line Line Line Line
  deriving Show

data AddrMode = AM8000 | AM8800

tileSize :: Word16
tileSize = 16

wordToPixs :: Word8 -> [Pixel]
wordToPixs byte = f <$> [0,2,4,6] where
  f n = mkPixel (testBit byte n, testBit byte (n+1))

pairs :: [a] -> Maybe [(a,a)]
pairs [] = Just []
pairs (a:b:xs) = ((a,b):) <$> pairs xs
pairs [x] = Nothing

mkTile :: [Word8] -> Maybe Tile
mkTile = f <=< pure . fmap (uncurry Line) <=< pairs where
  f [a,b,c,d,e,f,g,h] = Just (Tile a b c d e f g h)
  f _ = Nothing

fetchTile :: AddrMode -> VRAM -> Word8 -> Maybe Tile
fetchTile AM8000 vram tnum = getTileAtAddr (fromIntegral tnum * tileSize) vram
fetchTile AM8800 vram tnum
  | tnum < 128 = getTileAtAddr ((fromIntegral tnum * tileSize) + 0x1000) vram
  | otherwise  = getTileAtAddr ((fromIntegral tnum * tileSize) - 128 + 0x800) vram

getTileAtAddr :: Word16 -> VRAM -> Maybe Tile
getTileAtAddr addr vram = mkTile =<< mapM (vram !) (range (addr, addr + tileSize - 1))

getBGTileMap :: VRAM -> Maybe [Word8]
getBGTileMap vram = mapM (vram !) (range (0x1800, 0x1BFF))

tileToPixels :: Tile -> [[Pixel]]
tileToPixels (Tile a b c d e f g h) = ltp <$> [a,b,c,d,e,f,g,h]
  where
tileGridToPixels :: [Tile] -> [[Pixel]]
tileGridToPixels ts = let tileLines = chunks 32 (tileToPixels <$> ts)
                      in undefined
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

vramToScreen :: VRAM -> Maybe Screen
vramToScreen vram = tilesToScreen <$> (mapM (fetchTile AM8000 vram) =<< getBGTileMap vram)

sampleVram :: VRAM
sampleVram = RAM (fromJust $ VS.fromList (padList 0x2000 0 l))
  where
    l = padList 0x1800 0 (blankTile <> sampleTile) <> cycle [0,1,1,0]
    sampleTile = padList 16 0 [0..]
    blankTile = replicate 16 0

{-
PPU TODO:
- Get addressing mode from register
- Scroll X/Y
- Window
- Sprites
- Run once per pixel rather than once per frame
-}
