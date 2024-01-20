module Encoder where

import Codec.Picture
import Data.Bits (testBit)
import Data.Int (Int32, Int8)
import Sound
import Data.Word (Word8)
import Debug.Trace (traceShowId)

scanline :: [PixelRGB8] -> ([Word8], [Word8], [Word8])
scanline pixels = unzip3 $ map extractColorValues pixels

extractColorValues :: PixelRGB8 -> (Word8, Word8, Word8)
extractColorValues (PixelRGB8 r g b) = (r, g, b)

modulate :: Word8 -> Tone
modulate n = (1500 + ( fromIntegral n) * 3.1372549, 0.4576)

bit :: Bool -> Tone
bit True = (1100, 30)
bit False = (1300, 30)

vox :: [Tone]
vox =
  [ (1900, 100),
    (1500, 100),
    (1900, 300),
    (1500, 100),
    (2300, 100),
    (1500, 100),
    (2300, 100),
    (1500, 100)
  ]

visHeader :: Int8 -> [Tone]
visHeader visId =
  [ (1900, 300),
    (0, 10),
    (1900, 300),
    (1200, 30)
  ]
    ++ map bit (reverse [testBit visId i | i <- [0 .. 6]])
    ++ [ bit (odd $ toInteger visId),
         (1200, 30)
       ]

sstvSequence :: [PixelRGB8] -> [Tone]
sstvSequence line =
  [ (1200, 4.862), -- Sync pulse
    (1500, 0.572) -- Sync porch
  ]
    ++ map modulate greens
    ++ [(1500, 0.572)]
    ++ map modulate blues -- Separator pulse
    ++ [(1500, 0.572)]
    ++ map modulate reds -- Separator pulse
    ++ [ (1500, 0.572) -- Sync porch
       ]
  where
    (reds, greens, blues) = scanline line

martin :: Image PixelRGB8 -> [Tone]
martin image = concat [visHeader 44, concatMap sstvSequence rows]
  where
    rows = [[pixelAt image x row | x <- [0 .. imageWidth image - 1]] | row <- [0 .. imageHeight image - 1]]