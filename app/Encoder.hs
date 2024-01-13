module Encoder where

import Codec.Picture
import Data.Bits (testBit)
import Data.Int (Int32, Int8)
import Sound

scanline :: [PixelRGB8] -> ([Int8], [Int8], [Int8])
scanline pixels = unzip3 $ map extractColorValues pixels

extractColorValues :: PixelRGB8 -> (Int8, Int8, Int8)
extractColorValues (PixelRGB8 r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)

modulate :: Int8 -> [Int32]
modulate n = tone (1500 + fromIntegral n * 3.1372549) 0.4576

bit :: Bool -> [Int32]
bit True = tone 1100 30
bit False = tone 1300 30

vox :: [Int32]
vox =
  concatMap
    (uncurry tone)
    [ (1900, 100),
      (1500, 100),
      (1900, 300),
      (1500, 100),
      (2300, 100),
      (1500, 100),
      (2300, 100),
      (1500, 100)
    ]

visHeader :: Int8 -> [Int32]
visHeader visId =
  concat
    [ tone 1900 300,
      tone 0 10,
      tone 1900 300,
      tone 1200 30,
      concatMap bit (reverse [testBit visId i | i <- [0 .. 6]]),
      bit (odd $ toInteger visId),
      tone 1200 30
    ]

sstvSequence :: [PixelRGB8] -> [Int32]
sstvSequence line =
  concat
    [ tone 1200 4.862, -- Sync pulse
      tone 1500 0.572, -- Sync porch
      concatMap modulate greens,
      tone 1500 0.572, -- Separator pulse
      concatMap modulate blues,
      tone 1500 0.572, -- Separator pulse
      concatMap modulate reds,
      tone 1500 0.572 -- Sync porch
    ]
  where
    (reds, greens, blues) = scanline line

martin2 :: Image PixelRGB8 -> [Int32]
martin2 image = concat [vox, visHeader 44, concatMap sstvSequence rows]
  where
    rows = [[pixelAt image x row | x <- [0 .. imageWidth image - 1]] | row <- [0 .. imageHeight image - 1]]