module Sound where

import Data.Int (Int32)
import Data.WAVE

-- Constants
samplesPerSecond :: Int
samplesPerSecond = 48000

volumeGeneral :: Int32
volumeGeneral =  (volume 0.8)

bitrate :: Int
bitrate = 32

-- Function to generate tone samples
tone :: Double  -> Double -> [Int32]
tone freq len = toneFull freq samplesPerSecond len volumeGeneral

-- Function to generate tone samples
toneFull :: Double -> Int -> Double -> Int32 -> [Int32]
toneFull freq samples len vol =
  take (round $ len/1000 * fromIntegral samples) $
    map ((round . (* fromIntegral vol)) . sin) [0.0, (freq * 2 * pi / fromIntegral samples) ..]

volume :: Double -> Int32
volume v = maxBound `div` (round (v ** (-1)))

-- Creating WAVE data
waveData :: [Int32] -> WAVE
waveData d = WAVE (WAVEHeader 1 samplesPerSecond bitrate Nothing) (map (:[]) d)

-- Function to write WAVE data to a file
makeWavFile :: FilePath -> WAVE -> IO ()
makeWavFile = putWAVEFile