module Sound where

import Data.Int (Int32)
import Data.WAVE

-- Constants
samplesPerSecond :: Int
samplesPerSecond = 16000

bitrate :: Int
bitrate = 32

-- Function to generate tone samples
tone :: Double  -> Double -> Int32 -> [Int32]
tone freq len vol = toneSample  freq samplesPerSecond len vol

-- Function to generate tone samples
toneSample :: Double -> Int -> Double -> Int32 -> [Int32]
toneSample freq samples len vol =
  take (round $ (len/1000) * fromIntegral samples) $
    map ((round . (* fromIntegral vol)) . sin) [0.0, (freq * 2 * pi / fromIntegral samples) ..]

volume :: Double -> Int32
volume v = maxBound `div` (round (v ** (-1)))

-- Creating WAVE data
waveData :: [[Int32]] -> WAVE
waveData = WAVE (WAVEHeader 1 samplesPerSecond bitrate Nothing)

-- Function to write WAVE data to a file
makeWavFile :: FilePath -> WAVE -> IO ()
makeWavFile = putWAVEFile