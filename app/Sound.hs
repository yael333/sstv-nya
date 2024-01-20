module Sound where

import Codec.Audio.Wave
import Data.Int (Int32)
import Data.List (zip4, zipWith5)
import Foreign (withArray)
import GHC.IO.Handle (hPutBuf, Handle)
import GHC.IO.Handle.FD (withFile)
import GHC.IO.IOMode (IOMode (..))

type Signal = [Int32]

type Tone = (Double, Double) -- Freq, Dur

type PhaseTone = (Tone, Double)

-- Constants
samplesPerSecond :: Int
samplesPerSecond = 44100

volumeGeneral :: Int32
volumeGeneral = volume 0.9

bitrate :: Int
bitrate = 32

generateSine ::
  Double ->
  -- | Frequency
  Double ->
  -- | Phase Shift
  Int ->
  -- | Samples per second
  Double ->
  -- | Length of sound in milliseconds
  Int32 ->
  -- | Volume, (maxBound :: Int32) for highest, 0 for lowest
  Signal
generateSine freq phase samples len vol =
  map (truncate . (* fromIntegral vol)) $
    take (round $ len * fromIntegral samples / 1000) $
      map sin [phase, (freq * 2 * pi / fromIntegral samples) + phase ..]

calculatePhases :: [Tone] -> [Double]
calculatePhases tones = go tones 0 0 0
  where
    go :: [Tone] -> Double -> Double -> Double -> [Double]
    go [] _ _ _ = []
    go ((0, dur) : xs) _ _ acc = 0 : go xs 0 0 (acc + dur)
    go ((freq, dur) : xs) prevFreq prevPhase acc = p : go xs freq p (acc + dur)
      where
        p = (prevFreq / freq) * (acc + prevPhase) - acc

generateSignal :: [Tone] -> Signal
generateSignal tones = concat $ zipWith5 generateSine (map fst tones) (calculatePhases tones) (repeat samplesPerSecond) (map snd tones) (repeat volumeGeneral)

volume :: Double -> Int32
volume v = maxBound `div` round (v ** (-1))

-- Creating WAVE data
waveData :: Signal -> Wave
waveData s =
  Wave
    { waveFileFormat = WaveVanilla,
      waveSampleRate = 44100,
      waveSampleFormat = SampleFormatPcmInt 32,
      waveChannelMask = speakerMono,
      waveDataOffset = 44,
      waveDataSize = fromIntegral $ length s * 4,
      waveSamplesTotal = fromIntegral $ length s,
      waveOtherChunks = []
    }

writeAudioFile :: FilePath -> Signal -> IO ()
writeAudioFile f s = writeWaveFile f (waveData s) (writeSignalH s)

writeSignalH :: Signal -> Handle -> IO ()
writeSignalH s handle = do
  withArray s $ \ptr -> do
    hPutBuf handle ptr (4 * length s) -- Assuming 4 bytes per Int32
