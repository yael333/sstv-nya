module Main where

import Data.Int (Int32)
import Sound

-- Generating sample data
samples :: [[Int32]]
samples =
  map
    (: [])
    ( tone 600 300 (volume 0.5)
        ++ tone 0 200 (volume 0.5)
        ++ tone 600 300 (volume 0.5)
    )

-- Main function
main :: IO ()
main = do
  makeWavFile "temp.wav" (waveData samples)