module Main where

import Data.Bits (testBit)
import Data.Int (Int32, Int8)
import Sound

bit :: Bool -> [Int32]
bit True = tone 1100 30
bit False = tone 1300 30

vox :: [Int32]
vox =
  tone 1900 100
    ++ tone 1500 100
    ++ tone 1900 300
    ++ tone 1500 100
    ++ tone 2300 100
    ++ tone 1500 100
    ++ tone 2300 100
    ++ tone 1500 100

header :: Int8 -> [Int32]
header id =
  tone 1900 300
    ++ tone 0 10
    ++ tone 1900 300
    ++ tone 1200 30
    ++ concatMap bit (reverse [testBit id i | i <- [0 .. 6]])
    ++ bit (odd $ toInteger id)
    ++ tone 1200 30

-- Main function
main :: IO ()
main = do
  makeWavFile "temp.wav" (waveData (vox ++ header 60))