module Main where

import Codec.Picture
import Encoder
import Options.Applicative
import Sound

data Options = Options
  { inputFile :: FilePath,
    outputFile :: FilePath
  }

options :: Parser Options
options =
  Options
    <$> strOption
      ( long "input"
          <> short 'i'
          <> metavar "INPUT_FILE"
          <> help "Input image path"
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "OUTPUT_FILE"
          <> help "Output wav file path"
      )

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "A functional SSTV encoder~"
            <> header "sstv-nya"
        )

  img <- either (error . ("Error reading image: " ++)) id <$> readImage (inputFile opts)
  writeAudioFile (outputFile opts) (generateSignal $ martin $ convertRGB8 img)
  -- writeAudioFile (outputFile opts) (generateSignal (concat $ replicate 5 [(1500, 1000), (2000, 2000)]))
