module Main (main) where

import qualified Options.Applicative as Options
import qualified Speedscope.Main as Speedscope

main :: IO ()
main =
  Speedscope.main =<< Options.execParser info
  where
    info =
        Options.info
          (Speedscope.parseOptions Options.<**> Options.helper)
          (Options.fullDesc)
