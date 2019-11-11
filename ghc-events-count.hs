module Main (main) where

import qualified GHC.RTS.Events as GHC
import qualified System.Environment as System

main :: IO ()
main = do
  [filename] <- System.getArgs
  eventLog <- GHC.readEventLogFromFile filename >>= either error return
  let events = GHC.events $ GHC.dat eventLog
  print (length events)
  return ()
