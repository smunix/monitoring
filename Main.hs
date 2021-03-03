-- |
module Main where

import System.Environment
import System.Exit
import System.IO

data EventReport where
  Success :: EventReport
  Failure :: EventReport
  deriving (Eq)

data SystemStatus where
  Okay :: SystemStatus
  Alarm :: SystemStatus
  deriving (Eq)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  getArgs >>= \case
    ["aggregate-reports"] -> aggregateReportsMain
    ["send-demo-reports"] -> sendDemoReportsMain
    ["full-demonstration"] -> fullDemonstrationMain
    _ -> die "Invalid arguments"

fullDemonstrationMain :: IO ()
fullDemonstrationMain = error "not implemented"

sendDemoReportsMain :: IO ()
sendDemoReportsMain = error "not implemented"

aggregateReportsMain :: IO ()
aggregateReportsMain = error "not implemented"
