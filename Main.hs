{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Main where

import Control.Applicative
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Optics
import Data.Function
import Data.List.NonEmpty (NonEmpty, fromList)
import qualified Data.Ratio as Ratio
import qualified Data.Sequence as Seq
import GHC.Conc (ThreadId)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as N
import Optics
import System.Environment
import System.Exit
import System.IO
import System.Signal

data Report where
  Success :: Report
  Failure :: Report
  deriving (Eq)

reporter = iso enc dec
  where
    enc :: Report -> Bool
    enc Success = True
    enc Failure = False
    dec :: Bool -> Report
    dec True = Success
    dec False = Failure

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
    _ -> print "Invalid arguments"

-- | Launch the sender and the aggregator
fullDemonstrationMain :: IO ()
fullDemonstrationMain = error "not implemented"

-- | Send data to the monitoring server and see how it reacts
sendDemoReportsMain :: IO ()
sendDemoReportsMain = error "not implemented"

-- | Run a monitoring server
aggregateReportsMain :: IO ()
aggregateReportsMain = do
  (reportQueue :: TQueue Report) <- newTQueueIO
  alarmQueue <- newTQueueIO
  [ recvReports sa reportQueue,
    analyzeReports windowSize reportQueue alarmQueue,
    sendAlarms alarmQueue,
    waitForTerminationSignal
    ]
    & foldr1 race_
  putStrLn "The monitoring server is stopping"
  where
    windowSize :: Int
    windowSize = 10
    sa :: S.SockAddr
    sa = undefined

-- | Terminate with all other threads racing with us
waitForTerminationSignal :: IO ()
waitForTerminationSignal = do
  terminate <- newTVarIO False
  -- Install the SIGTERM handler
  installHandler sigTERM \_ -> writeTVar terminate True & atomically
  -- Check terminate's value until it is true (i.e. retry if false)
  (readTVar terminate >>= check) & atomically

sendAlarms :: TQueue a00 -> IO ()
sendAlarms = error "not implemented"

-- | Keep a list of the most recent n reports:
-- * if 80% or more are successes, the system is in good working order
-- * if 50% or fewer are successes, the system is failing
-- * a success rate between 50 and 80% doesn't allow to make a determination
analyzeReports :: Int -> TQueue Report -> TQueue SystemStatus -> IO ()
analyzeReports ws rq sq = go Nothing ws mempty
  where
    shouldAlert :: [] Report -> Maybe SystemStatus
    shouldAlert = \case
      [] -> Nothing
      (Success :< reports) ->
        walk (1, 1) kont reports
      (Failure :< reports) ->
        walk (0, 1) kont reports
      where
        kont :: Ratio.Ratio Int -> Maybe SystemStatus
        kont r
          | r <= 50 Ratio.% 100 = Just Alarm
          | 80 Ratio.% 100 <= r = Just Okay

        walk :: (Int, Int) -> (Ratio.Ratio Int -> Maybe SystemStatus) -> [] Report -> Maybe SystemStatus
        walk stats@(uncurry (Ratio.%) -> r) k [] = k r -- todo: smunix: implement short-circuiting if ratio is hit before end of list
        walk stats@(uncurry (Ratio.%) -> r) k (Success :< reports) = walk (stats ^. _1 + 1, stats ^. _2 + 1) k reports
        walk stats@(uncurry (Ratio.%) -> r) k (Failure :< reports) = walk (stats ^. _1, stats ^. _2 + 1) k reports

    go :: Maybe SystemStatus -> Int -> [] Report -> IO ()
    go !status 0 reports = do
      status'
        & traverseOf
          traversed
          \s ->
            writeTQueue sq s
              & atomically
              & when (status /= status')
      analyzeReports ws rq sq
      where
        status' :: Maybe SystemStatus
        !status' = shouldAlert reports <|> status
    go !status !n reports = do
      rpt <- readTQueue rq & atomically
      go status (n - 1) (rpt <| reports)

-- | Read report data from client socket and write it into the report queue
pullReports :: S.Socket -> TQueue Report -> IO ()
pullReports cs rq = do
  bytes <- N.recv cs 1024
  case C.length bytes of
    0 -> return ()
    _ -> reportBytes bytes rq >> pullReports cs rq

reportBytes :: C.ByteString -> TQueue Report -> IO ()
reportBytes str rq =
  str
    ^. unpackedChars
    & traverseOf
      traversed
      decodeByte
    & void
  where
    -- todo: smunix: decode a the bit level instead?
    decodeByte :: Char -> IO ()
    decodeByte =
      \case
        '0' -> writeTQueue rq Failure & atomically
        '1' -> writeTQueue rq Success & atomically
        _ -> return ()

recvReports :: S.SockAddr -> TQueue Report -> IO ()
recvReports sa rq =
  withServerSocket
    sa
    \ss ->
      mask (mkReporter ss)
        & forever
  where
    mkReporter :: S.Socket -> (IO () -> IO ()) -> IO ThreadId
    mkReporter ss restore = do
      (cs :: S.Socket, caddr :: S.SockAddr) <- S.accept ss
      forkFinally
        (pullReports cs rq & restore)
        \_ -> S.close cs

withServerSocket :: S.SockAddr -> (S.Socket -> IO a) -> IO a
withServerSocket sa action = bracket (S.socket S.AF_UNIX S.Stream S.defaultProtocol) S.close \ss -> do
  S.bind ss sa
  S.listen ss S.maxListenQueue
  action ss
