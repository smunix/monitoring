{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Optics
import Data.Function
import Data.Kind
import Data.Primitive
import Data.Ratio (Ratio)
import qualified Data.Ratio as Ratio
import Data.Word
import qualified Debug.Trace as Dbg
import Foreign.C.Types
import GHC.Conc (ThreadId)
import GHC.TypeLits
import qualified Network.Socket as S
import Network.Socket.ByteString
import qualified Network.Socket.ByteString as N
import Optics
import System.Environment
import System.Exit
import System.IO
import System.Posix (removeLink)
import System.Signal

data Report where
  Success :: Report
  Failure :: Report
  deriving (Eq, Show)

data Status where
  Green :: Status
  Red :: Status
  deriving (Eq, Show)

class WithSocket (a :: Symbol) where
  withSocket :: S.SockAddr -> (S.Socket -> IO ()) -> IO ()

instance WithSocket "server" where
  withSocket sa action =
    bracket
      (S.socket S.AF_UNIX S.Stream S.defaultProtocol)
      S.close
      \ss -> do
        S.bind ss sa
        S.listen ss S.maxListenQueue
        action ss

instance WithSocket "client" where
  withSocket sa action =
    bracket
      (S.socket S.AF_UNIX S.Stream S.defaultProtocol)
      S.close
      \sc -> do
        S.connect sc sa
        action sc

reporter = iso enc dec
  where
    enc :: Report -> Bool
    enc Success = True
    enc Failure = False
    dec :: Bool -> Report
    dec True = Success
    dec False = Failure

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  getArgs >>= \case
    ["aggregate-reports", sa] -> aggregateReportsMain 10 sa
    ["send-demo-reports", sa] -> sendDemoReportsMain sa
    ["recv", sa] -> aggregateReportsMain 10 sa
    ["send", sa] -> sendDemoReportsMain sa
    ["full-demonstration"] -> fullDemonstrationMain
    _ -> print "Invalid arguments"

-- | Launch the sender and the aggregator
fullDemonstrationMain :: IO ()
fullDemonstrationMain = error "not implemented"

-- | Send data to the monitoring server and see how it reacts
sendDemoReportsMain :: [] Char -> IO ()
sendDemoReportsMain saddr = do
  (reportQueue :: TQueue Report) <- newTQueueIO
  [ generateReports 30 reportQueue,
    sendReports sa reportQueue
    ]
    & foldr1 race_
  print "Done sending demo reports."
  where
    sa :: S.SockAddr
    sa = S.SockAddrUnix saddr

sendReports ::
  S.SockAddr ->
  TQueue Report ->
  IO ()
sendReports sa rq = withSocket @"client" sa (forever . loop)
  where
    loop :: S.Socket -> IO ()
    loop cs =
      readTQueue rq
        & atomically
        >>= sendAll cs
          . ( \case
                Success -> "1"
                Failure -> "0"
            )

generateReports :: Int -> TQueue Report -> IO ()
generateReports dur rq =
  [ forever act,
    threadDelay (dur * 1_000_000)
  ]
    & foldr1 race_
  where
    act =
      [ "Peter Abrams once said, I am not a free man... ",
        "That was the sad reality of life in ancient Africa",
        "We obviously can spend days, weeks, months and even years",
        "... debating the necessicity of such a claim",
        "And I'm not sure we will ever come to an agreement."
      ]
        & foldl1 (<>)
        & reports
          ( \r -> writeTQueue rq r & atomically
          )
    reports :: (Report -> IO ()) -> C.ByteString -> IO ()
    reports kont (b :< bytes) = do
      byteR 8 b
      reports kont bytes
      where
        byteR :: Word8 -> Word8 -> IO ()
        byteR !count !w
          | 0 == count = pure ()
          | 0x1 <- b = r Success
          | 0x0 <- b = r Failure
          where
            r x = kont x >> byteR (count -1) w'
            b = w .&. 0x1
            w' = w `shiftR` 1

aggregateReportsMain :: Int -> [] Char -> IO ()
aggregateReportsMain windowSize saddr = do
  (reportQueue :: TQueue Report) <- newTQueueIO
  alarmQueue <- newTQueueIO
  removeLink saddr
  [ recvReports sa reportQueue,
    -- analyze @"reports" @"list" windowSize reportQueue alarmQueue,
    analyze @"reports" @"wma" windowSize reportQueue alarmQueue,
    sendAlarms alarmQueue,
    waitForTerminationSignal
    ]
    & foldr1 race_
  where
    sa :: S.SockAddr
    sa = S.SockAddrUnix saddr

-- | Terminate with all other threads racing with us
waitForTerminationSignal :: IO ()
waitForTerminationSignal = do
  terminate <- newTVarIO False
  -- Install the SIGTERM handler
  installHandler
    sigTERM
    \_ -> writeTVar terminate True & atomically
  -- Check terminate's value until it is true (i.e. retry if false)
  (readTVar terminate >>= check) & atomically

sendAlarms :: TQueue Status -> IO ()
sendAlarms sq = loop & forever
  where
    loop :: IO ()
    loop =
      readTQueue sq & atomically
        >>= print

class Analyze (t :: Symbol) (s :: Symbol) where
  type AnalyzeT t s :: Type
  analyze :: Int -> TQueue Report -> TQueue Status -> IO ()

-- | Keep a list of the most recent n reports:
-- * if 80% or more are successes, the system is in good working order
-- * if 50% or fewer are successes, the system is failing
-- * a success rate between 50 and 80% doesn't allow to make a determination
instance Analyze "reports" "list" where
  type AnalyzeT "reports" "list" = Int -> TQueue Report -> TQueue Status -> IO ()
  analyze ws rq sq = go Nothing ws mempty
    where
      shouldAlert ::
        [] Report ->
        Maybe Status
      shouldAlert = \case
        [] -> Nothing
        Success :< reports -> walk 1 kont reports
        Failure :< reports -> walk 0 kont reports
        where
          kont ::
            Ratio.Ratio Int ->
            Maybe Status
          kont r
            | r <= 30 Ratio.% 100 = Just Red
            | 50 Ratio.% 100 <= r = Just Green
            | otherwise = Nothing

          rate :: Int -> Ratio.Ratio Int
          rate = flip (Ratio.%) ws

          walk ::
            Int ->
            (Ratio.Ratio Int -> Maybe Status) ->
            [] Report ->
            Maybe Status
          walk (rate -> !r) k [] = Dbg.traceShow r (k r)
          walk !stats k (Success :< reports) = walk (stats + 1) k reports
          walk !stats k (Failure :< reports) = walk stats k reports

      go :: Maybe Status -> Int -> [] Report -> IO ()
      go !status 0 reports = do
        print (status, status')
        status'
          & traverseOf
            traversed
            \s ->
              writeTQueue sq s
                & atomically
                & when (status /= status')
        go status' ws mempty
        where
          status' :: Maybe Status
          !status' = shouldAlert reports <|> status
      go !status !n reports = do
        rpt <- readTQueue rq & atomically
        go status (n - 1) (rpt <| reports)

{- Analyze reports using a Window Moving average value -}
instance Analyze "reports" "wma" where
  type AnalyzeT "reports" "wma" = Int -> TQueue Report -> TQueue Status -> IO ()
  analyze ws rq sq =
    newAlignedPinnedPrimArray @IO @CBool ws
      >>= go0
        ( \case
            Success -> 1
            Failure -> 0
        )
        id
        ws'max
    where
      ws'max = ws -1

      rate :: Int -> Ratio Int
      rate = flip (Ratio.%) ws

      withStatus0 :: Int -> (Maybe Status -> IO ()) -> IO ()
      withStatus0 (rate -> r) k
        | r <= 30 Ratio.% 100 = Red & pure & k
        | 80 Ratio.% 100 <= r = Green & pure & k
        | otherwise = k Nothing

      withStatus1 ::
        Int ->
        Status ->
        (Status -> Status -> IO ()) ->
        IO ()
      withStatus1 (rate -> r) status k
        | r <= 30 Ratio.% 100 = k status Red
        | 80 Ratio.% 100 <= r = k status Green
        | otherwise = k status status

      alarm0 ::
        (Report -> Word8) ->
        Int ->
        Int ->
        MutablePrimArray (PrimState IO) CBool ->
        Status ->
        IO ()
      alarm0 conv !count !i arr status =
        writeTQueue sq status & atomically
          >> go3 conv (+ count) i status arr

      alarm1 ::
        (Report -> Word8) ->
        Int ->
        Int ->
        MutablePrimArray (PrimState IO) CBool ->
        Status ->
        Status ->
        IO ()
      alarm1 conv !count !i arr status status' =
        writeTQueue sq status & atomically & when (status /= status')
          >> go3 conv (+ count) i status' arr

      go3 ::
        (Report -> Word8) ->
        (Int -> Int) ->
        Int ->
        Status ->
        MutablePrimArray (PrimState IO) CBool ->
        IO ()
      go3 conv countFn 0 !status arr = do
        !b <- readTQueue rq & atomically <&> conv
        b & CBool & writePrimArray arr 0
        !b' <- readPrimArray arr ws'max
        let !count = b & fromIntegral & countFn & flip subtract (b' & fromIntegral)
        withStatus1
          count
          status
          (alarm1 conv count ws'max arr)
      go3 conv countFn !i !status arr = do
        !b <- readTQueue rq & atomically <&> conv
        b & CBool & writePrimArray arr i
        !b' <- readPrimArray arr i'
        let !count = b & fromIntegral & countFn & flip subtract (b' & fromIntegral)
        withStatus1
          count
          status
          (alarm1 conv count i' arr)
        where
          !i' = i -1

      go1 ::
        (Report -> Word8) ->
        (Int -> Int) ->
        Int ->
        MutablePrimArray (PrimState IO) CBool ->
        IO ()
      go1 conv countFn 0 arr = do
        !b <- readTQueue rq & atomically <&> conv
        b & CBool & writePrimArray arr ws'max
        !b' <- readPrimArray arr ws'max
        let !count = b & fromIntegral & countFn & flip subtract (b' & fromIntegral)
        withStatus0
          count
          \case
            Just status' -> alarm0 conv count ws'max arr status'
            _ -> go1 conv (+ count) ws'max arr
      go1 conv countFn !i arr = do
        !b <- readTQueue rq & atomically <&> conv
        b & CBool & writePrimArray arr i
        !b' <- readPrimArray arr i'
        let !count = b & fromIntegral & countFn & flip subtract (b' & fromIntegral)
        withStatus0
          count
          \case
            Just status' -> alarm0 conv count i' arr status'
            _ -> go1 conv (+ count) i' arr
        where
          !i' = i -1

      go0 ::
        (Report -> Word8) ->
        (Int -> Int) ->
        Int ->
        MutablePrimArray (PrimState IO) CBool ->
        IO ()
      go0 conv countFn 0 arr = do
        !b <- readTQueue rq & atomically <&> conv
        b & CBool & writePrimArray arr 0
        let !count = b & fromIntegral & countFn
        withStatus0
          count
          \case
            Just status' -> alarm0 conv count ws'max arr status'
            _ -> go1 conv (+ count) ws'max arr
      go0 conv countFn !i arr = do
        !b <- readTQueue rq & atomically <&> conv
        b & CBool & writePrimArray arr i
        go0 conv (+ (b & fromIntegral & countFn)) (i -1) arr

-- | Read report data from client socket and write it into the report queue
pullReports ::
  S.Socket ->
  TQueue Report ->
  IO ()
pullReports cs rq = do
  bytes <- N.recv cs 1024
  case C.length bytes of
    0 -> return ()
    _ -> reportBytes bytes rq >> pullReports cs rq

reportBytes ::
  C.ByteString ->
  TQueue Report ->
  IO ()
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
        '0' -> print Failure >> (writeTQueue rq Failure & atomically)
        '1' -> print Success >> (writeTQueue rq Success & atomically)
        _ -> print "Unknown"

recvReports ::
  S.SockAddr ->
  TQueue Report ->
  IO ()
recvReports sa rq =
  withSocket @"server"
    sa
    \ss ->
      mask (mkReporter ss)
        & forever
  where
    mkReporter ::
      S.Socket ->
      (IO () -> IO ()) ->
      IO ThreadId
    mkReporter ss restore = do
      (cs :: S.Socket, caddr :: S.SockAddr) <- S.accept ss
      forkFinally
        (pullReports cs rq & restore)
        \_ -> S.close cs
