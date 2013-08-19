{-# LANGUAGE LambdaCase #-}

module Utilities (
        makeTimestamp
      , untilTerminate
      , receive
      , send
      , receive'
      , send'
      , toIO
      , connectToNode
      , debug
) where

import Data.Functor
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Int
import System.IO
import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import System.Timeout
import Control.Concurrent.STM
import Data.Maybe (fromJust)
import Network (connectTo, PortID(PortNumber))

import Data.Binary

import Types

-- | Creates a timestamp, which is a Double representation of the Unix time.
makeTimestamp :: IO Timestamp
makeTimestamp = Timestamp . realToFrac <$> getPOSIXTime
--   Since Haskell's Time library is borderline retarded, this seems to be the
--   cleanest way to get something that is easily an instance of Binary and
--   comparable to seconds.


-- | Similar to @Control.Monad.forever@, but will abort when @Terminate@ is
--   returned.
untilTerminate :: Monad m => m Proceed -> m ()
untilTerminate m = go
      where go = m >>= \case Continue  -> go
                             Terminate -> return ()





-- | Receives a Signal, encoded as Binary with a size header, from a Handle.
--   Inverse of 'send'.
--
--   **Note:** receiving limits individual incoming requests with a hard cutoff,
--   currently Int64 bytes.
receive' :: Binary a => Handle -> IO a
receive' h = do

      -- TODO: Add timeout to prevent Slowloris

      let -- Convert Int64 to Int. Since the messages are (hopefully) shorter
          -- than maxBound::Int, this should not be a problem.
          -- TODO: Ensure the above
          int64toInt = fromIntegral :: Int64 -> Int
          -- Size of an encoded Int64 in bytes
          int64Size :: Int
          int64Size = int64toInt . BS.length $ encode (maxBound :: Int64)

      -- Read message header = length of the incoming signal
      debug $ print "getting"
      mLength <- int64toInt . decode <$> BS.hGet h int64Size

      -- Read the previously determined amount of data
      r <- decode <$> BS.hGet h mLength

      debug $ print "getting done"

      return r

      -- TODO: Handle decoding errors (Maybe?)

-- | Monomorphic aliase for type safety
receive :: Handle -> IO Signal
receive = receive'

-- | Monomorphic aliase for type safety
send :: Handle -> Signal -> IO ()
send = send'


-- | Sends a Signal/ServerResponse, encoded as Binary with a size header, to a
--   Handle. Inverse of 'receive'.
send' :: Binary a => Handle -> a -> IO ()
send' h message = do
      debug $ putStrLn "sending"
      let mSerialized = encode message
          mLength = encode (BS.length mSerialized :: Int64)
      BS.hPut h mLength
      BS.hPut h mSerialized
      debug $ putStrLn "sent"
      hFlush h

-- | Very hacky timeout function. Crashes on timeout. :-x
raceAgainstTimeout :: IO a -> IO a
raceAgainstTimeout action = do
      result <- timeout (10^6) action
      case result of
            Just r -> return r
            Nothing -> debug (putStrLn "TIMEOUT") >> undefined
--   TODO: Make timeout more useful


-- | Sends an IO action, depending on the verbosity level.
toIO :: Environment -> Verbosity -> IO () -> STM ()
toIO env verbosity = when p . writeTBQueue (_io env)
      where p = verbosity >= _verbosity (_config env)


-- | Like Network.connectTo, but extracts the connection data from a @Node@
--   object.
connectToNode :: Node -> IO Handle
connectToNode n = connectTo (_host n) (PortNumber (_port n))




-- Debugging function. Delete to make the type system tell you where to clean up
debug :: a -> a
debug = id