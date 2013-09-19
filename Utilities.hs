{-# LANGUAGE LambdaCase #-}

module Utilities (
        makeTimestamp
      , untilTerminate
      , toIO
      , connectToNode

      -- * Sending/receiving network signals
      , send'
      , receive'
      , request'

      -- * Monomorphic aliases for type safety
      , send
      , receive
      , request
) where

import Data.Functor
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Int
import System.IO
import System.Timeout
import Control.Concurrent.STM
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


-- | Intended to replace 'untilTerminate' to allow more flexible return values
--   for looping functions
untilM :: Monad m => (a -> Bool) -> m a -> m ()
untilM p m = go
      where go = m >>= \x -> when (p x) go



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
      mLength <- int64toInt . decode <$> BS.hGet h int64Size

      -- Read the previously determined amount of data
      decode <$> BS.hGet h mLength

      -- TODO: Handle decoding errors (Maybe?)

-- | Sends a Signal/ServerResponse, encoded as Binary with a size header, to a
--   Handle. Inverse of 'receive'.
send' :: Binary a => Handle -> a -> IO ()
send' h message = do
      let mSerialized = encode message
          mLength = encode (BS.length mSerialized :: Int64)
      BS.hPut h mLength
      BS.hPut h mSerialized
      hFlush h

{-# DEPRECATED send, receive
      "Use request to avoid unhandled server answers" #-}
{-# DEPRECATED send', receive'
      "Use request' to avoid unhandled server answers" #-}

-- | Sends out a signal and waits for an answer. Combines 'send\'' and
--   'receive\'' in order to avoid unhandled server responses.
request' :: (Binary a, Binary b) => Handle -> a -> IO b
request' h message = send' h message >> receive' h



receive :: Handle -> IO Signal
receive = receive'

send :: Handle -> Signal -> IO ()
send = send'

request :: Handle -> Signal -> IO ServerResponse
request = request'


-- | Very hacky timeout function. Crashes on timeout. :-x
raceAgainstTimeout :: IO a -> IO a
raceAgainstTimeout action = do
      result <- timeout (10^6) action
      case result of
            Just r -> return r
            Nothing -> undefined
--   TODO: Make timeout more useful, especially: remove undefined
{-# WARNING raceAgainstTimeout
      "Function crashes on timeout, this is just for debugging" #-}


-- | Sends an IO action, depending on the verbosity level.
toIO :: Environment -> Verbosity -> IO () -> STM ()
toIO env verbosity = when p . writeTBQueue (_io env)
      where p = verbosity >= _verbosity (_config env)


-- | Like Network.connectTo, but extracts the connection data from a @Node@
--   object.
connectToNode :: To -> IO Handle
connectToNode (To n) = connectTo (_host n) (PortNumber (_port n))
