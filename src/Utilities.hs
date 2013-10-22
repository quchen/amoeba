module Utilities (
        makeTimestamp
      , toIO
      , connectToNode
      , whileM
      , isContinue
      , catchAll

      -- * Sending/receiving network signals
      , send'
      , receive'
      , request'

      -- * Monomorphic aliases for type safety
      , send
      , receive
      , request

      -- * Debugging
      , yell
      , assertNotFull
) where

import           Control.Concurrent.STM
import           Control.Exception (catch, SomeException)
import           Control.Monad
import           Control.Applicative
import           Control.Exception
import           Data.Functor
import           Data.Int
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Network (connectTo, PortID(PortNumber))
import qualified Data.ByteString.Lazy as BS
import           System.IO

import Data.Binary

import Types



-- | Creates a timestamp, which is a Double representation of the Unix time.
makeTimestamp :: IO Timestamp
makeTimestamp = Timestamp . realToFrac <$> getPOSIXTime
--   Since Haskell's Time library is borderline retarded, this seems to be the
--   cleanest way to get something that is easily an instance of Binary and
--   comparable to seconds.



-- | Repeatedly executes a monadic action until its contents evaluate to False.
whileM :: Monad m => (a -> Bool) -> m a -> m ()
whileM p m = go
      where go = m >>= \x -> when (p x) go



isContinue :: Proceed -> Bool
isContinue Continue = True
isContinue _        = False



-- | Receives a Signal, encoded as Binary with a size header, from a Handle.
--   Inverse of 'send'.
--
--   **Note:** receiving limits individual incoming requests with a hard cutoff,
--   currently (maxBound :: Int64) bytes.
receive' :: Binary a => Handle -> IO a
receive' h = do

      -- TODO: Add timeout to prevent Slowloris

      -- Read message header = length of the incoming signal
      let decodeToInt = (fromIntegral :: Int64 -> Int) . decode
      mLength <- decodeToInt <$> BS.hGet h int64Size

      -- Read the previously determined amount of data
      decode <$> BS.hGet h mLength

      -- TODO: Handle decoding errors (Maybe?)

-- | Size of an encoded Int64 in bytes.
int64Size :: Int
int64Size = fromIntegral . BS.length $ encode (maxBound :: Int64)

-- | Sends a Signal/ServerResponse, encoded as Binary with a size header, to a
--   Handle. Inverse of 'receive'.
send' :: Binary a => Handle -> a -> IO ()
send' h message = do
      let mSerialized = encode message
          mLength = encode (BS.length mSerialized :: Int64)
      BS.hPut h mLength
      BS.hPut h mSerialized
      hFlush h

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



-- | Sends an IO action, depending on the verbosity level.
toIO :: Environment -> Verbosity -> IO () -> STM ()
toIO env verbosity = when p . writeTBQueue (_io env)
      where p = verbosity >= _verbosity (_config env)



-- | Like Network.connectTo, but extracts the connection data from a @Node@
--   object.
connectToNode :: To -> IO Handle
connectToNode (To n) = connectTo (_host n) (PortNumber (_port n))



-- | Mandatory silly catchall function. Intended to be used as a safety net
--   only, not as a cpeap getaway :-)
catchAll :: IO a -> IO ()
catchAll x = void x `catch` handler
      where handler :: SomeException -> IO ()
            handler e = return ()

-- | Easily print colored text for debugging
yell n text = putStrLn $ "\ESC[" ++ show n ++ "m" ++ show n ++ " - " ++ text ++ "\ESC[0m"


-- | Check whether a 'TBQueue' is full. Used for debugging. DEBUG
isFullTBQueue :: TBQueue a -> STM Bool
isFullTBQueue q = (unGetTBQueue q undefined >> readTBQueue q >> pure False) <|> pure True

assertNotFull :: TBQueue a -> STM ()
assertNotFull q = do
      full <- isFullTBQueue q
      assert (not full) $ return ()