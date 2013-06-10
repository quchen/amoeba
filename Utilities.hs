{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module Utilities (
        makeTimestamp
      , untilTerminate
      , receive
      , send
      , toIO
      , debugError
) where

import Data.Functor
import qualified Data.ByteString.Lazy as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Int
import System.IO
import Control.Concurrent.STM

import Data.Binary

import Types

-- | Creates a timestamp, which is a Double representation of the Unix time.
makeTimestamp :: IO Timestamp
makeTimestamp = Timestamp . realToFrac <$> getPOSIXTime
--   Since Haskell's Time library is borderline retarded, this seems to be the
--   cleanest way to get something that is easily an instance of Binary and
--   comparable to seconds.


-- | Similar to @Control.Monad.forever@, but will abort when @False@ is
--   returned.
untilTerminate :: Monad m => m Proceed -> m ()
untilTerminate m = go
      where go = m >>= \case Continue  -> go
                             Terminate -> return ()


-- | For debugging. This function must not appear in production code.
debugError x = error $ "Debug error: " ++ x




-- | Receives a Signal, encoded as Binary with a size header, from a Handle.
--   Inverse of 'send'.
receive :: Handle -> IO Signal
receive h = do
      let int2int = fromIntegral :: Int64 -> Int
          int64Size = BS.length $ encode (0 :: Int64)
      -- Read length of the data first
      sLength <- decode <$> BS.hGet h (int2int int64Size)
      -- Read the previously determined amount of data
      decode <$> BS.hGet h (int2int sLength)



-- | Sends a Signal, encoded as Binary with a size header, to a Handle.
--   Inverse of 'receive'.
send :: Handle -> Signal -> IO ()
send h signal = do
      let sBinary = encode signal
          sLength = encode (BS.length sBinary :: Int64)
      BS.hPut h sLength
      BS.hPut h sBinary
      hFlush h


-- | Sends an IO action to the IO thread.
toIO :: NodeEnvironment -> IO () -> STM ()
toIO env = writeTBQueue (_io env)