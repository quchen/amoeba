{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Utilities.IOQueue (
        IOQueue(..)
      , OutMsg(..)
      , outputThread
) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           System.IO


-- | Wrapper around the queue to the output thread
newtype IOQueue = IOQueue { _ioQueue :: TBQueue OutMsg }

-- | Used to send a message to the terminal via 'IOQueue's.
data OutMsg = STDOUT String
            | STDERR String
            | STDLOG String

-- | Set up the decicated IO thread. Forks said thread, and returns a "TBQueue"
--   to it, along with the "ThreadId" of the thread (which may be useful for
--   killing it).
--
--   Sends messages tagged as 'STDOUT' to stdout,
--                            'STDERR' to stderr, and
--                            'STDLOG' to stderr.
--
--   Note: This does not change the buffering behaviour of STDERR, which is
--         unbuffered by default.
outputThread :: Int           -- ^ Thread size
             -> IO ( IOQueue  -- Channel
                   , ThreadId -- Thread ID of the spawned printer thread
                   )
outputThread size = do
      checkOutputBuffers
      q <- newTBQueueIO size
      thread <- forkIO (dispatchSignals q)
      return (IOQueue q, thread)

      where dispatchSignals q = forever $ atomically (readTBQueue q) >>= \case
                  STDOUT s -> hPutStrLn stdout s
                  STDERR s -> hPutStrLn stderr s
                  STDLOG s -> hPutStrLn stderr s



-- | Check whether the output buffers are set properly, i.e. are buffered.
checkOutputBuffers :: IO ()
checkOutputBuffers = do

      let err buffer = hPutStr stderr (buffer ++ " unbuffered! You may want to\
                                       \ change it to buffered for performance\
                                       \ reasons (e.g. using \
                                       \ Utilities.prepareOutputBuffers).")

      hGetBuffering stdout >>= \case
            NoBuffering -> err "STDOUT"
            _else       -> return ()

      hGetBuffering stderr >>= \case
            NoBuffering -> err "STDERR"
            _else       -> return ()