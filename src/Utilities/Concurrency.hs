-- | Functions for working with the concurrent parts of the program.

module Utilities.Concurrency (

      -- * General concurrency
        toIO
      , toIO'
      , prepareOutputBuffers
      , delay
      , timeout

      -- * Termination triggers
      , newTerminationTrigger
      , waitForTrigger
      , runTrigger
      , TerminationTrigger

      -- * Pipe-based communication channels
      , spawn
      , outputThread
) where


import           Control.Monad.Trans
import           Control.Concurrent hiding (yield)
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Functor
import           System.IO
import qualified System.Timeout


import qualified Pipes.Concurrent as P

-- import qualified Control.Lens as L
import           Control.Lens.Operators
import qualified Types.Lens as L

import           Types
import           Utilities.IOQueue




-- | Send a message depending on the verbosity level.
toIO :: Environment
     -> Verbosity
     -> OutMsg
     -> STM ()
toIO env verbosity msg = when p (writeTBQueue (env ^. L.io . L.ioQueue)
                                              msg)
      where p = verbosity >= env ^. L.config . L.verbosity



-- | Send a message to an "IOQueue" directly (ignoring verbosity).
toIO' :: IOQueue
      -> OutMsg
      -> IO ()
toIO' ioq msg = atomically (writeTBQueue (ioq ^. L.ioQueue) msg)



-- | Identical to 'P.spawn'', but uses the typesfe 'PChan' type instead of
--   @(,,)@.
spawn :: P.Buffer a -> IO (PChan a)
spawn buffer = toPChan <$> P.spawn' buffer
      where toPChan (output, input, seal) = PChan output input seal



-- | Used by the client pool. When the 'MVar' contained is filled, an arbitrary
--   node will be terminated.
newtype TerminationTrigger = TerminationTrigger (MVar ())



-- | Create a new termination trigger.
newTerminationTrigger :: IO TerminationTrigger
newTerminationTrigger = fmap TerminationTrigger newEmptyMVar



-- | Blocks until the trigger is triggered.
waitForTrigger :: TerminationTrigger -> IO ()
waitForTrigger (TerminationTrigger mVar) = takeMVar mVar



-- | Tries to trigger the termination trigger. Does nothing if it's currently
--   already triggered.
runTrigger :: TerminationTrigger -> IO ()
runTrigger (TerminationTrigger mVar) = void (tryPutMVar mVar ())



-- | Prepares the output buffers for logging text by making them line-buffered.
--
--   (STDERR in particular is unbuffered by default.)
prepareOutputBuffers :: IO ()
prepareOutputBuffers = do hSetBuffering stdout LineBuffering
                          hSetBuffering stderr LineBuffering



-- | Convert 'Integer' to 'Int', truncating if the input it too large.
maxBounded :: Integer -> Int
maxBounded x | x > fromIntegral maxInt = maxInt
             | otherwise               = fromIntegral x
             where maxInt = maxBound



-- | 'MonadIO' version of 'threadDelay'. Wait for a maximum of
--   @'maxBound' :: Int@ seconds, and truncates larger input.
delay :: MonadIO io => Microseconds -> io ()
delay (Microseconds us) = liftIO (threadDelay t)
      where t = maxBounded us



-- | 'System.IO.timeout' working with 'Microseconds'. Timeouts larger than
--   @'maxBound' :: Int@ will be truncated.
timeout :: Microseconds -> IO a -> IO (Maybe a)
timeout (Microseconds us) action = System.Timeout.timeout t action
      where t = maxBounded us