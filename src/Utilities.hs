{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Utilities (

        module Utilities.Debug
      , module Utilities.Databases

      -- * Various utilities
      , whenM
      , ifM
      , whileM
      , pluralS
      , mergeLists
      , showT
      , seqM
      , iSqrt

      -- * Concurrency
      , toIO
      , toIO'
      , prepareOutputBuffers
      , delay
      , timeout
      , newTerminationTrigger

      -- * Networking
      , connectToNode
      , listenOnNode
      , disconnect
      , sender
      , receiver
      , send
      , receive
      , request

      -- * Pipe-based communication channels
      , spawn
      , outputThread

) where



import           Control.Applicative
import           Control.Concurrent hiding (yield)
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString as BS
import           System.IO
import qualified System.Timeout

import           Pipes
import qualified Network.Simple.TCP as N
import qualified Network.Socket.ByteString as NSB
import qualified Pipes.Binary as P
import qualified Pipes.Concurrent as P
import qualified Pipes.Parse as P
import qualified Pipes.Prelude as P


import           Control.Monad.Catch (MonadCatch)
import           Data.Binary
import qualified Data.Text as T

import qualified Control.Lens as L
import           Control.Lens.Operators
import qualified Types.Lens as L

import           Types
import           Utilities.Debug
import           Utilities.Databases
import           Utilities.IOQueue



-- | Monadic version of 'when'.
whenM :: Monad m => m Bool -> m () -> m ()
whenM mp m = mp >>= \p -> when p m



-- | Monadic version of 'IfThenElse'.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mp x y = mp >>= \p -> if p then x else y



-- | Repeatedly executes a monadic action until its contents evaluate to False.
whileM :: Monad m => (a -> Bool) -> m a -> m ()
whileM p m = go
      where go = m >>= \x -> when (p x) go



-- | 'Node'-based version of 'P.connect'. Automatically closes when the
--   operation terminates or throws.
connectToNode :: (MonadIO io, MonadCatch io)
              => To
              -> ((N.Socket, N.SockAddr) -> io r)
              -> io r
connectToNode (To node) = N.connect (node ^. L.host)
                                    (node ^. L.port . L.to show)



-- | Closes a connection.
disconnect :: (MonadIO io)
           => N.Socket
           -> io ()
disconnect s = liftIO (N.closeSock s)



-- | 'Node'-based version of 'P.listen'
listenOnNode :: (MonadIO io, MonadCatch io)
             => Node
             -> ((N.Socket, N.SockAddr) -> io r)
             -> io r
listenOnNode node = N.listen (node ^. L.host . L.to N.Host)
                             (node ^. L.port . L.to show  )



-- | Continuously encode and send data to a 'N.Socket'.
sender :: (MonadIO io, Binary b)
       => N.Socket
       -> Consumer b io ServerResponse
sender s = encodeMany >-> toSocketTimeout 3e6 s -- TODO: Don't hardcode timeout



-- | Same as 'PN.toSocketTimeout', but returns 'Timeout' instead of throwing an
--   "IOError".
toSocketTimeout :: (MonadIO io)
                => Microseconds -- ^ Timeout
                -> N.Socket
                -> Consumer BS.ByteString io ServerResponse
toSocketTimeout t socket = loop where
      loop = do bs <- await
                liftIO (timeout t (NSB.sendAll socket bs)) >>= \case
                      Just _  -> loop
                      Nothing -> return Timeout



-- | Continuously receive and decode data from a 'N.Socket'.
--
--   Returns if the connection is closed, times out, or decoding fails.
receiver :: (MonadIO io, Binary b)
         => N.Socket
         -> Producer b io ServerResponse
receiver s = decoded where

      input = fromSocketTimeout 3e6 s 4096 -- TODO: Don't hardcode timeout

      decoded = P.evalStateT (L.zoom P.decoded P.draw) input >>= \case
            Nothing -> return DecodeError
            Just x  -> yield x >> decoded



-- | Same as 'PN.fromSocketTimeout', but issues 'ServerResponse' instead of
--   throwing an "IOError".
fromSocketTimeout :: (MonadIO io)
                  => Microseconds -- ^ Timeout
                  -> N.Socket
                  -> Int -- ^ Number of bytes to read at once
                  -> Producer' BS.ByteString io ServerResponse
fromSocketTimeout t socket nBytes = loop where
      loop = liftIO (timeout t (N.recv socket nBytes)) >>= \case
            Just (Just bs) -> yield bs >> loop
            Just Nothing   -> return ConnectionClosed
            Nothing        -> return Timeout



-- TODO: Requester. Continuously send data downstream and gather results.



-- | Receives a single piece of "Binary" data from a "N.Socket".
receive :: (MonadIO io, Binary b)
        => N.Socket
        -> io (Maybe b)
receive s = runEffect ((P.head . void . receiver) s)



-- | Sends a single piece of data to a "N.Socket".
send :: (MonadIO io, Binary b)
     => N.Socket
     -> b
     -> io ()
send s x = runEffect (yield x >-> void (sender s))



-- | Sends a single piece of data to a "N.Socket", and waits for a response.
request :: (MonadIO io, Binary a, Binary b)
        => N.Socket
        -> a
        -> io (Maybe b)
request s x = send s x >> receive s



-- | Continuously encodes the given "Bin.Binary" instance and sends each result
--   downstream in "BS.ByteString" chunks.
--
--   (Sent a pull request to Pipes-Binary for adding this.)
encodeMany :: (Monad m, Binary x) => Pipe x BS.ByteString m ServerResponse
encodeMany = err <$ for cat P.encode
      --P.map (BSL.toStrict . Put.runPut . put)
      where err = Error "Encoding failure, likely a bug"
            -- TODO: Remove this case somehow



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



-- | Identical to "P.spawn'", but uses the typesfe "PChan" type instead of
--   "(,,)".
spawn :: P.Buffer a -> IO (PChan a)
spawn buffer = toPChan <$> P.spawn' buffer
      where toPChan (output, input, seal) = PChan output input seal



newTerminationTrigger :: IO TerminationTrigger
newTerminationTrigger = fmap TerminationTrigger newEmptyMVar



-- | Prepares the output buffers for logging text by making them line-buffered.
--
-- (STDERR in particular is unbuffered by default.)
prepareOutputBuffers :: IO ()
prepareOutputBuffers = do hSetBuffering stdout LineBuffering
                          hSetBuffering stderr LineBuffering



-- | Convert 'Integer' to 'Int', truncating if the input it too large.
maxBounded :: Integer -> Int
maxBounded x | x > fromIntegral maxInt = maxInt
             | otherwise               = fromIntegral x
             where maxInt = maxBound



-- | "MonadIO" version of "threadDelay". Waits for a maximum of
--   @'maxBound' :: Int@ seconds, and truncates larger input.
delay :: MonadIO io => Microseconds -> io ()
delay (Microseconds us) = liftIO (threadDelay t)
      where t = maxBounded us



-- | 'System.IO.timeout' working with 'Microseconds'. Timeouts larger than
--   @'maxBound' :: Int@ will be truncated.
timeout :: Microseconds -> IO a -> IO (Maybe a)
timeout (Microseconds us) action = System.Timeout.timeout t action
      where t = maxBounded us



-- | To add an \"s\" in print statements if the first argument is 1.
--
--   >>> printf "%d minute%s remaining" n (pluralS n)
pluralS :: (Eq a, Num a) => a -> String
pluralS 1 = ""
pluralS _ = "s"



-- | Merges two lists by alternatingly taking one element of each. Overflow is
--   appended as bulk.
--
--   > mergeLists [a,b] [w,x,y,z]  ==  [a,w,b,x,y,z]
mergeLists :: [a] -> [a] -> [a]
mergeLists []     ys = ys
mergeLists (x:xs) ys = x : mergeLists ys xs



-- | 'T.Text' version of 'show'.
showT :: Show a => a -> T.Text
showT = T.pack . show



-- | Strictify monadic values
seqM :: Monad m => m a -> m a
seqM m = m >>= (return $!)


-- | Reasonably accurate version of an integer square root, used to model
--   diminishing returns for small values (e.g. neighbour counts).
--
--   This function is total, and returns 0 for inputs smaller than zero.
iSqrt :: Integral int => int -> int
iSqrt n = round (sqrt (max 0 (fromIntegral n :: Double)))