{-# LANGUAGE RankNTypes #-}

module Utilities (

      -- * Various utilities
        makeTimestamp
      , whenM
      , whileM
      , catchAll

      -- * Concurrency
      , asyncMany
      , toIO
      , delay

      -- * Networking
      , connectToNode
      , listenOnNode
      , sender
      , receiver
      , send
      , receive
      , request

      -- * Debugging
      , yell
      , yellAndRethrow

      -- * Pipe-based communication channels
      , spawn
      , getBroadcastOutput
      , outputThread
) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Applicative
import           Control.Exception
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString as BS
import qualified Data.Foldable as F

import           Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Concurrent as P
import qualified Pipes.Network.TCP as P
import qualified Pipes.Binary as P
import Control.Monad.Catch (MonadCatch)

import Data.Binary

import Types



-- | Creates a timestamp, which is a Double representation of the Unix time.
makeTimestamp :: (MonadIO m) => m Timestamp
makeTimestamp = liftIO $ Timestamp . realToFrac <$> getPOSIXTime
--   Since Haskell's Time library is borderline retarded, this seems to be the
--   cleanest way to get something that is easily an instance of Binary and
--   comparable to seconds.



-- | Monadic version of 'when'.
whenM :: Monad m => m Bool -> m () -> m ()
whenM mp m = mp >>= \p -> when p m



-- | Repeatedly executes a monadic action until its contents evaluate to False.
whileM :: Monad m => (a -> Bool) -> m a -> m ()
whileM p m = go
      where go = m >>= \x -> when (p x) go



-- | 'Node'-based version of 'P.connect'
connectToNode :: (MonadIO io, MonadCatch io)
              => To
              -> ((P.Socket, P.SockAddr) -> io r)
              -> io r
connectToNode (To node) = P.connect (_host node) (show $ _port node)



-- | 'Node'-based version of 'P.listen'
listenOnNode :: (MonadIO io, MonadCatch io)
             => Node
             -> ((P.Socket, P.SockAddr) -> io r)
             -> io r
listenOnNode node = P.listen (P.Host $ _host node)
                             (show   $ _port node)



-- | Continuously encode and send data to a 'P.Socket'.
sender :: (MonadIO io, Binary b)
       => P.Socket
       -> Consumer b io ()
sender s = encodeMany >-> P.toSocket s



-- | Continuously receive and decode data from a 'P.Socket'.
receiver :: (MonadIO io, Binary b)
         => P.Socket
         -> Producer b io ()
receiver s = void (P.decodeMany (P.fromSocket s 4096)) >-> dataOnly
      where dataOnly = P.map snd



-- TODO: Requester. Continuously send data downstream and gather results.



-- | Receives a single piece of data from a 'P.Socket'.
receive :: (MonadIO io, Binary b)
        => P.Socket
        -> io (Maybe b)
receive s = runEffect $ P.head (receiver s)



-- | Sends a single piece of data to a 'P.Socket'.
send :: (MonadIO io, Binary b)
     => P.Socket
     -> b
     -> io ()
send s x = runEffect $ yield x >-> sender s



-- | Sends a single piece of data to a 'P.Socket', and waits for a response.
request :: (MonadIO io, Binary a, Binary b)
        => P.Socket
        -> a
        -> io (Maybe b)
request s x = send s x >> receive s



-- | Continuously encodes the given 'Bin.Binary' instance and sends each result
--   downstream in 'BS.ByteString' chunks.
--
--   (Sent a pull request to Pipes-Binary for adding this.)
encodeMany :: (Monad m, Binary x) => Pipe x BS.ByteString m r
encodeMany = for cat P.encode



-- | Send an IO action depending on the verbosity level.
toIO :: Environment -> Verbosity -> IO () -> STM ()
toIO env verbosity = when p . writeTBQueue (_io env)
      where p = verbosity >= _verbosity (_config env)



-- | Mandatory silly catchall function. Intended to be used as a safety net
--   only, not as a cpeap getaway :-)
catchAll :: IO a -> IO ()
catchAll x = void x `catch` handler
      where handler :: SomeException -> IO ()
            handler _ = return ()

-- | Easily print colored text for debugging
yell :: MonadIO io => Int -> String -> io ()
yell n text = liftIO . putStrLn $
      "\ESC[" ++ show n ++ "m" ++ show n ++ " - " ++ text ++ "\ESC[0m"



-- | Identical to 'P.spawn\'', but uses the typesfe 'PChan' type instead of
--   '(,,)'.
spawn :: P.Buffer a -> IO (PChan a)
spawn buffer = toPChan <$> P.spawn' buffer
      where toPChan (output, input, seal) = PChan output input seal



-- | Concurrently run multiple IO actions, and wait for the first 'Async' to
--   complete. If one of them returns or throws, all others are 'cancel'ed.
asyncMany :: [IO ()] -> IO ()
asyncMany [] = return ()
asyncMany (io:ios) = withAsync io $ \a -> do
      void $ waitAnyCancel <$> mapM async ios
      wait a



-- | Retrieves all STSC (server-to-single-client) 'P.Output's and concatenates
--   them to a single broadcast channel.
getBroadcastOutput :: Environment
                   -> STM (P.Output NormalSignal)
getBroadcastOutput env =
      F.foldMap (_pOutput . _stsc) <$> readTVar (_downstream env)



-- | Dedicated (I)O thread to make sure messages aren't scrambled up. Reads
--   IO actions from a queue and executes them.
outputThread :: TBQueue (IO ()) -> IO ()
outputThread = forever . join . atomically . readTBQueue


-- | Catches all exceptions, 'yell's their contents, and rethrows them.
yellAndRethrow n = handle handler
      where handler :: SomeException -> IO ()
            handler (SomeException e) = yell n (show e) >> throw e



-- | 'MonadIO' version of 'threadDelay'.
delay :: MonadIO io => Int -> io ()
delay = liftIO . threadDelay