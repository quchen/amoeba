{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Utilities (

      -- * Various utilities
        makeTimestamp
      , whenM
      , whileM
      , catchAll
      , dbSize
      , pluralS

      -- * Concurrency
      , asyncMany
      , toIO
      , delay

      -- * Networking
      , connectToNode
      , connectToNode'
      , listenOnNode
      , disconnect
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
import           Data.Map (Map)
import qualified Data.Map as Map
import           System.Timeout

import           Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Concurrent as P
import qualified Pipes.Network.TCP as PN
import qualified Network.Simple.TCP as N
import qualified Network.Socket.ByteString as NSB
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



-- | 'Node'-based version of 'P.connect'. Automatically closes when the
--   operation terminates or throws.
connectToNode :: (MonadIO io, MonadCatch io)
              => To
              -> ((N.Socket, N.SockAddr) -> io r)
              -> io r
connectToNode (To node) = N.connect (_host node) (show $ _port node)


-- | 'Node'-based version of 'P.connectSock'. Opens the connection, but
--   contrary to 'connectToNode', it will not be closed automatically. Use
--   'P.closeSock' to do so.
connectToNode' :: (MonadIO io)
               => To
               -> io (N.Socket, N.SockAddr)
connectToNode' (To node) = N.connectSock (_host node) (show $ _port node)



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
listenOnNode node = N.listen (N.Host $ _host node)
                             (show   $ _port node)



-- | Continuously encode and send data to a 'N.Socket'.
sender :: (MonadIO io, Binary b)
       => N.Socket
       -> Consumer b io ServerResponse
sender s = encodeMany >-> toSocketTimeout (3*10^6) s -- TODO: Don't hardcode timeout



-- | Same as 'PN.toSocketTimeout', but returns '()' instead of throwing an
--   'IOError' on timeout.
toSocketTimeout :: (MonadIO io)
                => Int
                -> N.Socket
                -> Consumer BS.ByteString io ServerResponse
toSocketTimeout t socket = loop where
      loop = do
            bs <- await
            liftIO (timeout t (NSB.sendAll socket bs)) >>= \case
                  Just _ -> loop
                  Nothing -> return Timeout



-- | Continuously receive and decode data from a 'N.Socket'.
--
--   Returns if the connection is closed, times out, or decoding fails.
receiver :: (MonadIO io, Binary b)
         => N.Socket
         -> Producer b io ServerResponse
receiver s = decoded >-> dataOnly
      where dataOnly = P.map snd
            input = fromSocketTimeout (3*10^6) s 4096 -- TODO: Don't hardcode timeout

            decoded = decodeError <$> P.decodeMany input

            decodeError (Right r) = r
            decodeError (Left _)  = DecodeError



-- | Same as 'PN.fromSocketTimeout', but issues 'ServerResponse' instead of
--   throwing an 'IOError'.
fromSocketTimeout :: (MonadIO io)
                  => Int
                  -> N.Socket
                  -> Int
                  -> Producer' BS.ByteString io ServerResponse
fromSocketTimeout t socket nBytes = loop where
      loop = liftIO (timeout t (N.recv socket nBytes)) >>= \case
            Just (Just bs) -> yield bs >> loop
            Just Nothing   -> return ConnectionClosed
            Nothing        -> return Timeout



-- TODO: Requester. Continuously send data downstream and gather results.



-- | Receives a single piece of 'Binary' data from a 'N.Socket'.
receive :: (MonadIO io, Binary b)
        => N.Socket
        -> io (Maybe b)
receive s = runEffect $ (P.head . void . receiver) s



-- | Sends a single piece of data to a 'N.Socket'.
send :: (MonadIO io, Binary b)
     => N.Socket
     -> b
     -> io ()
send s x = runEffect $ yield x >-> void (sender s)



-- | Sends a single piece of data to a 'N.Socket', and waits for a response.
request :: (MonadIO io, Binary a, Binary b)
        => N.Socket
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



-- | Set up the decicated IO thread. Forks said thread, and returns a 'TBQueue'
--   to it, along with the 'Async' of the thread (which may be useful for
--   cancelling it).
outputThread :: Int                  -- ^ Thread size
             -> IO ( TBQueue (IO ()) -- Channel
                   , Async ()        -- Async of the printer thread
                   )
outputThread size = do
      q <- newTBQueueIO size
      thread <- async $ (forever . join . atomically . readTBQueue) q
      return (q, thread)


-- | Catches all exceptions, 'yell's their contents, and rethrows them.
yellAndRethrow :: (MonadIO io) => Int -> IO () -> io ()
yellAndRethrow n = liftIO . handle handler
      where handler :: SomeException -> IO ()
            handler (SomeException e) = yell n (show e) >> throw e



-- | 'MonadIO' version of 'threadDelay'.
delay :: MonadIO io => Int -> io ()
delay = liftIO . threadDelay



-- | Determine the current size of a database
dbSize :: Environment
       -> (Environment -> TVar (Map.Map k a)) -- '_upstream' or '_downstream'
       -> STM Int
dbSize env db = Map.size <$> readTVar (db env)


-- | Add an \"s\" in print statements
pluralS :: (Eq a, Num a) => a -> String
pluralS 1 = ""
pluralS _ = "s"
