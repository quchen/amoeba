{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Utilities (

      -- * Various utilities
        whenM
      , whileM
      , pluralS
      , mergeLists

      -- * Database-related functions
      , makeTimestamp
      , dbSize
      , nodeRelationship

      -- * Concurrency
      , asyncMany
      , toIO
      , toIO'
      , prepareOutputBuffers
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
      , catchAll
      -- (Re-exported definitions from in Utilities/Debug.hs)

      -- * Pipe-based communication channels
      , spawn
      , outputThread

) where



import           Control.Concurrent (threadDelay, ThreadId, forkIO)
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Applicative
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import           System.IO
import           System.Timeout

import           Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Concurrent as P
import qualified Network.Simple.TCP as N
import qualified Network.Socket.ByteString as NSB
import qualified Pipes.Binary as P
import Control.Monad.Catch (MonadCatch)

import Data.Binary

import Types
import Utilities.Debug



-- | Creates a timestamp, which is a Double representation of the Unix time.
makeTimestamp :: (MonadIO m) => m Timestamp
makeTimestamp = liftIO (Timestamp . realToFrac <$> getPOSIXTime)
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
connectToNode (To node) = N.connect (_host node)
                                    (show (_port node))



-- | 'Node'-based version of 'P.connectSock'. Opens the connection, but
--   contrary to 'connectToNode', it will not be closed automatically. Use
--   'P.closeSock' to do so.
connectToNode' :: (MonadIO io)
               => To
               -> io (N.Socket, N.SockAddr)
connectToNode' (To node) = N.connectSock (_host node)
                                         (show (_port node))



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
listenOnNode node = N.listen (N.Host (_host node))
                             (show   (_port node))



-- | Continuously encode and send data to a 'N.Socket'.
sender :: (MonadIO io, Binary b)
       => N.Socket
       -> Consumer b io ServerResponse
sender s = encodeMany >-> toSocketTimeout (3*10^6) s -- TODO: Don't hardcode timeout



-- | Same as "PN.toSocketTimeout", but returns "Timeout" instead of throwing an
--   "IOError".
toSocketTimeout :: (MonadIO io)
                => Int
                -> N.Socket
                -> Consumer BS.ByteString io ServerResponse
toSocketTimeout t socket = loop where
      loop = do
            bs <- await
            liftIO (timeout t (NSB.sendAll socket bs)) >>= \case
                  Just _  -> loop
                  Nothing -> return Timeout



-- | Continuously receive and decode data from a "N.Socket".
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



-- | Same as "PN.fromSocketTimeout", but issues "ServerResponse" instead of
--   throwing an "IOError".
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
encodeMany :: (Monad m, Binary x) => Pipe x BS.ByteString m r
encodeMany = for cat P.encode



-- | Send a message depending on the verbosity level.
toIO :: Environment
     -> Verbosity
     -> OutMsg
     -> STM ()
toIO env verbosity msg = when p (writeTBQueue (_getIOQueue (_io env))
                                              msg)
      where p = verbosity >= _verbosity (_config env)



-- | Send a message to an "IOQueue" directly (ignoring verbosity).
toIO' :: IOQueue
      -> OutMsg
      -> IO ()
toIO' ioq msg = atomically (writeTBQueue (_getIOQueue ioq) msg)



-- | Identical to "P.spawn'", but uses the typesfe "PChan" type instead of
--   "(,,)".
spawn :: P.Buffer a -> IO (PChan a)
spawn buffer = toPChan <$> P.spawn' buffer
      where toPChan (output, input, seal) = PChan output input seal



-- | Concurrently run multiple IO actions, and wait for the first "Async" to
--   complete. If one of them returns or throws, all others are "cancel"ed.
asyncMany :: [IO ()] -> IO ()
asyncMany [] = return ()
asyncMany (io:ios) = withAsync io $ \a -> do
      void (waitAnyCancel <$> mapM async ios)
      wait a
      -- TODO: Is this function even used?



-- | Set up the decicated IO thread. Forks said thread, and returns a "TBQueue"
--   to it, along with the "ThreadId" of the thread (which may be useful for
--   killing it).
--
--   Sends messages tagged as STDOUT to stdout
--                            STDERR to stderr
--                            STDLOG to stderr
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



-- | Prepares the output buffers for logging text by making them line-buffered.
--
-- (STDERR in particular is unbuffered by default.)
prepareOutputBuffers :: IO ()
prepareOutputBuffers = do hSetBuffering stdout LineBuffering
                          hSetBuffering stderr LineBuffering



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



-- | "MonadIO" version of "threadDelay".
delay :: MonadIO io => Int -> io ()
delay = liftIO . threadDelay



-- | Determine the current size of a database
dbSize :: Environment
       -> (Environment -> TVar (Map.Map k a)) -- _upstream or _downstream
       -> STM Int
dbSize env db = Map.size <$> readTVar (db env)



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




-- | Check whether a connection to a certain node is allowed. A node must not
--   connect to itself or to known neighbours multiple times.
--
--   Due to the fact that an "EdgeRequest" does not contain the upstream address
--   of the connection to be established, it cannot be checked whether the node
--   is already an upstream neighbour directly; timeouts will have to take care
--   of that.
nodeRelationship :: Environment
                 -> To
                 -> STM NodeRelationship
nodeRelationship env node
      | node == _self env = return IsSelf
      | otherwise = do isDS <- Map.member node <$> readTVar (_downstream env)
                       return (if isDS then IsDownstreamNeighbour
                                       else IsUnrelated)
