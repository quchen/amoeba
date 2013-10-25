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

      -- * Sending/receiving network signals
      , connectToNode
      , send'
      , receive'
      , request'
      , encodeMany

      -- * Monomorphic aliases for type safety
      , send
      , receive
      , request

      -- * Debugging
      , yell

      -- * Pipe-based communication channels
      , spawn
      , getBroadcastOutput
) where

import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Exception (catch, SomeException)
import           Control.Monad
import           Control.Applicative
import           Control.Exception
import           Data.Functor
import           Data.Int
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString as BS
import           Data.Binary (Binary)
import           System.IO
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
connectToNode :: (MonadIO m, MonadCatch m)
              => To
              -> ((P.Socket, P.SockAddr) -> m r)
              -> m r
connectToNode (To node) = P.connect (_host node) (show $ _port node)





-- | Continuously receive and decode data.
receive' :: (MonadIO m, Binary a)
         => P.Socket
         -> Producer a m ()
receive' s = void (P.decodeMany (P.fromSocket s 4096)) >-> dataOnly
      where dataOnly = P.map snd



-- | Continuously encode and send data.
send' :: (MonadIO io, Binary a)
      => P.Socket
      -> Consumer a io ()
send' s = encodeMany >-> P.toSocket s



-- | Encode and send a single piece of data, and receive and decode the
--   response.
request' :: (MonadIO io, Binary a, Binary b)
         => P.Socket
         -> a
         -> io (Maybe b)
request' s x = runEffect (yield x >-> send' s) >> P.head (receive' s)




-- | Specialized alias of 'receive\'' that receives only 'Signal's.
receive :: (MonadIO m)
        => P.Socket
        -> Producer Signal m ()
receive = receive'

-- | Specialized alias of 'send\'' that sends only 'Signal's.
send :: (MonadIO m)
     => P.Socket
     -> Consumer Signal m ()
send = send'

-- | Specialized alias of 'request\''. Types match what a typical communication
--   of a client with a downstream neighbour would involve.
request :: (MonadIO m)
        => P.Socket
        -> Signal
        -> m (Maybe ServerResponse)
request = request'



-- | Continuously encodes the given 'Bin.Binary' instance and sends each result
--   downstream in 'BS.ByteString' chunks.
--
--   (Sent a pull request to Pipes-Binary for adding this.)
encodeMany :: (Monad m, Binary x) => Pipe x BS.ByteString m r
encodeMany = for cat P.encode



-- | Sends an IO action, depending on the verbosity level.
toIO :: Environment -> Verbosity -> IO () -> STM ()
toIO env verbosity = when p . writeTBQueue (_io env)
      where p = verbosity >= _verbosity (_config env)



-- | Mandatory silly catchall function. Intended to be used as a safety net
--   only, not as a cpeap getaway :-)
catchAll :: IO a -> IO ()
catchAll x = void x `catch` handler
      where handler :: SomeException -> IO ()
            handler e = return ()

-- | Easily print colored text for debugging
yell n text = putStrLn $ "\ESC[" ++ show n ++ "m" ++ show n ++ " - " ++ text ++ "\ESC[0m"



-- | Identical to 'P.spawn\'', but uses the typesfe 'PChan' type instead of
--   '(,,)'.
spawn :: P.Buffer a -> IO (PChan a)
spawn buffer = toPChan <$> P.spawn' buffer
      where toPChan (output, input, seal) = PChan output input seal




-- | Fork a number of IO actions using 'withAsync', and wait for the initial
--   one. Useful to fork many threads that should all be terminated if one of
--   them fails.
--
--   Runs the waiting function even if the action list is empty.
--
-- @
--   asyncMany [a, b, c] wait'
--   =
--   withAsync a $ \thread ->
--    withAsync b $ \_ ->
--     withAsync c $ \_ ->
--      wait' thread
--
--   asyncMany [] wait'
--   =
--   withAsync (return ()) wait'
-- @
asyncMany :: [IO ()] -- ^ Actions to run asynchronously
          -> (Async () -> IO ()) -- ^ "Waiting action" to apply to the first
                                 --   asynchronous action of the list in the
                                 --   end. Typically involves 'wait'.
          -> IO ()
asyncMany []     wait' = withAsync (return ()) wait'
asyncMany (x:xs) wait' = withAsync x $ \t -> asyncMany' xs >> wait' t
      where asyncMany' = foldr asyncForget (return ())
            -- Fork a thread and "forget" about it. Safety comes from the
            -- outermost wrapper: if it fails, the whole hierarchy collapses.
            asyncForget x xs = withAsync x $ \_ -> xs



-- | Retrieves all STSC (server-to-single-client) 'P.Output's and concatenates
--   them to a single broadcast channel.
getBroadcastOutput :: Environment
                   -> STM (P.Output NormalSignal)
getBroadcastOutput env =
      F.foldMap (_pOutput . _stsc) <$> readTVar (_downstream env)