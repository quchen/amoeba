{-# LANGUAGE RankNTypes #-}

module Utilities (
        makeTimestamp
      , whileM
      , isContinue
      , catchAll

      -- * Concurrency
      , asyncMany
      , toIO

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

      -- * Pipe-based communication channels
      , spawn
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
import           Network (connectTo, PortID(PortNumber))
import qualified Data.ByteString as BS
import           System.IO
import           Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Concurrent as P
import qualified Pipes.Network.TCP as P
import qualified Pipes.Binary as P

import Data.Binary

import Types



-- | Creates a timestamp, which is a Double representation of the Unix time.
makeTimestamp :: (MonadIO m) => m Timestamp
makeTimestamp = liftIO $ Timestamp . realToFrac <$> getPOSIXTime
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



-- | Continuously receive and decode data.
receive' :: (MonadIO m, Binary a)
         => P.Socket
         -> Producer a m ()
receive' s = void (P.decodeMany (P.fromSocket s 4096)) >-> dataOnly
      where dataOnly = P.map snd



-- | Encode and send a single piece of data.
send' :: (MonadIO m, Binary a)
      => P.Socket
      -> a -- ^ Data to send
      -> Effect m ()
send' s x= P.encode x >-> P.toSocket s



-- | Encode and send a single piece of data, and receive and decode the
--   response.
request' :: (MonadIO m, Binary a, Binary b)
         => P.Socket
         -> a
         -> m (Maybe b)
request' s x =
      do runEffect (send' s x)
         P.head (receive' s)




-- | Specialized alias of 'receive\'' that sends only 'Signal's.
receive :: (MonadIO m)
        => P.Socket
        -> Producer Signal m ()
receive = receive'

-- | Specialized alias of 'send\'' that receives only 'Signal's.
send :: (MonadIO m)
     => P.Socket
     -> Signal -- ^ Data to send
     -> Effect m ()
send = send'

-- | Specialized alias of 'request\''. Types match what a typical communication
--   of a client with a downstream neighbour would involve.
request :: (MonadIO m)
        => P.Socket
        -> Signal
        -> m (Maybe ServerResponse)
request = request'



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


-- TODO: How to check whether a PChan is full?



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
