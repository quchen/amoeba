{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- | Networking functions.

module Utilities.Networking (
        connectToNode
      , listenOnNode
      , disconnect
      , sender
      , receiver
      , send
      , receive
      , request
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import qualified Data.ByteString as BS

import           Pipes
import qualified Network.Simple.TCP as N
import qualified Network.Socket.ByteString as NSB
import qualified Pipes.Binary as P
import qualified Pipes.Parse as P
import qualified Pipes.Prelude as P
import           Data.Binary

import qualified Control.Lens as L
import           Control.Lens.Operators
import qualified Types.Lens as L

import           Types
import           Utilities.Concurrency




-- | 'Node'-based version of 'P.connect'. Automatically closes when the
--   operation terminates or throws.
connectToNode :: (MonadIO io, MonadCatch io)
              => To
              -> ((N.Socket, N.SockAddr) -> io r)
              -> io r
connectToNode (To node) = N.connect (node ^. L.host)
                                    (node ^. L.port . L.to show)



-- | Close a connection.
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



-- | Continuously encode the given "Bin.Binary" instance and sends each result
--   downstream in "BS.ByteString" chunks.
--
--   (Sent a pull request to Pipes-Binary for adding this.)
encodeMany :: (Monad m, Binary x) => Pipe x BS.ByteString m ServerResponse
encodeMany = err <$ for cat P.encode
      --P.map (BSL.toStrict . Put.runPut . put)
      where err = Error "Encoding failure, likely a bug"
            -- TODO: Remove this case somehow



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
