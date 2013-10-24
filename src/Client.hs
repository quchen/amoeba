-- | A client represents one connection to a downstream node.
--

{-# LANGUAGE LambdaCase #-}


module Client  (
      client
) where




import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception
import System.IO
import System.Timeout
import Data.Functor
import Control.Monad
import qualified Data.Map as Map

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Concurrent as P
import Pipes.Network.TCP (Socket)



import Types
import Utilities


-- | Start a new client
client :: (MonadIO io)
       => Environment
       -> Socket -- ^ Connection to use (created by the handshake process)
       -> To -- ^ Node the Socket connects to. Only used for bookkeeping, in
             --   order to keep the client pool up to date.
       -> io ()
client env socket to = runEffect $
      P.fromInput input >-> signalH env socket to

      where st1c   = _st1c    env
            input  = _pInput  st1c
            -- TODO: Implement stc to support flood messages
-- TODO: send shutdown notice on termination




signalH :: (MonadIO io)
        => Environment
        -> Socket
        -> To
        -> Consumer NormalSignal io ()
signalH env socket to = go
      where go = do

            signal <- await

            proceed <- lift $ do
                  response <- request socket (Normal signal)
                  case response of
                        Just OK     -> ok           env to
                        Just Error  -> genericError env
                        Just Ignore -> ignore       env
                        Just Denied -> denied       env
                        Nothing     -> noResponse   env

            case proceed of
                  Terminate -> return ()
                  Continue  -> go



-- | Response to sending a signal to a server successfully. (Updates the "last
--   successfully sent signal to" timestamp)
ok :: (MonadIO io)
   => Environment
   -> To          -- ^ Target downstream neighbour
   -> io Proceed
ok env node = liftIO $ do
      timestamp <- makeTimestamp
      let updateTimestamp client = client { _clientTimestamp = timestamp }
      atomically $ modifyTVar (_downstream env) $
            Map.adjust updateTimestamp node
      return Continue



-- | A downstream node has received a signal from this node without having it
--   in its list of upstream neighbours. A a result, it tells the issuing client
--   that it will ignore its requests.
--
--   The purpose of this is twofold:
--
--     - Nodes can only be contacted by other registered nodes. A malicious
--       network of other nodes cannot nuke a node with illegal requests,
--       because it will just ignore all the illegally created ones.
--
--     - If a node doesn't send a signal for too long, it will time out. When it
--       starts sending new signals, it will be told that it was dropped.
ignore :: (MonadIO io)
       => Environment
       -> io Proceed
ignore env = liftIO $ do
      atomically . toIO env Debug $
            putStrLn "Server ignores this node, terminating client"
      return Terminate



-- | Server sent back a generic error, see docs for 'Error'
genericError :: (MonadIO io)
             => Environment
             -> io Proceed
genericError env = liftIO $ do
      atomically . toIO env Debug $
            putStrLn "Generic server error, terminating client"
      return Terminate



-- | Server denied a valid request, see docs for 'Denied'
denied :: (MonadIO io)
       => Environment
       -> io Proceed
denied env = liftIO $ do
      atomically . toIO env Debug $
            putStrLn "Server denied the request"
      return Terminate



-- | Server did not respond
noResponse :: (MonadIO io)
           => Environment
           -> io Proceed
noResponse env = liftIO $ do
      atomically . toIO env Debug $
            putStrLn "Server did not respond"
      return Terminate