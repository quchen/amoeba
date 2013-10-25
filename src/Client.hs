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
import Data.Monoid
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
       -> PChan NormalSignal
       -> io ()
client env socket to stsc = runEffect $
      P.fromInput input >-> signalH env socket to

      where st1c   = _st1c   env
            input  = mconcat [_pInput st1c, _pInput stsc]
            -- TODO: Implement stc to support flood messages
-- TODO: send shutdown notice on termination




signalH :: (MonadIO io)
        => Environment
        -> Socket
        -> To
        -> Consumer NormalSignal io ()
signalH env socket to = go
      where terminate = return ()
            go = do signal <- await
                    response <- request socket (Normal signal)
                    case response of
                          Just OK     -> ok           env to >> go
                          Just Error  -> genericError env    >> terminate
                          Just Ignore -> ignore       env    >> terminate
                          Just Denied -> denied       env    >> terminate
                          Nothing     -> noResponse   env    >> terminate



-- | Response to sending a signal to a server successfully. (Updates the "last
--   successfully sent signal to" timestamp)
ok :: (MonadIO io)
   => Environment
   -> To          -- ^ Target downstream neighbour
   -> io ()
ok env node = liftIO $ do
      timestamp <- makeTimestamp
      let updateTimestamp client = client { _clientTimestamp = timestamp }
      atomically $ modifyTVar (_downstream env) $
            Map.adjust updateTimestamp node



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
       -> io ()
ignore env = liftIO $ do
      atomically . toIO env Debug $
            putStrLn "Server ignores this node, terminating client"



-- | Server sent back a generic error, see docs for 'Error'
genericError :: (MonadIO io)
             => Environment
             -> io ()
genericError env = liftIO $ do
      atomically . toIO env Debug $
            putStrLn "Generic server error, terminating client"



-- | Server denied a valid request, see docs for 'Denied'
denied :: (MonadIO io)
       => Environment
       -> io ()
denied env = liftIO $ do
      atomically . toIO env Debug $
            putStrLn "Server denied the request"



-- | Server did not respond
noResponse :: (MonadIO io)
           => Environment
           -> io ()
noResponse env = liftIO $ do
      atomically . toIO env Debug $
            putStrLn "Server did not respond"
