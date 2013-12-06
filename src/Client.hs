-- | A client represents one connection to a downstream node.
--

{-# LANGUAGE LambdaCase #-}


module Client  (
      startHandshakeH
) where




import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Monoid
import Control.Exception (finally)
import qualified Data.Map as Map
import System.Timeout
import Control.Monad
import Control.Exception
import Control.Applicative

import Pipes
import qualified Pipes.Concurrent as P
import Pipes.Network.TCP (Socket)



import Types
import ClientPool
import Utilities




-- | Initiate a handshake with a remote node, with the purpose of adding it as
--   a downstream neighbour.
--
--   Counterpart of 'Server.handshakeH'.
--
--   The procedure is as follows:
--
--   1. Open a connection to the new node and send it the 'Handshake' signal.
--   2. When the answer is 'OK', attempt to launch a new client. If not, close
--      the connection and stop.
--   3. With its 'OK', the downstream node stated that it has space and reserved
--      a slot for the new connection. Therefore, a new client can be spawned,
--      but will only done so if this node has room for it, and the downstream
--      neighbour is not yet known.
--   4. Spawn a new client with the connection just opened.
startHandshakeH :: (MonadIO io)
                => Environment
                -> To -- ^ Node to add
                -> io ServerResponse
startHandshakeH env to = liftIO $ bracketOnError initialize release action

      where initialize = connectToNode' to

            release (socket, _addr) = disconnect socket

            action (socket, _addr) =
                  request socket (Special Handshake) >>= \case
                        Just OK -> tryLaunchClient env to socket
                                   -- (Closes socket itself on error)
                        x -> do
                              disconnect socket
                              (return . Error) ("Handshake signal response: " ++ show x)



-- | Start a new client thread, if the current environment allows it (i.e.
--   there is no connection already, and the downstrean pool isn't full).
tryLaunchClient :: Environment
                -> To     -- ^ Target address (for bookkeeping)
                -> Socket -- ^ Connection
                -> IO ServerResponse
tryLaunchClient env to socket = do
      time <- makeTimestamp
      stsc <- spawn buffer
      bracket (async (client env socket to stsc))
              (rollback socket)
              (tryLaunch time stsc)

      where

      tryLaunch time stsc thread = atomically $
            -- Checking the node relationship ensures no double downstream
            -- connections are made.
            nodeRelationship env to >>= \case
                  IsSelf -> (return . Error) "Tried to launch client to self"
                  IsDownstreamNeighbour -> (return . Error)
                        "Tried to launch client to already known node"
                  IsUnrelated -> do
                        isRoom <- isRoomIn env _downstream
                        if not isRoom
                              then (return . Error) "No room for new client"
                              else do insertClient (Client time thread stsc)
                                      toIO env Chatty $ putStrLn "New client!"
                                      return OK

      buffer = P.Bounded (_maxChanSize (_config env))

      -- Cancel the client thread if it is not found in the database
      -- after the procedure has finished
      rollback socket thread = do
            p <- atomically $ isClientIn _downstream
            unless p (cancel thread >> disconnect socket)

      insertClient = modifyTVar (_downstream env) . Map.insert to
      isClientIn db = Map.member to <$> readTVar (db env)





-- | Start a new client
client :: Environment
       -> Socket -- ^ Connection to use (created by the handshake process)
       -> To -- ^ Node the Socket connects to. Only used for bookkeeping, in
             --   order to keep the client pool up to date.
       -> PChan NormalSignal
       -> IO ()
client env socket to stsc = (`finally` cleanup) . runEffect $
      P.fromInput input >-> signalH env socket to

      where st1c  = _st1c env
            input = mconcat [ _pInput st1c
                            , _pInput stsc
                            ]
            -- TODO: Implement stc to support flood messages

            cleanup = (`finally` disconnect socket) $ do

                  atomically $ modifyTVar (_downstream env) $ Map.delete to

                  -- Send shutdown notice as a courtesy
                  timeout (_mediumTickRate (_config env))
                          (send socket (Normal ShuttingDown))



signalH :: (MonadIO io)
        => Environment
        -> Socket
        -> To
        -> Consumer NormalSignal io ()
signalH env socket to = go
      where terminate = return ()
            go = do
                  signal <- await
                  request socket (Normal signal) >>= \case
                        Just OK        -> ok           env to >> go
                        Just (Error e) -> genericError env e  >> terminate
                        Just Ignore    -> ignore       env    >> terminate
                        Just Denied    -> denied       env    >> terminate
                        Just Illegal   -> illegal      env    >> terminate
                        Nothing        -> noResponse   env    >> terminate




-- | Response to sending a signal to a server successfully. (Updates the "last
--   successfully sent signal to" timestamp.)
ok :: (MonadIO io)
   => Environment
   -> To          -- ^ Target downstream neighbour
   -> io ()
ok env node = liftIO $ do
      timestamp <- makeTimestamp
      let updateTimestamp client = client { _clientTimestamp = timestamp }
      atomically $ modifyTVar (_downstream env) $
            Map.adjust updateTimestamp node



errorPrint :: (MonadIO io)
           => Environment
           -> String
           -> io ()
errorPrint env = liftIO . atomically . toIO env Debug . putStrLn



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
ignore env = errorPrint env "Server ignores this node, terminating client"



-- | Server sent back a generic error, see docs for 'Error'
genericError :: (MonadIO io)
             => Environment
             -> String
             -> io ()
genericError env e = errorPrint env $ "Generic error encountered, terminating\
                                      \ client (" ++ e ++ ")"



-- | Server denied a valid request, see docs for 'Denied'
denied :: (MonadIO io)
       => Environment
       -> io ()
denied env = errorPrint env "Server denied the request"



-- | Server denied a valid request, see docs for 'Denied'
illegal :: (MonadIO io)
        => Environment
        -> io ()
illegal env = errorPrint env "Signal illegal"



-- | Server did not respond
noResponse :: (MonadIO io)
           => Environment
           -> io ()
noResponse env = errorPrint env "Server did not respond"