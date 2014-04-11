-- | A client represents one connection to a downstream node.

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK show-extensions #-}


module Client  (
      startHandshakeH
) where




import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent (forkIO)
import           Data.Monoid
import           System.Timeout
import           Control.Monad
import           Control.Exception
import           Text.Printf
import           Control.Applicative

import           Pipes
import qualified Pipes.Concurrent as P
import           Pipes.Network.TCP (Socket)

import qualified Types.Lens as L

import           Types
import           ClientPool
import           Utilities



-- | Initiate a handshake with a remote node, with the purpose of adding it as
--   a downstream neighbour and launching a new client.
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
startHandshakeH :: MonadIO io
                => Environment
                -> To -- ^ Node to add
                -> io ()
startHandshakeH env to = liftIO . void . forkIO $
      connectToNode to $ \(socket, _addr) ->
            (request socket (Special Handshake) >>= \case
                  Just OK -> newClient env to socket -- OK will be sent back by
                                                     -- newClient after checking
                                                     -- for permission
                  x -> do send socket (Error "Bad handshake")
                          errorPrint env (printf "Handshake signal response: %s"
                                         (show x)))



-- | Check whether everything is alright on this end of the connection, and
--   start the client in that case.
newClient :: Environment
          -> To     -- ^ Target address (for bookkeeping)
          -> Socket -- ^ Connection
          -> IO ()
newClient env to socket = ifM allowed
                              (send socket OK >> goClient)
                              (send socket Ignore)

      where

      goClient = (`finally` cleanup) $ do
            time <- makeTimestamp
            stsc <- (spawn . P.Bounded . _maxChanSize . _config) env
            withAsync (clientLoop env socket to stsc) $ \thread -> do
                  -- (Client waits until its entry is in the DB
                  -- before it starts working.)
                  let client = Client time thread stsc
                  proceed <- atomically $ do
                        -- Double check whether the DSN is already known. Since
                        -- this is in an STM block, it is *guaranteed* that
                        -- nothing is overwritten.
                        isNew <- not <$> isDsn env to
                        when isNew (insertDsn env to client)
                        return isNew
                  when proceed (wait thread)

      -- Check whether the client is allowed, and log the event accordingly
      allowed = atomically $ nodeRelationship env to >>= \case
            IsSelf -> do
                  toIO env Debug (STDERR "Tried to launch client to self")
                  return False
            IsDownstreamNeighbour -> do
                  toIO env Debug (STDERR "Tried to launch client to already known node")
                  return False
            IsUnrelated -> do
                  isRoom <- isRoomIn env L.downstream
                  if not isRoom
                        then do toIO env Debug (STDERR "No room for new client")
                                return False
                        else return True

      -- Remove the DSN from the DB, and notify it of the event before
      -- terminating the connection
      cleanup = do atomically (deleteDsn env to)
                   timeout (_mediumTickRate (_config env))
                           (send socket (Normal ShuttingDown))



-- | Main client function; read the communication channels and execute orders.
clientLoop :: Environment
           -> Socket -- ^ Connection to use (created by the handshake process)
           -> To     -- ^ Node the 'Socket' connects to. Only used for
                     --   bookkeeping, in order to keep the client pool up to
                     --   date.
           -> PChan NormalSignal -- ^ Channel to this client
           -> IO ()
clientLoop env socket to stsc = do
      waitForDBEntry >>= \case
            Nothing -> return () -- Timeout before DB entry appears
            Just () -> runEffect (P.fromInput input >-> signalH env socket to)

      where input = mconcat [ _pInput (_st1c env)
                            , _pInput stsc
                            ]

            -- Retry until the client is inserted into the DB.
            -- Hack to allow forking the client and having it insert its
            -- own async in the DB (so it can clean up when it terminates).
            -- Since there is no DB entry to check, the timeout is added
            -- explicitly here as well.
            waitForDBEntry = timeout timeoutT
                                     (atomically (whenM (not <$> isDsn env to)
                                                        retry))

            timeoutT = (round . (* 10^6) . _poolTimeout . _config) env



-- | Send signals downstream, and handle the server's response.
signalH :: (MonadIO io)
        => Environment
        -> Socket
        -> To
        -> Consumer NormalSignal io ()
signalH env socket to = go where
      terminate = return ()
      go = await >>= request socket . Normal >>= \case
            Just OK              -> ok           env to >> go
            Just PruneOK         -> pruneOK      env    >> terminate
            Just (Error e)       -> genericError env e  >> terminate
            Just Ignore          -> ignore       env    >> terminate
            Just Denied          -> denied       env    >> terminate
            Just Illegal         -> illegal      env    >> terminate
            Just DecodeError     -> decodeError  env    >> terminate
            Just Timeout         -> timeoutError env    >> terminate
            Just ConnectionClosed -> cClosed     env    >> terminate
            Nothing              -> noResponse   env    >> terminate




-- | Response to sending a signal to a server successfully. (Updates the "last
--   successfully sent signal to" timestamp.)
ok :: (MonadIO io)
   => Environment
   -> To          -- ^ Target downstream neighbour
   -> io ()
ok env to = liftIO (do t <- makeTimestamp
                       atomically (updateDsnTimestamp env to t))



errorPrint :: (MonadIO io)
           => Environment
           -> String
           -> io ()
errorPrint env = liftIO . atomically . toIO env Debug . STDLOG
      -- TODO: Rename this function, since it only reports the results to
      --       STDLOG; the messages aren't errors of this client after all, but
      --       a result of the DSN's behaviour



pruneOK :: (MonadIO io)
        => Environment
        -> io ()
pruneOK env = errorPrint env "Pruning confirmed, terminating client"



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
denied env = errorPrint env "Server denied the request, terminating client"



-- | Server denied a valid request, see docs for 'Denied'
illegal :: (MonadIO io)
        => Environment
        -> io ()
illegal env = errorPrint env "Signal illegal, terminating client"



-- | Server did not respond
noResponse :: (MonadIO io)
           => Environment
           -> io ()
noResponse env = errorPrint env "Server did not respond, terminating client"



-- | Decoding the response unsuccessul
decodeError :: (MonadIO io)
           => Environment
           -> io ()
decodeError env = errorPrint env "Signal decoding error, terminating client"



-- | Timeout
timeoutError :: (MonadIO io)
           => Environment
           -> io ()
timeoutError env = errorPrint env "Timeout before response, terminating client"



-- | Connection closed by downstream
cClosed :: (MonadIO io)
           => Environment
           -> io ()
cClosed env = errorPrint env "The remote host has closed the connection,\
                             \ terminating client"