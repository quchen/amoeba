-- | The server is the main part of a node. It accepts incoming requests, and
--   distributes the responses to the clients.
--
--   The suffix 'H' stands for 'Handler', which is a function that reacts
--   directly to an incoming signal's instructions. (Contrary to that, helper
--   functions invoked inside handlers don't have a special name.)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Server (
      server
) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Applicative
import           Data.Functor
import           Data.IORef
import           Data.Maybe
import           Network
import           System.IO
import           System.Random
import           Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Concurrent as P
import Pipes.Network.TCP (Socket)
import qualified Pipes.Network.TCP as PN
import Control.Monad.Catch (MonadCatch)


import Types
import Utilities
import Client
import ClientPool (isRoomIn)




server :: (MonadIO io)
       => Environment
       -> Socket -- ^ Socket to listen on
       -> io ()
server env serverSocket = liftIO $ do

      -- The counter will assign each node a unique name.
      counter <- newIORef 0

      withAsync (workerLdc env) $ \_ldcThread -> forever $ do
            from <- do c <- readIORef counter
                       modifyIORef' counter (+1)
                       return (From c)

            PN.acceptFork serverSocket $ \(clientSocket, addr) -> do
                  atomically . toIO env Debug $
                        printf "New worker (%s) from %s" (show from) (show addr)
                  worker env from clientSocket



-- | Handles 'Signal's coming in from the network.
worker :: (MonadIO io)
       => Environment
       -> From        -- ^ Unique worker ID
       -> Socket      -- ^ Incoming connection
       -> io ()
worker env from socket = runEffect $ input >-> dispatch >-> respond

      where input :: (MonadIO io) => Producer Signal io ()
            input = receive socket

            dispatch :: (MonadIO io) => Pipe Signal ServerResponse io r
            dispatch = P.mapM $ \case
                  Normal  normal  -> normalH  env from        normal
                  Special special -> specialH env from socket special

            -- Send response back to the client, or terminate the worker if a
            -- bad signal (not "OK") is received.

            -- TODO: Maybe the termination rule should be relaxed to "n strikes"
            respond :: (MonadIO io) => Consumer ServerResponse io ()
            respond = do
                  response <- await
                  send' socket response
                  case response of
                        OK -> respond
                        _  -> return ()



-- | Handles 'Signal's coming in from the LDC (local direct connection).
--   Used by node pools.
workerLdc :: (MonadIO io)
          => Environment
          -> io ()
workerLdc env@(_ldc -> Just pChan) = runEffect $ input >-> dispatch >-> discard

      where input :: (MonadIO io) => Producer NormalSignal io ()
            input = P.fromInput (_pInput pChan)

            dispatch :: (MonadIO io) => Pipe NormalSignal ServerResponse io r
            dispatch = P.mapM $ \case
                  EdgeRequest to edge  -> edgeBounceH env to edge
                  Flood tStamp fSignal -> floodSignalH env (tStamp, fSignal)
                  _else                -> ldcError >> return Error

            -- Eat up all incoming signals; this is the equivalent to the
            -- 'respond' consumer in the ordinary worker, but in the LDC case
            -- the communication is one-way.
            --
            -- This is a bit of a hack of course. The dispatch pipe above is
            -- built from Producers, so their re-emitted server responses have
            -- to be destroyed.
            discard :: (MonadIO io) => Consumer ServerResponse io r
            discard = forever await

            ldcError :: (MonadIO io) => io ()
            ldcError = liftIO . atomically . toIO env Debug . putStrLn $
                                                       "Bad LDC signal received"
                               -- TODO This should be red bold and underlined. ^
workerLdc _ = return ()



-- | Handler for normally issued normal signals, as sent by an upstream
--   neighbour. This handler will check whether the sender is a valid upstream
--   neighbour, update timestamps etc. (An example for an abnormally issued
--   normal signal is one encapsulated in a bootstrap request, which will be
--   processed differently.)

-- | Handle normal signals, as sent by an upstream neighbour. ("non-normal"
--   signals include bootstrap requests and the like.)
--
--   Will check whether the sender is a valid upstream neighbour and keep
--   timestamps current, see 'isRequestAllowed'.
normalH :: (MonadIO io)
        => Environment
        -> From
        -> NormalSignal
        -> io ServerResponse
normalH env from signal = liftIO $ do
      allowed <- isRequestAllowed env from
      if allowed
            then case signal of
                  EdgeRequest to edge  -> edgeBounceH   env to edge
                  Flood tStamp fSignal -> floodSignalH  env (tStamp, fSignal)
                  KeepAlive            -> keepAliveH    env from
                  ShuttingDown         -> shuttingDownH env from
            else do atomically . toIO env Debug . putStrLn $
                          "Illegally contacted by " ++ show from ++ "; ignoring"
                    return Ignore



-- | Check whether the request is allowed and therefore be processed, by
--   checking whether the contacting node is registered as upstream. Also
--   updates the timestamp if the node is already known.
isRequestAllowed :: (MonadIO io)
                 => Environment
                 -> From
                 -> io Bool
isRequestAllowed env from = do
      timestamp <- makeTimestamp
      liftIO . atomically $ do
            allowed <- Map.member from <$> readTVar (_upstream env)
            when allowed $ modifyTVar (_upstream env) $
                  Map.adjust (const timestamp) from
            return allowed





-- | Handler for special signals, such as 'BootstrapRequest's and 'Handshake's.
specialH :: (MonadIO io)
         => Environment
         -> From
         -> Socket
         -> SpecialSignal
         -> io ServerResponse
specialH env from socket signal = case signal of
      BootstrapRequest {} -> illegalBootstrapSignalH env
      Handshake           -> handshakeH env from socket
      HandshakeRequest to -> startHandshakeH env to
      YourHostIs {}       -> illegalYourHostIsH env



-- | Print a log message and generate an 'Illegal' 'ServerResponse'.
illegal :: (MonadIO io)
        => Environment
        -> String
        -> io ServerResponse
illegal env msg = liftIO $ do
      atomically . toIO env Debug $ putStrLn msg
      return Illegal



illegalBootstrapSignalH :: (MonadIO io)
                        => Environment
                        -> io ServerResponse
illegalBootstrapSignalH env =
      illegal env "BootstrapRequest signal received on a normal server"



illegalYourHostIsH :: (MonadIO io)
                   => Environment
                   -> io ServerResponse
illegalYourHostIsH env =
      illegal env "YouHostIs signal received on a normal server"



-- | Acknowledges an incoming KeepAlive signal, which is effectively a no-op,
--   apart from that it (like any other signal) refreshes the "last heard of
--   timestamp" via the check in 'normalH'.
keepAliveH :: (MonadIO io)
           => Environment
           -> From
           -> io ServerResponse
keepAliveH env from = liftIO $ do
      atomically . toIO env Chatty $ printf
            "KeepAlive signal received from %s" (show from)
            -- This is *very* chatty, probably too much so.
      return OK



-- | Check whether a 'FloodSignal' has already been received; execute and
--   redistribute it if not.
floodSignalH :: (MonadIO io)
             => Environment
             -> (Timestamp, FloodSignal)
             -> io ServerResponse
floodSignalH env tFSignal@(timestamp, fSignal) = do

      knownIO <- liftIO . atomically $ do
            knownSTM <- Set.member tFSignal <$> readTVar (_handledFloods env)
            when (not knownSTM) $ do
                  modifyTVar (_handledFloods env) (Set.insert tFSignal)

                  -- Broadcast message to all downstream neighbours
                  broadcast <- getBroadcastOutput env
                  void $ P.send broadcast (Flood timestamp fSignal)

            return knownSTM

      case (knownIO, fSignal) of
            (True, _)                  -> return OK
            (_, NeighbourList painter) -> neighbourListH env painter
            (_, TextMessage message)   -> textMessageH   env message



-- | Print a text message
textMessageH :: (MonadIO io)
             => Environment
             -> String
             -> io ServerResponse
textMessageH env msg = do
      liftIO . atomically $ toIO env Quiet $ putStrLn msg
      return OK



-- | Senc a list of neighbours to the painting server.
neighbourListH :: (MonadIO io)
               => Environment
               -> To
               -> io ServerResponse
neighbourListH env painter = liftIO $ do
      connectToNode painter $ \(socket, _) -> do
            putStrLn $ "Connected to painter. TODO: Send something useful."
            return undefined -- TODO.




shuttingDownH :: (MonadIO io)
              => Environment
              -> From
              -> io ServerResponse
shuttingDownH env from = liftIO . atomically $ do
      modifyTVar (_upstream env) (Map.delete from)
      toIO env Debug . putStrLn $
            "Shutdown notice from %s" ++ show from
      return OK



-- | Check whether a connection to a certain node is allowed. A node must not
--   connect to itself or to known neighbours multiple times.
--
--   Due to the fact that an 'EdgeRequest' does not contain the upstream address
--   of the connection to be established, it cannot be checked whether the node
--   is already an upstream neighbour directly; timeouts will have to take care
--   of that.
nodeRelationship :: Environment
                 -> To
                 -> STM NodeRelationship
nodeRelationship env node@(To to) = do
      let isSelf = to == _self env
      isAlreadyDownstream <- Map.member node <$> readTVar (_downstream env)
      case (isSelf, isAlreadyDownstream) of
            (True, _) -> return IsSelf
            (_, True) -> return IsDownstreamNeighbour
            _         -> return IsUnrelated



-- | Bounce 'EdgeRequest's through the network in order to make new connections.
--   The idea behind this behaviour is that a new connection should be as long
--   as possible, i.e. ideally establish a link to an entirely different part of
--   the network, which prevents clustering.
--
--   When a node receives an incoming ("Hey, I'm here, please make me your
--   neighbour") or outgoing ("I need more neighbours") request, it either
--   accepts it or bounces the request on to a downstream neighbour that repeats
--   the process. The procedure has two phases:
--
--     1. In phase 1, a counter will keep track of how many bounces have
--        occurred. For example, a signal may contain the information "bounce
--        me 5 times". This makes sure the signal traverses the network a
--        certain amount, so the signal doesn't just stay in the issuing node's
--        neighbourhood.
--        Because of its definite "bounce on" nature, these are also referred to
--        as "hard bounces".
--
--     2. Phase 2 is like phase 1, but instead of a counter, there's a denial
--        probability. If there's room and the node rolls to keep the signal,
--        it will do what its contents say. If there is no room or the node
--        rolls to deny the request, it is bounced on once again, but with a
--        reduced denial probability. This leads to an approximately
--        exponentially distributed bounce-on-length in phase 2. This solves the
--        issue of having a long chain of nodes, where only having phase one
--        would reach the same node every time.
--        Because of the probabilistic travelling distance of these bounces,
--        they are also referred to as "soft bounces".

edgeBounceH :: (MonadIO io)
            => Environment
            -> To
            -> EdgeData
            -> io ServerResponse

-- Phase 1 ends: Left counter reaches 0, start soft bounce phase
edgeBounceH env origin (EdgeData dir (Left 0)) =
      edgeBounceH env origin $
      EdgeData dir . Right $ (0, _acceptP $ _config env)

-- Phase 1: Left value, bounce on.
edgeBounceH env origin (EdgeData dir (Left n)) = liftIO $ do

      let buildSignal = EdgeRequest origin . EdgeData dir
          nMax = _bounces $ _config env

      atomically $ do

            -- FIXME: The TQueue is now a PQueue.
            P.send (_pOutput $ _st1c env) . buildSignal $ Left $ min (n - 1) nMax
                                -- Cap the number of hard    ^
                                -- bounces with the current  |
                                -- node's configuration to   |
                                -- prevent "maxBound bounces |
                                -- left" attacks             |
            toIO env Chatty $ printf "Bounced %s request from %s (%d left)\n"
                                     (show dir)
                                     (show origin)
                                     n

      return OK

-- Phase 2: either accept or bounce on with adjusted acceptance
-- probability.
--
-- (Note that bouncing on always decreases the denial probability, even in case
-- the reason was not enough room.)
edgeBounceH env origin (EdgeData dir (Right (n, p))) = liftIO $ do

      -- Build "bounce on" action to relay signal if necessary
      let buildSignal = EdgeRequest origin . EdgeData dir
          p' = max p $ (_acceptP._config) env
          -- ^ The relayed acceptance probability is at least as high as the
          -- one the relaying node uses. This prevents "small p" attacks
          -- that bounce indefinitely.
          bounceOn = if n >= (_maxSoftBounces $ _config env)
                then let msg = "\ESC[31mToo many bounces, swallowing\ESC[0m"
                     in  atomically . toIO env Debug $ putStrLn msg
                else atomically . void $ do
                      P.send (_pOutput $ _st1c env) . buildSignal $ Right (n+1, p')

      -- Build "bounce again from the beginning" signal. This is invoked if
      -- an EdgeRequest reaches the issuing node again.
      let n = _bounces $ _config env
          bounceReset = void . atomically $ do
                P.send (_pOutput $ _st1c env) . buildSignal $ Left n

      -- Make sure not to connect to itself or to already known nodes
      relationship <- atomically $ nodeRelationship env origin


      -- Roll whether to accept the query first, then check whether there's
      -- room. In case of failure, bounce on.
      acceptEdge <- (< p) <$> randomRIO (0, 1 :: Double)
      case (relationship, acceptEdge, dir) of

            -- Don't connect to self
            (IsSelf, _, _) -> do
                  atomically . toIO env Chatty . putStrLn $
                        "Edge to self requested, bouncing"
                  bounceReset

            -- Don't connect to the same node multiple times
            (IsDownstreamNeighbour, _, Incoming) -> do
                  -- In case you're wondering about the seemingly different
                  -- directions in that pattern (Incoming+Downstream): The
                  -- direction is from the viewpoint of the requestee, while the
                  -- relationship to that node is seen from the current node.
                  atomically . toIO env Chatty . putStrLn $
                        "Edge downstream already exists, bouncing"
                  bounceOn

            -- Request randomly denied
            (_, False, _) -> do
                  atomically . toIO env Chatty . putStrLn $
                        "Random bounce (accept: p = " ++ show p ++ ")"
                  bounceOn

            -- Try accepting an Outgoing request
            (_, _, Outgoing) -> sendHandshakeRequest env origin
                  -- TODO: Bounce on if failed, otherwise an almost saturated
                  --       network won't allow new nodes

            -- Try accepting an Incoming request
            (_, _, Incoming) -> void $ startHandshakeH env origin
                  -- TODO: Bounce on if failed, otherwise an almost saturated
                  --       network won't allow new nodes

      return OK -- The upstream neighbour that relayed the EdgeRequest has
                -- nothing to do with whether the handshake fails etc.


-- | Prompt another node to start a handshake in order to be added as its
--   downstream neighbour.
sendHandshakeRequest :: (MonadIO io, MonadCatch io)
                     => Environment
                     -> To
                     -> io ()
sendHandshakeRequest env to =
      connectToNode to $ \(socket, addr) -> do
            response <- request socket signal
            case response of
                  Just OK -> return ()
                  _else   -> return ()
                  -- Nothing to do here, the handshake is a one-way command,
                  -- waiting for response is just a courtesy

      where signal = Special . HandshakeRequest . To $ _self env



-- | Handle an incoming handshake, i.e. a remote node wants to add this node
--   as its downstream neighbour.
--
--   Counterpart of 'startHandshakeH'.
--
--   The procedure is as follows:
--
--   1. Check whether there is space for another upstream neighbour; if yes,
--      then add it to the pool temporarily.
--   2. Send back an OK signal, "I'm ready to take your connection".
--   3. If the upstream node acknowledged this OK signal, it responds with an OK
--      on its own. The function terminates, the temporary neighbourship is made
--      permanent, and the connection can stay open and both parties know that
--      the other one has done their part.
--   4. If something goes wrong, remove the temporary partner and terminate.
handshakeH :: (MonadIO io)
           => Environment
           -> From
           -> Socket
           -> io ServerResponse
handshakeH env from socket = do
      timestamp <- makeTimestamp
      isRoom' <- liftIO . atomically $ do
            isRoom <- isRoomIn env _upstream
            when isRoom $ modifyTVar (_upstream env) (Map.insert from timestamp)
            return isRoom
      if isRoom'
            then do
                  request' socket OK >>= \case
                        Just OK -> return OK
                              -- This leaves the connection open, as it will be
                              -- used further by the client on the other side.
                        _ -> liftIO . atomically $ do
                                   modifyTVar (_upstream env) (Map.delete from)
                                   return Error
            else return Error



-- | Initiate a handshake with a remote node, with the purpose of adding it as
--   a downstream neighbour.
--
--   Counterpart of 'handshakeH'.
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
startHandshakeH env to = liftIO $
      connectToNode to $ \(socket, addr) ->
            request socket (Special Handshake) >>= \case
                  Just OK -> tryLaunchClient socket
                  _else   -> return Error

      where

            buffer = P.Bounded (_maxChanSize $ _config env)

            tryLaunchClient socket = do
                  timestamp <- makeTimestamp
                  stsc      <- spawn buffer
                  thread    <- async $ client env socket to stsc
                  let client = Client timestamp thread stsc

                  -- Add node if it is unrelated and there is room
                  keepClient <- atomically $ do
                        let allowed IsUnrelated = True
                            allowed _else       = False
                        keep <- liftA2 (&&)
                              (allowed <$> nodeRelationship env to)
                              (isRoomIn env _downstream)
                        when keep $ modifyTVar (_downstream env) $
                                                            Map.insert to client
                        return keep

                  if keepClient
                        then return OK
                        else cancel thread >> return Error



















