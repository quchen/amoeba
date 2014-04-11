-- | The server is the main part of a node. It accepts incoming requests, and
--   distributes the responses to the clients.
--
--   The suffix 'H' stands for 'Handler', which is a function that reacts
--   directly to an incoming signal's instructions.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Server (server) where

import           Control.Concurrent.Async
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Applicative
import           System.Random
import           Text.Printf
import qualified Data.Foldable as F

import           Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Concurrent as P
import           Pipes.Network.TCP (Socket)
import qualified Pipes.Network.TCP as PN

import Types
import Utilities
import Client
import Housekeeping



server :: Environment
       -> Socket -- ^ Socket to listen on
       -> IO ()
server env serverSocket = do

      -- The counter will assign each node a unique name.
      counter <- newTVarIO 0

      withAsync (workerLdc env) $ \_ldcThread -> forever $ do

            from <- atomically $ do
                  c <- readTVar counter
                  modifyTVar counter (+1)
                  return (From c)

            pid <- PN.acceptFork serverSocket $ \(clientSocket, addr) -> do
                  (atomically . toIO env Debug . STDLOG)
                        (printf "New worker %s from %s"
                                (show from)
                                (show addr))
                  terminationReason <- worker env from clientSocket
                  (atomically . toIO env Debug . STDLOG)
                        (printf "Worker %s from %s terminated (%s)"
                                (show from)
                                (show addr)
                                (show terminationReason))

            forkIO (workerWatcher env from pid)



-- | Handles 'Signal's coming in from the network and sends back the server's
--   response.
worker :: Environment
       -> From        -- ^ Unique worker ID
       -> Socket      -- ^ Incoming connection
       -> IO ServerResponse
worker env from socket =

      (`finally` release) . runEffect $
            receiver socket >-> dispatch >-> terminator >-> sender socket

      where dispatch :: Pipe Signal ServerResponse IO r
            dispatch = P.mapM $ \case
                  Normal  normal  -> normalH  env from        normal
                  Special special -> specialH env from socket special

            -- Pipes incoming signals on, but terminates afterwards if the last
            -- one was an error. In other words it's similar P.takeWhile, but
            -- passes on the first failing element before returning it.
            terminator :: Pipe ServerResponse ServerResponse IO ServerResponse
            terminator = do
                  signal <- await
                  yield signal
                  case signal of
                        OK  -> terminator
                        err -> return err

            release = atomically (deleteUsn env from)


-- | Handles "Signal"s coming in from the LDC (local direct connection).
--   Used by node pools; only accepts a handful of signals.
workerLdc :: Environment
          -> IO ()
workerLdc env@(_ldc -> Just pChan) =

      runEffect (input >-> dispatch >-> P.drain)
                                        -- Since there is no USN to send the
                                        -- ServerResponse back to, discard
                                        -- the answers to LDC signals.

      where input :: Producer NormalSignal IO ()
            input = P.fromInput (_pInput pChan)

            dispatch :: Pipe NormalSignal ServerResponse IO r
            dispatch = P.mapM $ \case
                  EdgeRequest to edge  -> edgeBounceH env to edge
                  Flood tStamp fSignal -> floodSignalH env (tStamp, fSignal)
                  _else                -> return (Error "Bad LDC signal")

workerLdc _ = return ()



-- | Handle normal signals, as sent by an upstream neighbour. ("non-normal"
--   signals include bootstrap requests and the like.)
normalH :: Environment
        -> From
        -> NormalSignal
        -> IO ServerResponse
normalH env from signal =
      atomically (isUsn env from) >>= \case True  -> continue
                                            False -> deny

      where continue = do
                  result <- case signal of
                        EdgeRequest to edge  -> edgeBounceH   env to edge
                        Flood tStamp fSignal -> floodSignalH  env (tStamp, fSignal)
                        KeepAlive            -> keepAliveH    env from
                        ShuttingDown         -> shuttingDownH env from
                        Prune                -> pruneH        env
                  when (result == OK)
                       (do t <- makeTimestamp
                           atomically (updateUsnTimestamp env from t))
                  return result

            deny = do
                  atomically . toIO env Debug . STDLOG $
                        "Illegally contacted by " ++ show from ++ "; ignoring"
                  return Ignore



-- | Handler for special signals, such as "BootstrapRequest"s and "Handshake"s.
specialH :: Environment
         -> From
         -> Socket
         -> SpecialSignal
         -> IO ServerResponse
specialH env from socket signal = case signal of
      BootstrapRequest {} -> illegalBootstrapSignalH env
      NeighbourList {}    -> illegalNeighbourListSignalH env
      Handshake           -> incomingHandshakeH      env from socket
      HandshakeRequest to -> Client.startHandshakeH  env to >> return OK
                                                               -- Sender doesn't handle
                                                               -- this response anyway,
                                                               -- see sendHandshakeRequest



-- | Print a log message and generate an "Illegal" "ServerResponse".
illegal :: Environment
        -> String
        -> IO ServerResponse
illegal env msg = do (atomically . toIO env Debug . STDLOG) msg
                     return Illegal



illegalBootstrapSignalH :: Environment
                        -> IO ServerResponse
illegalBootstrapSignalH env =
      illegal env "BootstrapRequest signal received on a normal server"



illegalNeighbourListSignalH :: Environment
                            -> IO ServerResponse
illegalNeighbourListSignalH env =
      illegal env "NeighbourList signal received on a normal server"



-- | Acknowledges an incoming KeepAlive signal, which is effectively a no-op,
--   apart from that it (like any other signal) refreshes the "last heard of
--   timestamp" via the check in "normalH".
keepAliveH :: Environment
           -> From
           -> IO ServerResponse
keepAliveH env from = do
      (atomically . toIO env Chatty . STDLOG )
            (printf "KeepAlive signal received from %s"
                    (show from))
                  -- This is *very* chatty, probably too much so.
      return OK



-- | Check whether a "FloodSignal" has already been received; execute and
--   redistribute it if not.
floodSignalH :: Environment
             -> (Timestamp, FloodSignal)
             -> IO ServerResponse
floodSignalH env tfSignal@(timestamp, fSignal) = do

      knownIO <- atomically $ do
            knownSTM <- knownFlood env tfSignal
            when (not knownSTM) $ do
                  insertFlood env tfSignal

                  -- Broadcast message to all downstream neighbours
                  broadcast <- broadcastOutput env
                  void (P.send broadcast
                               (Flood timestamp fSignal))
            return knownSTM

      case (knownIO, fSignal) of
            (True, _)                      -> return OK
            (_, SendNeighbourList painter) -> neighbourListH env painter
            (_, TextMessage message)       -> textMessageH   env message



-- | Retrieve all STSC (server-to-single-client) "P.Output"s and concatenate
--   them to a single broadcast channel.
broadcastOutput :: Environment
                -> STM (P.Output NormalSignal)
broadcastOutput env =
      F.foldMap (_pOutput . _stsc) <$> readTVar (_downstream env)



-- | Print a text message
textMessageH :: Environment
             -> String
             -> IO ServerResponse
textMessageH env msg = do
      atomically (toIO env Quiet (STDOUT msg))
      return OK



-- | Send a list of neighbours to the painting server.
neighbourListH :: Environment
               -> To
               -> IO ServerResponse
neighbourListH env painter = do
      connectToNode painter $ \(socket, _) -> do
            atomically (toIO env Chatty (STDLOG "Processing painter request"))
            let self = _self env
            dsns <- atomically (dumpDsnDB env)
            send socket (NeighbourList self dsns)
      return OK



-- | An upstream neighbour is shutting down; terminate the local worker.
shuttingDownH :: Environment
              -> From
              -> IO ServerResponse
shuttingDownH env from = atomically $ do
      toIO env Debug . STDLOG $
            "Shutdown notice from " ++ show from
      return ConnectionClosed
      -- NB: Cleanup of the USN DB happens when the worker shuts down



-- | A USN wants to terminate the connection because it has too mans DSNs.
--   Check whether this can be done without dropping the USN count below the
--   minimum number of neighbours, and terminate the worker if this is the case.
pruneH :: Environment
       -> IO ServerResponse
pruneH env = atomically $ do
      usnSize <- dbSize env _upstream
      let minN = _minNeighbours (_config env)
      if usnSize > minN
            then
                  -- Send back a special "OK" signal that terminates the
                  -- connection
                  return PruneOK
            else
                  -- "OK" means "do not terminate the worker" here!
                  return OK



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

edgeBounceH :: Environment
            -> To
            -> EdgeData
            -> IO ServerResponse

-- Phase 1 ends: Hard bounce counter reaches 0, start soft bounce phase
edgeBounceH env origin (EdgeData dir (HardBounce 0)) =
      edgeBounceH env
                  origin
                  (EdgeData dir
                            (SoftBounce 0
                                        ((_acceptP . _config) env)))

-- Phase 1: Hard bounce, bounce on.
edgeBounceH env origin (EdgeData dir (HardBounce n)) = do

      let buildSignal = EdgeRequest origin . EdgeData dir
          nMax = _bounces (_config env)

      atomically $ do

            P.send (_pOutput (_st1c env))
                   (buildSignal (HardBounce (min (n - 1) nMax)))
                                          -- Cap the number of hard
                                          -- bounces with the current
                                          -- node's configuration to
                                          -- prevent "maxBound bounces
                                          -- left" attacks
            (toIO env Chatty . STDLOG)
                  (printf "Hardbounced %s request from %s (%d left)"
                          (show dir)
                          (show origin)
                          n)

            return OK

-- Phase 2: either accept or bounce on with adjusted acceptance
-- probability.
--
-- (Note that bouncing on always decreases the denial probability, even in case
-- the reason was not enough room.)
edgeBounceH env origin (EdgeData dir (SoftBounce n p)) = do

      (isRoom, relationship) <- atomically $ do

            -- Make sure not to connect to itself or to already known nodes
            rel <- nodeRelationship env origin

            -- Check whether there is room for another connection. Note that
            -- an Incoming request will construct a downstream neighbour from
            -- this node, so the database lookups are flipped.
            room <- case dir of
                  Incoming -> isRoomIn env _downstream
                  Outgoing -> isRoomIn env _upstream

            return (room, rel)

      -- Roll whether to accept the query first, then check whether there's
      -- room. In case of failure, bounce on.
      acceptEdge <- (< p) <$> randomRIO (0, 1 :: Double)

      case (relationship, dir) of

            -- Don't connect to self
            (IsSelf, _) -> atomically $ do
                  toIO env Chatty . STDLOG $
                        "Edge to self requested, bouncing"
                  bounceReset

            -- Don't connect to the same node multiple times
            (IsDownstreamNeighbour, Incoming) -> atomically $ do
                  -- In case you're wondering about the seemingly different
                  -- directions in that pattern (Incoming+Downstream): The
                  -- direction is from the viewpoint of the requestee, while the
                  -- relationship to that node is seen from the current node.
                  (toIO env Chatty . STDLOG)
                        (printf "Edge to %s already exists, bouncing"
                                (show origin))
                  bounceOn -- TODO: bounceReset here to avoid clustering?

            -- No room
            _ | not isRoom -> atomically $ do
                  let -- Direction of the edge (not) to be as seen from the
                      -- current node. Accepting an Outgoing request would
                      -- produce an incoming connection for example.
                      relativeDir Outgoing = "incoming"
                      relativeDir Incoming = "outgoing"
                  (toIO env Chatty . STDLOG)
                        (printf "No room for another %s connection to handle\
                                      \ the %s request"
                                (relativeDir dir)
                                (show dir))
                  bounceOn

            -- Request randomly denied
            _ | not acceptEdge -> atomically $ do
                  toIO env Chatty . STDLOG $
                        "Random bounce (accept: p = " ++ show p ++ ")"
                  bounceOn

            -- Try accepting an Outgoing request
            (_, Outgoing) -> do
                  atomically . toIO env Chatty . STDLOG $
                        "Outgoing edge request accepted, sending handshake\
                        \ request" ++ show dir
                  sendHandshakeRequest env origin

            -- Try accepting an Incoming request
            (_, Incoming) -> do
                  atomically . toIO env Chatty . STDLOG $
                        "Incoming edge request accepted, starting handshake"
                  void $ Client.startHandshakeH env origin
                  -- TODO: Bounce on if failed, otherwise an almost saturated
                  --       network won't allow new nodes

      return OK -- The upstream neighbour that relayed the EdgeRequest has
                -- nothing to do with whether the handshake fails etc.

      where

            buildSignal = EdgeRequest origin . EdgeData dir


            -- Build "bounce on" action to relay signal if necessary
            bounceOn | n >= _maxSoftBounces (_config env) =
                             toIO env Chatty . STDLOG $
                                   "Too many bounces, swallowing"
                     | otherwise =
                           let p' = max p (_acceptP (_config env))
                               -- ^ The relayed acceptance probability is at
                               --   least as high as the one the relaying node
                               --   uses. This prevents "small p" attacks that
                               --   bounce indefinitely.
                           in  void (P.send (_pOutput (_st1c env))
                                            (buildSignal (SoftBounce (n+1) p')))

            -- Build "bounce again from the beginning" signal. This is invoked
            -- if an EdgeRequest reaches the issuing node again.
            bounceReset = let b = _bounces (_config env)
                          in  void (P.send (_pOutput (_st1c env))
                                           (buildSignal (HardBounce b)))
            -- TODO: Maybe swallowing the request in this case makes more sense.
            --       The node is spamming the network with requests anyway after
            --       all.



-- | Prompt another node to start a handshake in order to be added as its
--   downstream neighbour.
sendHandshakeRequest :: Environment
                     -> To
                     -> IO ()
sendHandshakeRequest env to =
      connectToNode to $ \(socket, _addr) -> do
            request socket signal >>= \case
                  Just OK -> return ()
                  _else   -> return ()
                  -- Nothing to do here, the handshake is a one-way command,
                  -- waiting for response is just a courtesy

      where signal = (Special . HandshakeRequest) (_self env)



-- | Handle an incoming handshake, i.e. a remote node wants to add this node
--   as its downstream neighbour.
--
--   Counterpart of 'Client.startHandshakeH'.
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
incomingHandshakeH :: Environment
                   -> From
                   -> Socket
                   -> IO ServerResponse
incomingHandshakeH env from socket = do
      timestamp <- makeTimestamp

      inserted <- atomically (insertUsn env from timestamp)

      if inserted
            then send socket OK >> receive socket >>= \case
                  Just OK -> return OK
                  x -> (return . Error) (errMsg x)
            else (return . Error) errNoRoom

      where errMsg x = "Incoming handshake denied:\
                       \ server response <" ++ show x ++ ">"
            errNoRoom = "No room for another USN"
