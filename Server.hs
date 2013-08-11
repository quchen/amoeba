-- TODO: Handle pattern mismatches on all of the handler functions. Those
--       branches should never be reached, but just to be safe there should be
--       error messages anyway.

module Server (
      randomSocket,
      serverLoop
) where

import           Control.Concurrent
import           Control.Concurrent.Async (cancel)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Functor
import           Network
import           System.IO
import           System.Random
import           Text.Printf
import qualified Data.Map               as Map
import qualified Data.Set               as Set


import Types
import Utilities
import Client (forkNewClient)




-- | Tries opening a socket on a certain amount of random ports.
randomSocket :: Config
             -> IO Socket
randomSocket config = randomSocket' config (_maxRandomPorts config)
      where randomSocket' _ 0 = error("Couldn't find free port")
            randomSocket' config n = do
                  socket <- randomPort config >>= try . listenOn
                  case socket :: Either SomeException Socket of
                        Left  _ -> randomSocket' config (n-1)
                        Right r -> return r





-- | Generates a random PortID based on the 'portRange' config value
randomPort :: Config -> IO PortID
randomPort config = PortNumber . fromIntegral <$> randomRIO (_portRange config)





-- | Forks off a worker for each incoming connection.
serverLoop :: Socket
           -> Environment
           -> IO ()
serverLoop socket env = forever $ do

      -- Accept incoming connections
      connection@(h, host, port) <- accept socket
      let fromNode = Node { _host = host, _port = port }

      isUpstream <- atomically $ Map.member fromNode <$> readTVar (_knownBy env)

      -- Ignore connections from nodes that aren't registered upstream
      -- TODO: Create a flag so that certain clients omit this check in order to
      --       help new clients connect to the network easier
      when isUpstream $ do
            hSetBinaryMode h True
            void . forkIO $ worker env h fromNode
            -- TODO: close handle after the worker is done





-- | Handles an incoming connection: Pass incoming work orders on to clients,
--   print chat messages etc.
--   (The first parameter is the same as in the result of Network.accept.)
worker :: Environment
       -> Handle
       -> Node
       -> IO ()
worker env h fromNode = untilTerminate $ do

      -- TODO: Ignore signals sent by nodes not registered as upstream.
      --       Open issues:
      --         - Do this here or in the server loop?
      --         - How do the ignored nodes find out they're being ignored? --> Solution: timeouts

      -- Update "last heard of" timestamp. Note that this will not add a valid
      -- return address (but some address the incoming connection happens to
      -- have)!
      makeTimestamp >>= atomically . updateKnownBy env fromNode

      -- TODO: Housekeeping to delete already handled messages

      -- TODO: Error handling: What to do if rubbish data comes in?
      signal <- receive h

      -- TODO: Unified logging interface. A message should have a debug level
      --       of sorts verbose/debug/normal/quiet, which is defined in the
      --       environment. Messages should also state the origin and what
      --       signal came in.

      case signal of
            TextMessage {}     -> floodMessage      env signal
            ShuttingDown node  -> shuttingDown      env node
            IAddedYou          -> iAddedYouReceived env fromNode
            AddMe              -> addMeReceived     env fromNode
            EdgeRequest {}     -> edgeBounce        env signal
            KeepAlive          -> keepAlive         env fromNode
            NotYourNeighbour   -> notYourNeighbour  env fromNode
            YourHostIs {}      -> yourHostIs        env
            BootstrapRequest _ -> bootstrapRequest  env







-- | A node should never receive a YourHostIs signal unless it issued
--   Bootstrap. In case it gets one anyway, this function is called.
yourHostIs :: Environment -> IO Proceed
yourHostIs env = do
      atomically . toIO env . putStrLn $
            "YourHostIs signal received without bootstrap process, ignoring"
      return Continue



bootstrapRequest :: Environment -> IO Proceed
bootstrapRequest env = do
      atomically . toIO env . putStrLn $
            "BootstrapRequest signal received without bootstrap process, ignoring"
      return Continue



-- | Acknowledges an incoming KeepAlive signal, which is effectively a no-op,
--   apart from that it (like any other signal) refreshes the "last heard of
--   timestamp".
keepAlive :: Environment -> Node -> IO Proceed
keepAlive env origin = do
      atomically . toIO env . putStrLn $
            "KeepAlive signal received from " ++ show origin
      return Continue



-- | A downstream node has received a signal from this node without having it
--   in its list of upstream neighbours. A a result, it tells the issuing node
--   that it will ignore its requests.
--
-- The purpose of this is twofold:
--
--   - Nodes can only be contacted by other registered nodes. A malicious
--     network of other nodes cannot nuke a node with illegal requests, because
--     it will just ignore all the illegally created ones.
--
--   - If a node doesn't send a signal for too long, it will time out. When it
--     starts sending new signals, it will be told that it was dropped.
notYourNeighbour :: Environment -> Node -> IO Proceed
notYourNeighbour env complainer = do
      atomically . toIO env . putStrLn $
            "NotYourNeighbour signal received from " ++ show complainer

      -- Determine the Async of the client to kick
      kick <- atomically $ Map.lookup complainer <$> readTVar (_knownNodes env)
      -- Cancel client
      maybe (return ()) (cancel._clientAsync) kick
      -- NB: The client will remove itself from the pool when it is kicked. If
      --     that doesn't work, the client pool will periodically clean up as
      --     well, so there's no need for de-registering the client here.

      return Continue




-- | Sends a message to the printer thread
floodMessage :: Environment
             -> Signal
             -> IO Proceed
floodMessage env signal = do

      -- Only process the message if it hasn't been processed already
      process <- atomically $
            Set.member signal <$> readTVar (_handledQueries env)

      when process $ atomically $ do

            -- Add signal to the list of already handled ones
            modifyTVar (_handledQueries env) (Set.insert signal)

            -- Print message on the current node
            let (TextMessage _timestamp message) = signal -- TODO: Handle pattern mismatch
            toIO env $ putStrLn message

            -- Propagate message on to all clients
            writeTChan (_stc env) signal

      return Continue -- Keep the connection alive, e.g. to get more messages
                      -- from that node.





-- | Inserts or updates the timestamp in the "last heard of" database. Does
--   nothing if the node isn't registered upstream.
updateKnownBy :: Environment
              -> Node
              -> Timestamp
              -> STM ()
updateKnownBy env node timestamp = modifyTVar (_knownBy env) $
                                               Map.adjust (const timestamp) node





-- | Remove the issuing node from the database
shuttingDown :: Environment
             -> Node
             -> IO Proceed
shuttingDown env node = atomically $ do

      -- Status message
      let action = printf "Shutdown notice from %s:%s" (show $ _host node)
                                                       (show $ _port node)
      writeTBQueue (_io env) action

      -- Remove from lists of known nodes and nodes known by
      modifyTVar (_knownBy env) (Map.delete node)

      return Continue







-- | A node signals that it has added the current node to its pool. This happens
--   at the end of a neighbour search.
--
--   This should be the first signal the current node receives from another node
--   choosing it as its new neighbour.
iAddedYou :: Environment
          -> Node
          -> IO Proceed
iAddedYou env node = do
      timestamp <- makeTimestamp
      atomically $ do
            modifyTVar (_knownBy env) (Map.insert node timestamp)
            toIO env $ putStrLn $ "New upstream neighbour: " ++ show node
      return Continue -- Let's not close the door in front of our new friend :-)






-- | Bounces EdgeRequests through the network in order to make new connections.
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
--
--     2. Phase 2 is like phase 1, but instead of a counter, there's a denial
--        probability. If there's room and the node rolls to keep the signal,
--        it will do what its contents say. If there is no room or the node
--        rolls to deny the request, it is bounced on once again, but with a
--        reduced denial probability. This leads to an approximately
--        exponentially distributed bounce-on-length in phase 2. This solves the
--        issue of having a long chain of nodes, where only having phase one
--        would reach the same node every time.

edgeBounce :: Environment
           -> Signal
           -> IO Proceed

-- Phase 1: Left value, bounce on.
edgeBounce env (EdgeRequest origin (EdgeData dir (Left n))) = do

      let buildSignal = EdgeRequest origin . EdgeData dir
      atomically $ do
            writeTBQueue (_st1c env) . buildSignal $ case n of
                  0 -> Right $ 1 / (_lambda._config) env
                  k -> Left  $ k - 1
            toIO env $ printf "Bounced %s (%d left)" (show origin) n

      return Continue

-- Phase 2: either accept or bounce on with adjusted acceptance
-- probability.
--
-- (Note that bouncing on always decreases the denial probability, even in case
-- the reason was not enough room.)
edgeBounce env (EdgeRequest origin (EdgeData dir (Right p))) = do

      -- "Bounce on" action with denial probabillity decreased by lambda
      let buildSignal = EdgeRequest origin . EdgeData dir
          bounceOn = atomically $ writeTBQueue (_st1c env) $
                buildSignal . Right $ p / (_lambda._config) env

      -- Checks whether there's still room for another entry. The TVar can
      -- either be the set of known or "known by" nodes.
      let threshold = (_maxNeighbours._config) env
          isRoomIn tVar = atomically $
                (< threshold) . fromIntegral . Map.size <$> readTVar tVar

      -- Make sure not to connect to itself or to already known nodes
      allowed <- isAllowed env dir origin

      -- Roll whether to accept the query first, then check whether there's
      -- room. In case of failure, bounce on.
      acceptEdge <- (> p) <$> randomRIO (0,1)
      case (allowed && acceptEdge, dir) of
            (False, _) -> bounceOn
            (_, Outgoing) -> do
                  isRoom <- isRoomIn (_knownNodes env)
                  if isRoom then acceptOutgoingRequest env origin
                            else bounceOn
            (_, Incoming) -> do
                  isRoom <- isRoomIn (_knownBy env)
                  if isRoom then forkNewClient env origin
                            else bounceOn

      return Continue

      -- TODO: What should happen if a client manipulates the denial probability
      --       or the number of bounces left? There should be a maximum "bounce
      --       on" value, and p should be constrained between .1 and .9 or so.


-- Bad signal received. "Else" case of the function's pattern.
edgeBounce env signal = do
      atomically $ toIO env $ printf ("Signal %s received by edgeBounce;"
                                   ++ "this should never happen") (show signal)
      return Continue



-- | Sent as a confirmation message when a Outgoing request issued by another
--   node was accepted by this node. In other words, this function sends a "yes,
--   you may add me as your neighbour".
acceptOutgoingRequest :: Environment -> Node -> IO ()
acceptOutgoingRequest env origin = do
      timestamp <- makeTimestamp
      bracket (connectToNode origin) hClose $ \h -> do
            send h AddMe
            atomically $ updateKnownBy env origin timestamp



-- | Invoked on an incoming "Outgoing request successful" signal, i.e. after
--   sending out a Outgoing EdgeRequest, another node has given this node the
--   permission to add it as a downstream neighbour. Spawns a new worker
--   connecting to the accepting node.
addMeReceived :: Environment -> Node -> IO Proceed
addMeReceived env node = forkNewClient env node >> return Continue


-- | IAddedYou is sent to a new downstream neighbour as the result of a
--   successful Incoming request. When received, this handler does the
--   bookkeeping for the new incoming connection.
iAddedYouReceived :: Environment -> Node -> IO Proceed
iAddedYouReceived env node = do
      timestamp <- makeTimestamp
      atomically $ updateKnownBy env node timestamp
      return Continue

      -- TODO: Check whether the previous 3 functions get all the directions right (i.e. do what they should)!




-- | Checks whether a connection from/to origin is allowed. A node must not
--   connect to itself or to known neighbours multiple times.
isAllowed :: Environment -> Direction -> Node -> IO Bool
isAllowed env dir origin = do

      -- Don't connecto to yourself
      let isSelf = origin == _self env

      -- 1. The origin node requesting an incoming connection is already
      --    downstream
      -- 2. The origin node requesting an outgoing connection is already
      --    upstream
      let isInDatabase db = atomically $ Map.member origin <$> readTVar db
      isAlreadyKnown <- case dir of
            Incoming -> isInDatabase (_knownNodes env)
            Outgoing -> isInDatabase (_knownBy env)

      return $ isSelf || isAlreadyKnown

