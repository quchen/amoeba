-- TODO: Handle pattern mismatches on all of the handler functions. Those
--       branches should never be reached, but just to be safe there should be
--       error messages anyway.

module Server (
      randomSocket,
      serverLoop
) where

import           System.Random
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Functor
import           Text.Printf
import qualified Data.Set               as Set
import qualified Data.Map               as Map
import           Network
import           System.IO
import           Control.Exception
import           Data.Word


import Types
import Utilities
import ClientPool (sendEdgeRequest)
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
serverLoop :: Socket -> Environment -> IO ()
serverLoop socket env = forever $ do

      -- Accept incoming connections
      connection@(h, _, _) <- accept socket
      hSetBinaryMode h True
      forkIO $ worker connection env





-- | Handles an incoming connection: Pass incoming work orders on to clients,
--   print chat messages etc.
--   (The first parameter is the same as in the result of Network.accept.)
worker :: (Handle, HostName, PortNumber)
       -> Environment
       -> IO ()
worker (h, host, port) env = untilTerminate $ do

      -- TODO: Ignore signals sent by nodes not registered as upstream.
      --       Open issues:
      --         - Do this here or in the server loop?
      --         - How do the ignored nodes find out they're being ignored?
      --         - Nodes trying to connect to bootstrap over the current node
      --           should not be ignored. Make a special Bootstrap signal that
      --           is only sent in the very beginning?

      -- Update "last heard of" timestamp. Note that this will not add a valid
      -- return address (but some address the incoming connection happens to
      -- have)!
      let fromNode = Node { _host = host, _port = port }
      makeTimestamp >>= atomically . updateKnownBy env fromNode

      -- TODO: Error handling: What to do if rubbish data comes in?
      signal <- receive h

      case signal of
            TextMessage {}     -> floodMessage env signal
            ShuttingDown node  -> shuttingDown env node
            IAddedYou          -> iAddedYouReceived env fromNode
            AddMe              -> addMeReceived env fromNode
            EdgeRequest {}     -> edgeBounce env signal
            KeepAlive          -> return Continue -- Just update timestamp, already done above
            YourHostIs {}      -> yourHostIsError env
            NotYourNeighbour   -> error("Implement NotYourNeighbour handling") -- TODO
            BootstrapRequest _ -> undefined -- TODO: Ignore. Bootstrap requests
                                            --       are only taken by the
                                            --       bootstrap server.






-- | A node should never receive a YourHostIs signal unless it issued
--   Bootstrap. In case it gets one anyway, this function is called.
yourHostIsError :: Environment -> IO Proceed
yourHostIsError env = do
      atomically . toIO env . print $
            "YourHostIs signal received without bootstrap process. This is a bug, terminating server."
      return Terminate









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





-- | Inserts or updates the timestamp in the "last heard of" database.
updateKnownBy :: Environment
              -> Node
              -> Timestamp
              -> STM ()
updateKnownBy env node timestamp = modifyTVar (_knownBy env) $
                                                       Map.insert node timestamp





-- | When received, remove the issuing node from the database to ease network
--   cleanup.
shuttingDown :: Environment
             -> Node
             -> IO Proceed
shuttingDown env node = atomically $ do

      -- Status message
      let action = printf "Shutdown notice from %s:%s"
                          (show $ _host node)
                          (show $ _port node)
      writeTBQueue (_io env) action

      -- Remove from lists of known nodes and nodes known by
      modifyTVar (_knownBy env) (Map.delete node)

      return Terminate -- The other node is shutting down, there's no need to
                       -- maintain a worker for it.







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







-- | This models the "bouncing" behaviour of finding neighbours. When a node
--   receives an incoming ("Hey, I'm here, please make me your neighbour")
--   or outgoing ("I need more neighbours") request, it either accepts it or
--   bounces the request on to a downstream neighbour that repeats the process.
--   The procedure has two phases:
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

      -- Roll whether to accept the query first, then check whether there's
      -- room. In case of failure, bounce on.
      acceptEdge <- (> p) <$> randomRIO (0,1)
      case (acceptEdge, dir) of
            (False, _) -> bounceOn
            (True, Outgoing) -> do
                  isRoom <- isRoomIn (_knownNodes env)
                  if isRoom then acceptOutgoingRequest env origin
                            else bounceOn
            (True, Incoming) -> do
                  isRoom <- isRoomIn (_knownBy env)
                  if isRoom then forkNewClient env origin
                            else bounceOn

      return Continue

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