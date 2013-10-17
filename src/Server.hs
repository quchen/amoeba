-- | The server is the main part of a node. It accepts incoming requests, and
--   distributes the responses to the clients.
--
--   The suffix 'H' stands for 'Handler', which is a function that reacts
--   directly to an incoming signal's instructions. (Contrary to that, helper
--   functions invoked inside handlers don't have a special name.)

-- TODO: Refactor the edge accepting functions, a lot of that code is duplicated

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Server (
      startServer
) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception (finally, bracket)
import           Control.Monad
import           Data.Functor
import           Network
import           System.IO
import           System.Random
import           Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set


import Types
import Utilities
import Client
import ClientPool (isRoomIn)






startServer :: Socket -> Environment -> IO ()
startServer s env = do void . async $ workerLdc env
                       serverLoop s env


-- | Forks off a worker for each incoming connection.
serverLoop :: Socket
           -> Environment
           -> IO ()
serverLoop socket env = forever $ do

      -- NB: h is closed by the worker, bracketing here would close it
      --     immediately after forking
      (h, host, port) <- accept socket
      let fromNode = From $ Node host port
      atomically . toIO env Debug . putStrLn $
            "New connection from " ++ show fromNode
      void . async $ worker env h fromNode
      -- TODO: orchestrate





-- | Handles an incoming connection: Pass incoming work orders on to clients,
--   print chat messages etc.
--   (The first parameter is the same as in the result of Network.accept.)
worker :: Environment
       -> Handle      -- ^ Incoming connection handle
       -> From        -- ^ Incoming connection address
       -> IO ()
worker env h from = (`finally` hClose h) . whileM isContinue $ do

      -- TODO: Ignore signals sent by nodes not registered as upstream.
      --       Open issues:
      --         - Do this here or in the server loop?
      --         - How do the ignored nodes find out they're being ignored?
      --             --> Solution: timeouts

      -- TODO: Housekeeping to delete already handled messages

      -- TODO: Error handling: What to do if rubbish data comes in?
      --       -> Respond error, kill worker
      (proceed, response) <- receive h >>= \case
            Normal  normal  -> normalH  env from normal
            Special special -> specialH env from special

      send' h response
      return proceed



-- | Listens on the direct connection or terminates immediately if there is
--   none.
workerLdc :: Environment -> IO ()
workerLdc env@(_ldc -> Just tbq) = forever $
      atomically (readTBQueue tbq) >>= ldcH env
workerLdc _ = return ()



-- | Local direct connection handler. Only accepts a couple of commands.
ldcH :: Environment -> NormalSignal -> IO ()
ldcH env (EdgeRequest to edge) = void $ edgeBounceH env to edge
ldcH env (Flood tStamp fSignal) = void $ floodSignalH  env (tStamp, fSignal)
ldcH env _ = atomically . toIO env Debug . putStrLn $
                    "Bad local signal received"
                    -- TODO DEBUG This should be red bold and underlined.



-- | Handler for normally issued normal signals, as sent by an upstream
--   neighbour. This handler will check whether the sender is a valid upstream
--   neighbour, update timestamps etc. (An example for an abnormally issued
--   normal signal is one encapsulated in a bootstrap request, which will be
--   processed differently.)
normalH :: Environment
        -> From         -- ^ Signal origin
        -> NormalSignal -- ^ Signal type
        -> IO (Proceed, ServerResponse)

normalH env from signal = isRequestAllowed env from >>= \p -> if p
      then (, OK) <$> normalH' env from signal
      else do atomically . toIO env Debug . putStrLn $
                    "Illegally contacted by " ++ show from ++ "; ignoring"
              return (Terminate, Ignore)



-- | Check whether the request is allowed and therefore be processed, by
--   checking whether the contacting node is registered as upstream. Also
--   updates the "last heard of" timestamp.
--
-- contacting node is valid upstream
-- allowed <- atomically $ Map.member from <$> readTVar (_upstream env)
-- TODO: Distinguish between certain signals here. For example, an
--       IAddedYou should of course not require an already existing
--       connection.
isRequestAllowed :: Environment
                 -> From
                 -> IO Bool
isRequestAllowed env from = do
      timestamp <- makeTimestamp
      atomically $ do
            allowed <- Map.member from <$> readTVar (_upstream env)
            when allowed $ updateKnownBy env from timestamp
            return allowed



-- | Handler for any normal signal. Does not check whether the sender is legal,
--   and should therefore only be invoked from within other handlers making sure
--   of that, see normalH and specialH.
normalH' :: Environment
         -> From         -- ^ Signal origin
         -> NormalSignal -- ^ Signal type
         -> IO Proceed

normalH' env _    (Flood tStamp fSignal) = floodSignalH  env (tStamp, fSignal)
normalH' env _    (EdgeRequest to edge)  = edgeBounceH   env to edge
normalH' env from  ShuttingDown          = shuttingDownH env from
normalH' env from  KeepAlive             = keepAliveH    env from




-- | Handler for special signals, i.e. those that may circumvent the usual
--   checks like whether the signal was sent from a registered upstream
--   neighbour.
specialH :: Environment
         -> From          -- ^ Signal origin
         -> SpecialSignal -- ^ Signal type
         -> IO (Proceed, ServerResponse)

specialH env _ (BootstrapRequest {}) =
      (, Error) <$> illegalBootstrapSignalH env

specialH env _ (YourHostIs {}) =
      (, Error) <$> illegalYourHostIsH env

specialH env _ (AddMe node) = do
      result <- addMeReceivedH env (To node)
      return $ case result of
            Continue  -> (Continue, OK)
            Terminate -> (Terminate, Error)

specialH env from IAddedYou = do
      result <- iAddedYouReceivedH env from
      return $ case result of
            Continue  -> (Continue, OK)
            Terminate -> (Terminate, Error)






-- | A node should never receive a YourHostIs signal unless it issued a
--   bootstrap. In case it gets one anyway, this function is called.
illegalYourHostIsH :: Environment -> IO Proceed
illegalYourHostIsH env = do
      atomically . toIO env Debug . putStrLn $
            "BootstrapRequest signal received on a normal server, ignoring"
      return Terminate



-- | A node should never receive a Bootstrap signal unless it issued a
--   bootstrap. In case it gets one anyway, this function is called.
illegalBootstrapSignalH :: Environment -> IO Proceed
illegalBootstrapSignalH env = do
      atomically . toIO env Debug . putStrLn $
            "Illegal bootstrap signal; ignoring"
      return Terminate




-- | Acknowledges an incoming KeepAlive signal, which is effectively a no-op,
--   apart from that it (like any other signal) refreshes the "last heard of
--   timestamp".
keepAliveH :: Environment -> From -> IO Proceed
keepAliveH env origin = do
      atomically . toIO env Chatty . putStrLn $
            "KeepAlive signal received from " ++ show origin
      return Continue





-- | Checks whether a signal meant to be distributed over the entire network has
--   already been received; if yes it is ignored, otherwise the contents are
--   executed and the signal is passed on to all downstream neighbours.
--
--   (Time timestamp sent along is so that identical signal bodies can be
--   distinguished.)
floodSignalH :: Environment
             -> (Timestamp, FloodSignal)
             -> IO Proceed
floodSignalH env tFSignal@(timestamp, fSignal) = do

      -- Check whether the signal has previously been handled
      known <- atomically $
            Set.member tFSignal <$> readTVar (_handledFloods env)

      let floodOn = atomically $ do
            modifyTVar (_handledFloods env) (Set.insert tFSignal)
            writeTChan (_stc env) $ Flood timestamp fSignal

      when (not known) $ floodOn >> case fSignal of
            TextMessage message   -> textMessageH env message
            NeighbourList painter -> neighbourListH env painter

      return Continue


-- | Prints a text message and floods it on to the network
textMessageH :: Environment
             -> String
             -> IO ()
textMessageH env message = atomically $ toIO env Quiet $ putStrLn message


-- | Sends the list of downstream neighbours to a specified address
neighbourListH :: Environment
               -> To
               -> IO ()
neighbourListH env painter =
      bracket (connectToNode painter) hClose $ \h ->
            send' h =<< atomically (Map.keysSet <$> readTVar (_downstream env))
            -- TODO: Handle response?





-- | Inserts or updates the timestamp in the "last heard of" database. Does
--   nothing if the node isn't registered upstream.
updateKnownBy :: Environment
              -> From
              -> Timestamp
              -> STM ()
updateKnownBy env node timestamp =
      modifyTVar (_upstream env) $ Map.adjust (const timestamp) node





-- | Remove the issuing node from the database
shuttingDownH :: Environment
              -> From -- ^ Shutdown node's incoming address as seen from
                      --   this node (used to terminate downstream
                      --   connections to it)
              -> IO Proceed
shuttingDownH env from = do

      atomically . toIO env Debug . putStrLn $
            "Shutdown notice from %s:%s" ++ show from

      -- Remove from lists of known nodes and nodes known by
      atomically $ modifyTVar (_upstream env) (Map.delete from)

      return Terminate
















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
            -> IO Proceed

-- Phase 1 ends: Left counter reaches 0, start soft bounce phase
edgeBounceH env origin (EdgeData dir (Left 0)) =
      edgeBounceH env origin .
      EdgeData dir .
      Right $
      (0, _acceptP $ _config env)

-- Phase 1: Left value, bounce on.
edgeBounceH env origin (EdgeData dir (Left n)) = do

      let buildSignal = EdgeRequest origin . EdgeData dir
          nMax = _bounces $ _config env

      atomically $ do

            assertNotFull (_st1c env)

            writeTBQueue (_st1c env) . buildSignal $ Left $ min (n - 1) nMax
                                -- Cap the number of hard    ^
                                -- bounces with the current  |
                                -- node's configuration to   |
                                -- prevent "maxBound bounces |
                                -- left" attacks             |
            toIO env Chatty $ printf "Bounced %s request from %s (%d left)\n"
                                     (show dir)
                                     (show origin)
                                     n

      return Continue



-- Phase 2: either accept or bounce on with adjusted acceptance
-- probability.
--
-- (Note that bouncing on always decreases the denial probability, even in case
-- the reason was not enough room.)
edgeBounceH env origin (EdgeData dir (Right (n, p))) = do

      -- Build "bounce on" action to relay signal if necessary
      let buildSignal = EdgeRequest origin . EdgeData dir
          p' = max p $ (_acceptP._config) env
          -- ^ The relayed acceptance probability is at least as high as the
          -- one the relaying node uses. This prevents "small p" attacks
          -- that bounce indefinitely.
          bounceOn = if n >= (_maxSoftBounces $ _config env)
                then let msg = "\ESC[31mToo many bounces, swallowing\ESC[0m"
                     in  atomically . toIO env Debug $ putStrLn msg
                else atomically $ do
                      assertNotFull (_st1c env)
                      writeTBQueue (_st1c env) . buildSignal $ Right (n+1, p')

      -- Build "bounce again from the beginning" signal. This is invoked if
      -- an EdgeRequest reaches the issuing node again.
      let n = _bounces $ _config env
          bounceReset = atomically $ do
                assertNotFull (_st1c env)
                writeTBQueue (_st1c env) . buildSignal $ Left n

      -- Make sure not to connect to itself or to already known nodes
      relationship <- nodeRelationship env origin


      -- Roll whether to accept the query first, then check whether there's
      -- room. In case of failure, bounce on.
      acceptEdge <- (< p) <$> randomRIO (0, 1 :: Double)
      case (relationship, acceptEdge, dir) of

            -- Don't connect to self
            (IsSelf, _, _) -> do
                  let msg = "Edge to self requested, bouncing"
                  atomically . toIO env Chatty $ putStrLn msg
                  bounceReset

            -- Don't connect to the same node multiple times
            (IsDownstreamNeighbour, _, Incoming) -> do
                  -- In case you're wondering about the seemingly different
                  -- directions in that pattern (Incoming+Downstream): The
                  -- direction is from the viewpoint of the requestee, while the
                  -- relationship to that node is seen from the current node.
                  let msg = "Edge downstream already exists, bouncing"
                  atomically . toIO env Chatty $ putStrLn msg
                  bounceOn

            -- Request randomly denied
            (_, False, _) -> do
                  let msg = "Random bounce (accept: p = " ++ show p ++ ")"
                  atomically . toIO env Chatty $ putStrLn msg
                  bounceOn

            -- Try accepting an Outgoing request
            (_, _, Outgoing) -> do -- TODO: Sub-method, running off screen here
                  isRoom <- atomically $ isRoomIn env _downstream
                  if isRoom then acceptOutgoingRequest env origin
                            else do let msg = "No room for incoming " ++
                                              "connections, bouncing"
                                    atomically . toIO env Chatty $ putStrLn msg
                                    bounceOn

            -- Try accepting an Incoming request
            (_, _, Incoming) -> do -- TODO: Sub-method, running off screen here
                  isRoom <- atomically $ isRoomIn env _upstream
                  if isRoom then acceptIncomingRequest env origin
                            else do let msg = "No room for outgoing " ++
                                              "connections, bouncing"
                                    atomically . toIO env Chatty $ putStrLn msg
                                    bounceOn

      return Continue





-- | Invoked when an Outgoing EdgeRequest is accepted by the current node. Sends
--   the originally issuing node the message "yes, you may add me as your new
--   neighbour".
acceptOutgoingRequest :: Environment -> To -> IO ()
acceptOutgoingRequest env node = do
      -- TODO: Await confirmation of the AddMe signal (-> proper handshake)
      bracket (connectToNode node) hClose $ \h -> do
            send h (Special . AddMe $ _self env)
            -- TODO: receive/handle server response
            atomically $
                  toIO env Debug . putStrLn $ "'Outgoing' request from "
                                                     ++ show node ++ " accepted"




-- | Invoked when an Incoming 'EdgeRequest' is accepted by the current node.
--   Spawns a new worker.
--
--   Identical to 'addMeReceivedH', except that the request was issued
--   internally as the response to an 'EdgeRequest'.
acceptIncomingRequest :: Environment -> To -> IO ()
acceptIncomingRequest env node = do
      atomically $ toIO env Debug . putStrLn $ "'Incoming' request from "
                                                   ++ show node ++ " accepted"
      forkNewClient env node




-- | Invoked on an incoming "Outgoing request successful" signal, i.e. after
--   sending out a Outgoing EdgeRequest, another node has given this node the
--   permission to add it as a downstream neighbour. Spawns a new worker.
--
--   Identical to 'acceptIncomingRequest', except that the request came
--   directly from another node (in particular, it is not the result of
--   handling an EdgeRequest by this node).
addMeReceivedH :: Environment -> To -> IO Proceed
addMeReceivedH env node = do
      isRoom <- atomically $ isRoomIn env _downstream
      if isRoom
            then do atomically $ toIO env Debug . putStrLn $
                          "New downstream neighbour: " ++ show node
                    forkNewClient env node
                    return Continue
            else do atomically $ toIO env Debug . putStrLn $
                          "Downstream full, rejecting request"
                    return Terminate



-- | IAddedYou is sent to a new downstream neighbour as the result of a
--   successful Incoming request. When received, this handler does the
--   bookkeeping for the new incoming connection.
iAddedYouReceivedH :: Environment -> From -> IO Proceed
iAddedYouReceivedH env node = do
      isRoom <- atomically $ isRoomIn env _upstream
      if isRoom
            then do
                  timestamp <- makeTimestamp
                  atomically $ do
                        modifyTVar (_upstream env) (Map.insert node timestamp)
                        toIO env Debug $ putStrLn $
                              "New upstream neighbour: " ++ show node
                  return Continue
            else do
                  atomically $ toIO env Debug . putStrLn $
                        "Upstream full, rejecting request"
                  return Terminate

      -- TODO: Check whether the previous 3 functions get all the directions
      --       right (i.e. do what they should)!






-- | Checks whether a connection to a certain node is allowed. A node must not
--   connect to itself or to known neighbours multiple times.
--
--   Due to the fact that an EdgeRequest does not contain the upstream address
--   of the connection to be established, it cannot be checked whether the node
--   is already an upstream neighbour directly; timeouts will have to take care
--   of that.
nodeRelationship :: Environment -> To -> IO NodeRelationship
nodeRelationship env node@(To to) = do
      let self = to == _self env
      isAlreadyDownstream <- atomically $
            Map.member node <$> readTVar (_downstream env)

      case (self, isAlreadyDownstream) of
            (True, _) -> return IsSelf
            (_, True) -> return IsDownstreamNeighbour
            _         -> return IsUnrelated
-- TODO: Solve the upstream neighbour checking issue
