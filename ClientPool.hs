module ClientPool (
        clientPool
      , sendEdgeRequest
) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import qualified Data.Map               as Map
import           Control.Monad
import           Data.Traversable
import           Data.Maybe (isJust)

import Types
import Utilities (makeTimestamp)


-- | Sets up the client pool by forking the housekeeping thread, and then starts
--   the client pool loop.
--
--   For further documentation, see @housekeeping@ and @clientLoop@.
clientPool :: Environment -> IO ()
clientPool env = withAsync (clientPoolLoop env) $ \cPool  -> do
                 withAsync (housekeeping env)   $ \_hkeep -> do
                 wait cPool




-- | The client pool makes sure the client count isn't too low. It does this by
--   periodically checking the current values, and issuing announces/requests if
--   necessary. The actual client threads will be spawned by the server when it
--   receives an according acceptance signal.
--
--   The goal is to know and be known by the minimum amount of nodes specified
--   by the configuration.
clientPoolLoop :: Environment -> IO ()
clientPoolLoop env = forever $ do

      -- How many nodes does the current node know, how many is it known by?
      let mapSize db = fromIntegral . Map.size <$> readTVar (db env)
                        -- ^ fromIntegral :: Int -> Word
      (numKnownNodes, numKnownBy) <- atomically $
            liftA2 (,) (mapSize _downstream) (mapSize _upstream)

      let minNeighbours = _minNeighbours (_config env)

      -- Enough downstream neighbours?
      when (numKnownNodes < minNeighbours) $ do  -- Send out requests
            let deficit = minNeighbours - numKnownNodes
            forM_ [1..deficit] $ \_ -> sendEdgeRequest env Outgoing

      -- Enough upstream neighbours?
      when (numKnownBy < minNeighbours) $ do
            -- Send out announces
            let deficit = minNeighbours - numKnownBy
            forM_ [1..deficit] $ \_ -> sendEdgeRequest env Incoming

      threadDelay $ _poolTickRate (_config env)





-- | Sends out a request for either an incoming  or outgoing edge to the
--   network.
sendEdgeRequest :: Environment
                -> Direction
                -> IO ()
sendEdgeRequest env dir = atomically $
                          writeTBQueue (_st1c env) $
                          EdgeRequest (_self env) $
                          EdgeData dir $
                          Left $
                          (_bounces._config) env





-- | Makes sure other nodes know this node is still running and has them as its
--   neighbour, removes timed out upstream nodes and dead clients/downstream
--   nodes.
housekeeping :: Environment -> IO ()
housekeeping env = forever $ do
      -- Order matters: remove dead neighbours first, then send KeepAlive
      -- signals
      -- TODO: Update timestamps of running clients
      removeTimedOut env
      removeDeadClients env
      sendKeepAlive env
      threadDelay (_keepAliveTickRate $ _config env)



-- | Sends KeepAlive signals to downstream nodes so they can update their "last
--   heard of" timestamp
sendKeepAlive :: Environment -> IO ()
sendKeepAlive env = do

      (Timestamp now) <- makeTimestamp

      -- Get a map of all registered clients
      clients <- atomically $ readTVar (_downstream env)

      -- Find out which ones haven't been contacted in a while
      let lastHeard client = let (Timestamp t) = _clientTimestamp client
                             in  now - t
          threshold = (_poolTimeout._config) env / 5 -- TODO: make the factor an option
          needsRefreshing client = lastHeard client >= threshold
          needKeepAlive = Map.filter needsRefreshing clients
          -- Sends a KeepAlive signal to the client's dedicated channel
          sendSignal chan = atomically $ writeTBQueue chan KeepAlive

      void $ traverse (sendSignal._clientQueue) needKeepAlive




-- | Remove timed out upstream nodes
removeTimedOut :: Environment -> IO ()
removeTimedOut env = do
      (Timestamp now) <- makeTimestamp
      atomically $ do
            let notTimedOut (Timestamp t) = now - t < (_poolTimeout._config) env
            modifyTVar (_upstream env) (Map.filter notTimedOut)
            -- TODO: terminate corresponding connection (right now I *think*
            --       timeouts will eventually do this, but explicit termination
            --       would be a cleaner solution.



-- | Poll all clients and removes those that are not running anymore
removeDeadClients :: Environment -> IO ()
removeDeadClients env = do
      -- Send a poll request to all currently running clients.
      -- knownNodes :: Map Node (Async ())
      knownNodes <- atomically $ readTVar (_downstream env)
      -- polledClients :: Map Node (Maybe Either <...>)
      polledClients <- sequenceA $ fmap (poll._clientAsync) knownNodes

      let -- deadNodes :: [Node]
          deadNodes = Map.keys $ Map.filter isJust polledClients
          -- TODO: Emit reason the client died if it was because of an exception

      -- Finally, remove all dead nodes by their just found out keys
      atomically $ modifyTVar (_downstream env) $ \known ->
            foldr Map.delete known deadNodes



