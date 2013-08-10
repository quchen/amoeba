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
clientPool env = forkIO (housekeeping env) *> clientPoolLoop env




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
            liftA2 (,) (mapSize _knownNodes) (mapSize _knownBy)

      let minNeighbours = _minNeighbours (_config env)

      -- Enough downstream neighbours?
      when (numKnownNodes < minNeighbours) $ do  -- Send out requests
            let deficit = minNeighbours - numKnownNodes
            forM_ [1..deficit] $ \_ -> sendEdgeRequest env (_self env) Outgoing

      -- Enough upstream neighbours?
      when (numKnownBy < minNeighbours) $ do
            -- Send out announces
            let deficit = minNeighbours - numKnownBy
            forM_ [1..deficit] $ \_ -> sendEdgeRequest env (_self env) Incoming

      threadDelay $ _poolTickRate (_config env)





-- | Sends out a request for either an incoming (announce) or outgoing (request)
--   edge to the network.
sendEdgeRequest :: Environment
                -> Node
                -> Direction
                -> IO ()
sendEdgeRequest env node dir = atomically $
      writeTBQueue (_st1c env) $
            EdgeRequest node . EdgeData dir . Left $ (_bounces._config) env





-- | Makes sure other nodes know this node is still running and has them as its
--   neighbour, removes timed out upstream nodes and dead clients/downstream
--   nodes.
housekeeping :: Environment -> IO ()
housekeeping env = forever $ do
      sendKeepAlive env
      removeTimedOut env
      removeDeadClients env
      threadDelay (_keepAliveTickRate $ _config env)



-- | Sends KeepAlive signals to downstream nodes so they can update their "last
--   heard of" timestamp
sendKeepAlive :: Environment -> IO ()
sendKeepAlive env = atomically $ writeTBQueue (_st1c env) KeepAlive



-- | Remove timed out upstream nodes
removeTimedOut :: Environment -> IO ()
removeTimedOut env = do
      (Timestamp now) <- makeTimestamp
      atomically $ do
            -- TODO: check whether the </- are right :-)
            let notTimedOut (Timestamp t) = now - t < (_poolTimeout._config) env
            modifyTVar (_knownBy env) (Map.filter notTimedOut)
            -- TODO terminate corresponding connection



-- | Polls all clients and removes those that are dead
removeDeadClients :: Environment -> IO ()
removeDeadClients env = do
      -- Send a poll request to all currently running clients.
      -- knownNodes :: Map Node (Async ())
      knownNodes <- atomically $ readTVar (_knownNodes env)
      -- polledClients :: Map Node (Maybe Either <...>)
      polledClients <- sequenceA $ fmap poll knownNodes

      let -- deadNodes :: [Node]
          deadNodes = Map.keys $ Map.filter isJust polledClients

      -- Finally, remove all dead nodes by their just found out keys
      atomically $ modifyTVar (_knownNodes env) $ \known ->
            foldr Map.delete known deadNodes



