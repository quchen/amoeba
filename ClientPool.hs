module ClientPool (
        clientPool
      , sendEdgeRequest
) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import           Control.Monad
import           Data.Functor

import Types
import Utilities (makeTimestamp)


-- | Sets up the client pool by forking the KeepAlive thread, and then starts
--   the client pool loop.
--
--   For further documentation, see @keepAliveLoop@ and @clientLoop@.
clientPool :: Environment -> IO ()
clientPool env = forkIO (keepAliveLoop env) *> clientPoolLoop env




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
      (numKnownNodes, numKnownBy) <- atomically $ liftA2 (,)
            (fromIntegral . Set.size <$> readTVar (_knownNodes env))
            (fromIntegral . Map.size <$> readTVar (_knownBy env   ))
            -- ^ fromIntegral :: Int -> Word

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










-- | Send KeepAlive signal to a random downstream neighbour, and removes
--   upstream neighbours that haven't sent anything recently from the pool.
--   The former will cause a little spam initially when there are very few
--   downstream neighbours, but later on the least busy nodes are more likely to
--   pick up the signal, conveniently favouring them to send KeepAlive signals.
keepAliveLoop :: Environment -> IO ()
keepAliveLoop env = forever $ do

      -- Send it
      atomically $ writeTBQueue (_st1c env) KeepAlive

      -- Cleanup: Remove all nodes from the knownBy pool that haven't sent a
      -- signal in some time
      (Timestamp now) <- makeTimestamp
      atomically $ do
            -- TODO: check whether the </- are right :-)
            let notTimedOut (Timestamp t) = now - t < (_poolTimeout._config) env
            modifyTVar (_knownBy env) (Map.filter notTimedOut)
            -- TODO terminate corresponding connection

      threadDelay (_keepAliveTickRate . _config $ env)