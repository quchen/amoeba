-- | The client pool keeps track of running clients, requests new connections
--   when there's a deficit, and cleans up terminated ones.

-- TODO: Print status periodically like in the pre-pipes version (i.e. current
--       neighbours in both directions)
-- TODO: Refactor the housekeeping part, it's fugly
-- FIXME: If the db sizes don't change and the transaction in 'balanceEdges'
--        is retrying, the function will lock. This could be avoided by having
--        a periodically changed nonsense TVar in the transaction, but that
--        seems a bit hacky. Are there better solutions?

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module ClientPool (
          clientPool
        , isRoomIn
) where

import           Control.Concurrent (threadDelay)
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import qualified Data.Map as Map
import           Control.Monad
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Data.Maybe (isJust)
import           Text.Printf
import           Data.List (sort)
import qualified Data.Set as Set

import Pipes
import qualified Pipes.Prelude as P


import Types
import Utilities


-- | Sets up the client pool by forking the housekeeping thread, and then starts
--   the client pool loop.
--
--   For further documentation, see @housekeeping@ and @clientLoop@.
clientPool :: Environment -> IO ()
clientPool env = withAsync (housekeeping env) $ \_ -> fillPool env


-- | Watches the count of nodes in the database, and issues 'EdgeRequest's
--   to fill the ranks if necessary.
fillPool :: Environment -> IO ()
fillPool env =

      runEffect $ balanceEdges env
              >-> P.map edgeRequest
              >-> dispatch

      where
            -- Send signal to the single worker channel
            dispatch :: Consumer' NormalSignal IO ()
            dispatch = toOut (_st1c env)

            -- Create an 'EdgeRequest' from a 'Direction'
            edgeRequest :: Direction
                        -> NormalSignal
            edgeRequest dir = EdgeRequest (To $ _self env) $
                              EdgeData dir $
                              Left $ -- Left = "bounce at least n times"
                              (_bounces._config) env



-- | Watch the database of upstream and downstream neighbours. If there is a
--   deficit in one of them, generate the 'Direction' of the new edge to
--   construct.
balanceEdges :: Environment -> Producer' Direction IO r
balanceEdges env =

      forever $ do

            (yield =<<) . lift . atomically $ do

                  usnCount <- dbSize _upstream
                  dsnCount <- dbSize _downstream

                  if | dsnCount < minNeighbours -> deficitAction Outgoing
                     | usnCount < minNeighbours -> deficitAction Incoming
                     | otherwise                -> retry

      where

            minNeighbours = _minNeighbours (_config env)

            dbSize db = fromIntegral . Map.size <$> readTVar (db env)

            deficitAction dir = do
                  toIO env Debug $
                        printf "Deficit of %s edges detected" (show dir)
                  return dir



-- | Makes sure other nodes know this node is still running and has them as its
--   neighbour, removes timed out upstream nodes and dead clients/downstream
--   nodes.
housekeeping :: Environment -> IO ()
housekeeping env = forever $ do
      -- Order matters: remove dead neighbours first, then send KeepAlive
      -- signals
      -- TODO: Update timestamps of running clients

      removeTimedOutUsn env
      removeTimedOutDsn env
      removeDeadClients env
      sendKeepAlive env
      threadDelay (_mediumTickRate $ _config env)



-- | Sends KeepAlive signals to DownStream Nodes so they can update their "last
--   heard of" timestamp
sendKeepAlive :: Environment -> IO ()
sendKeepAlive env = do

      (Timestamp now) <- makeTimestamp

      -- Get a map of all registered downstream clients
      clients <- atomically $ readTVar (_downstream env)

      -- Find out which ones haven't been contacted in a while
      let lastHeard client = let (Timestamp t) = _clientTimestamp client
                             in  now - t
          threshold = (_poolTimeout._config) env / 5 -- TODO: make the factor an option
          needsRefreshing client = lastHeard client >= threshold
          needKeepAlive = Map.filter needsRefreshing clients
          -- Sends a KeepAlive signal to the client's dedicated channel
          sendSignal chan = atomically $ do
                --assertNotFull chan
                writeTBQueue chan KeepAlive

      void $ T.traverse (sendSignal._clientQueue) needKeepAlive




-- | Remove timed out UpStream Nodes. Timestamps are updated by the server every
--   time a signal is received and accepted.
removeTimedOutUsn :: Environment -> IO ()
removeTimedOutUsn env = do
      (Timestamp now) <- makeTimestamp
      atomically $ do
            let notTimedOut (Timestamp t) = now - t < (_poolTimeout._config) env
            modifyTVar (_upstream env) (Map.filter notTimedOut)
            -- TODO: terminate corresponding connection (right now I *think*
            --       timeouts will eventually do this, but explicit termination
            --       would be a cleaner solution.



-- | Kick all clients that haven't been sent an order in some time.
removeTimedOutDsn :: Environment -> IO ()
removeTimedOutDsn env = do
      Timestamp now <- makeTimestamp
      kill' <- atomically $ do
            let notTimedOut (Client { _clientTimestamp = Timestamp t }) =
                      now - t < (_poolTimeout._config) env
                ds = _downstream env
            (keep, kill) <- Map.partition notTimedOut <$> readTVar ds
            writeTVar ds keep
            return kill

      when (not $ Map.null kill') $
            atomically . toIO env Debug $
                 putStrLn "\ESC[34mDownstream neighbour housekilled. Is this\
                          \ a bug?\ESC[0m\n"

      void $ T.traverse (cancel . _clientAsync) kill'
-- TODO: Find out whether this function is useful, or whether
--       'removeDeadClients' is enough
-- TODO: Clients are bracketed to remove themselves from the thread pool once
--       they terminate for some reason. This function may be totally
--       unnecessary.



-- | Poll all clients and removes those that are not running anymore.
removeDeadClients :: Environment -> IO ()
removeDeadClients env = do

      -- Send a poll request to all currently running clients.
      -- knownNodes :: Map Node (Async ())
      knownNodes <- atomically $ readTVar (_downstream env)
      -- polledClients :: Map Node (Maybe Either <...>)
      polledClients <- T.sequenceA $ fmap (poll . _clientAsync) knownNodes

      let deadNodes = Map.keysSet $ Map.filter isJust polledClients
          -- TODO: Emit reason the client died if it was because of an exception

      when (not $ Set.null deadNodes) $
            atomically . toIO env Debug $
                 putStrLn "\ESC[34mClient housekilled. This may be a bug\
                          \ (client should cleanup itself).\ESC[0m\n"

      -- Finally, remove all dead nodes by their just found out keys
      atomically $ modifyTVar (_downstream env) $ \knownNodes ->
            F.foldr Map.delete knownNodes deadNodes


-- | Checks whether a certain part of the pool is full. The second argument is
--   supposed to be either '_upstream' or '_downstream'.
--
--   The third argument allows specifying a comparison function, and will be
--   placed between the current size and the max size. For example 'isRoomIn'
--   is implemented using '<', as the pool size has to be strictly smaller than
--   the maximum size in order to allow another node.
checkPoolSize :: Environment
              -> (Environment -> TVar (Map.Map k a)) -- ^ Projector
              -> (Int -> Int -> Bool) -- ^ Comparison function
              -> STM Bool
checkPoolSize env proj cmp = compareWithSize <$> db
      where compareWithSize m = Map.size m `cmp` maxSize
            maxSize = fromIntegral . _maxNeighbours $ _config env
            db = readTVar (proj env)



-- | Checks whether there is room to add another node to the pool. The second
--   argument is supposed to be either '_upstream' or '_downstream'.
isRoomIn :: Environment
         -> (Environment -> TVar (Map.Map k a))
         -> STM Bool
isRoomIn env proj = checkPoolSize env proj (<)




