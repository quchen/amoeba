-- | The client pool keeps track of running clients, requests new connections
--   when there's a deficit, and cleans up terminated ones.

-- TODO: Print status periodically like in the pre-pipes version (i.e. current
--       neighbours in both directions)
-- TODO: Refactor the housekeeping part, it's fugly
-- FIXME: If the db sizes don't change and the transaction in 'balanceEdges'
--        is retrying, the function will lock. This could be avoided by having
--        a periodically changed nonsense TVar in the transaction, but that
--        seems a bit hacky. Are there better solutions?
-- TODO: >-> is pull-based. Would push-based composition be more appropriate?

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module ClientPool (
          clientPool
        , isRoomIn
) where

import           Control.Exception
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import qualified Data.Map as Map
import           Control.Monad
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Data.Maybe (isJust)
import           Text.Printf
import qualified Data.Set as Set

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Concurrent as P


import Types
import Utilities


-- | Sets up the client pool by forking the housekeeping thread, and then starts
--   the client pool loop.
--
--   For further documentation, see @housekeeping@ and @clientLoop@.
clientPool :: Environment -> IO ()
clientPool env = withAsync hkeep $ \_ -> fillPool env
      where hkeep = housekeeping env
      -- where hkeep = yell 31 "Housekeeping disabled" -- TODO: enable


-- | Watches the count of nodes in the database, and issues 'EdgeRequest's
--   to fill the ranks if necessary.
fillPool :: Environment -> IO ()
fillPool env = (`finally` yell 42 "fillPool terminated! Should never happen!") $
               (yellAndRethrow 42) $

      runEffect $ balanceEdges env
              >-> P.map edgeRequest
              >-> dispatch

      where
            -- Send signal to the single worker channel
            dispatch :: Consumer NormalSignal IO ()
            dispatch = P.toOutput (_pOutput $ _st1c env)

            -- Create an 'EdgeRequest' from a 'Direction'
            edgeRequest :: Direction
                        -> NormalSignal
            edgeRequest dir = EdgeRequest (_self env) $
                              EdgeData dir $
                              Left $ -- Left = "bounce at least n times"
                              (_bounces._config) env



-- | Watch the database of upstream and downstream neighbours. If there is a
--   deficit in one of them, generate the 'Direction' of the new edge to
--   construct.
balanceEdges :: Environment -> Producer Direction IO r
balanceEdges env = forever $ do

      delay (_mediumTickRate $ _config env)

      (usnDeficit, dsnDeficit) <- liftIO $ atomically $ do

            usnCount <- dbSize env _upstream
            dsnCount <- dbSize env _downstream

            -- Print status message: "Network connections: USN 3/5, DSN 2/5"
            -- to indicate there are 3 of a maximum of 5 upstream neighbours
            -- currently connected, and similarly for downstream.
            toIO env Debug $ printf
                  "Network connections: upstream %d/(%d..%d),\
                                    \ downstream %d/(%d..%d)\n"
                  usnCount minNeighbours maxNeighbours
                  dsnCount minNeighbours maxNeighbours

            return ( minNeighbours - usnCount
                   , minNeighbours - dsnCount
                   )

      each $ mergeLists (replicate dsnDeficit Outgoing)
                        (replicate usnDeficit Incoming)


      where minNeighbours = _minNeighbours (_config env)
            maxNeighbours = _maxNeighbours (_config env)

            -- mergeLists [a,b] [w,x,y,z]  ==  [a,w,b,x,y,z]
            --
            -- Should be equivalent to 'concat . transpose' in terms of
            -- functionality, but I don't understand 'transpose' entirely.
            mergeLists []     ys = ys
            mergeLists (x:xs) ys = x : mergeLists ys xs



-- | Makes sure other nodes know this node is still running and has them as its
--   neighbour, removes timed out upstream nodes and dead clients/downstream
--   nodes.
housekeeping :: Environment -> IO ()
housekeeping env = forever $ do
      t <- makeTimestamp
      removeTimedOutUsn env t
      removeTimedOutDsn env t
      removeDeadClients env
      sendKeepAlive     env t
      delay (_mediumTickRate $ _config env)




-- | Remove timed out upstream nodes (USNs). Timestamps are updated by the
--   server every time a signal is received and accepted.
removeTimedOutUsn :: Environment -> Timestamp -> IO ()
removeTimedOutUsn env (Timestamp now) = atomically $ do
      let timedOut (Timestamp t) = now - t > (_poolTimeout . _config) env
      (dead, alive) <- Map.partition timedOut <$> readTVar (_upstream env)

      -- Keep only alive USNs
      writeTVar (_upstream env) alive

      -- Log message about dead USNs
      let numDead = Map.size dead
      when (_verbosity (_config env) >= Debug && numDead > 0) $ do
            toIO env Debug $
                  printf "%d timed out upstream neighbour%s removed\n"
                         numDead
                         (pluralS numDead)



-- | Kick all clients that haven't been sent an order in some time.
removeTimedOutDsn :: Environment -> Timestamp -> IO ()
removeTimedOutDsn env (Timestamp now) = do
      kill' <- atomically $ do
            let notTimedOut (Client { _clientTimestamp = Timestamp t }) =
                      now - t < (_poolTimeout._config) env
                ds = _downstream env
            (keep, kill) <- Map.partition notTimedOut <$> readTVar ds

            when (not $ Map.null kill) $ toIO env Debug $
                       putStrLn "Downstream neighbour housekilled. This is\
                                \ likely a bug, as clients should clean\
                                \ themselves up after termination."
                                -- TODO: Verify this claim

            writeTVar ds keep
            return kill

      void $ T.traverse (cancel . _clientAsync) kill'



-- | Poll all clients and remove those that are not running anymore.
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
                 putStrLn "Client housekilled. This may be a bug\
                          \ (client should cleanup itself).\n"
                          -- TODO: Verify this claim

      -- Finally, remove all dead nodes by their just found out keys
      atomically $ modifyTVar (_downstream env) $ \knownNodes ->
            F.foldr Map.delete knownNodes deadNodes



-- | Send 'KeepAlive' signals to downstream nodes (DSNs) so they can update
--   their "last heard of" timestamp
sendKeepAlive :: Environment -> Timestamp -> IO ()
sendKeepAlive env (Timestamp now) = do

      -- Get a map of all registered downstream clients
      clients <- atomically $ readTVar (_downstream env)

      -- Find out which ones haven't been contacted in a while
      let lastHeard client = let Timestamp t = _clientTimestamp client
                             in  now - t
          threshold = (_poolTimeout._config) env / 5 -- TODO: make the factor an option
          needsRefreshing client = lastHeard client >= threshold
          needKeepAlive = Map.filter needsRefreshing clients
          sendSignal node = atomically $ do
                P.send ((_pOutput . _stsc) node) KeepAlive

      void $ T.traverse sendSignal needKeepAlive



-- | Check whether there is room to add another node to the pool. The second
--   argument is supposed to be either
isRoomIn :: Environment
         -> (Environment -> TVar (Map.Map k a))
            -- ^ Projector from 'Environment' to the database, i.e. either
            -- '_upstream' or '_downstream'.
         -> STM Bool
isRoomIn env db = (maxSize >) <$> dbSize env db
      where maxSize = (fromIntegral . _maxNeighbours . _config) env

