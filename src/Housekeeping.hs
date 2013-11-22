-- | Watch the databases and clean up dead or orphaned entries.

{-# LANGUAGE LambdaCase #-}

module Housekeeping where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Control.Applicative
import Control.Monad
import Text.Printf

import qualified Pipes.Concurrent as P

import Types
import Utilities



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



-- | Periodically check whether the worker is allowed to be; if not, kill its
--   thread.
--
--   Starts by giving the worker a grace period after it is created. If it is
--   not entered as an upstream neighbour in this time, it is killed.
--   If it is in the DB, periodically check whether this hasn't changed.
workerWatcher :: Environment -> From -> ThreadId -> IO ()
workerWatcher env from tid =
      race waitForEntry gracePeriod >>= \case
            Left  _ -> watch
            Right _ -> kill


      where waitForEntry = atomically $ do
                  known <- fmap (Map.member from) (readTVar (_upstream env))
                  when (not known) retry

            gracePeriod = delay t

            kill = do
                  atomically $ modifyTVar (_upstream env) (Map.delete from)
                  killThread tid

            watch = do
                  delay t
                  makeTimestamp >>= removeTimedOutUsn env
                  known <- atomically $ fmap (Map.member from)
                                             (readTVar (_upstream env))
                  if known then watch
                           else kill

            t = (_longTickRate . _config) env
