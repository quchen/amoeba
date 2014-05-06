-- | Watch the databases and clean up dead or orphaned entries.

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Housekeeping (dsnHousekeeping, workerWatcher) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM hiding (check)
import qualified Data.Foldable as F
import           Control.Monad
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Traversable as T

import qualified Pipes.Concurrent as P

import           Control.Lens.Operators
import qualified Control.Lens as L
import qualified Types.Lens as L

import           Types
import           Utilities



-- | Makes sure other nodes know this node is still running and has them as its
--   neighbour, removes timed out upstream nodes and dead clients/downstream
--   nodes.
dsnHousekeeping :: Environment -> IO ()
dsnHousekeeping env = forever $ do
      t <- makeTimestamp
      cleanupDsn    env t
      prune         env
      sendKeepAlive env t
      delay (env ^. L.config . L.mediumTickRate)



cleanupDsn :: Environment -> Timestamp -> IO ()
cleanupDsn env (Timestamp now) = do

      -- Nodes to kill because of timeout
      (notTimedOut, killTimeout) <- atomically $ do
            let notTimedOut (Client { _clientTimestamp = Timestamp t }) =
                      (now - t) < (env ^. L.config . L.poolTimeout)
            (keep, kill) <- fmap (Map.partition notTimedOut)
                                 (env ^. L.downstream . L.to readTVar)

            when (not (Map.null kill)) $ toIO env Debug $
                       STDLOG "Downstream neighbour housekilled. This is\
                               \ likely a bug, as clients should clean\
                               \ themselves up after termination."
                               -- TODO: Verify this claim

            return (keep, kill)

      -- Cancel timed out client threads
      void (T.traverse (cancel . L.view L.clientAsync) killTimeout)


      -- Gather otherwise terminated nodes
      polledClients <- T.traverse (poll . L.view L.clientAsync) notTimedOut
      let deadNodes = Map.filter isJust polledClients
      when (not (Map.null deadNodes)) $
            atomically . toIO env Debug $
                 STDLOG "Client housekilled. This may be a bug\
                        \ (client should cleanup itself)."
                        -- TODO: Verify this claim

      -- Remove timed out or otherwise terminated nodes
      let toKill = Map.keysSet killTimeout <> Map.keysSet deadNodes
      atomically $ modifyTVar (env ^. L.downstream) $ \knownDsn ->
            F.foldr Map.delete knownDsn toKill



-- | If the DSN pool is larger than the minimum amount of neighbours, ask
--   random DSNs whether the connection can be dropped.
--
--   (The amount of DSNs contacted is equivalent to the excess of connections.)
prune :: Environment -> IO ()
prune env = atomically $ do
      usnSize <- dbSize env L.upstream
      let minN = env ^. L.config . L.minNeighbours
          excess = usnSize - minN
      forM_ [1..excess]
            (\_i -> (P.send (env ^. L.st1c . L.pOutput)
                            Prune))



-- | Send 'KeepAlive' signals to downstream nodes (DSNs) so they can update
--   their "last heard of" timestamp
sendKeepAlive :: Environment -> Timestamp -> IO ()
sendKeepAlive env (Timestamp now) = do

      -- Get a map of all registered downstream clients
      clients <- env ^. L.downstream . L.to (atomically . readTVar)

      -- Find out which ones haven't been contacted in a while
      let lastHeard client = let Timestamp t = client ^. L.clientTimestamp
                             in  now - t
          threshold = env ^. L.config . L.poolTimeout . L.to (/5) -- TODO: make the factor an option
          needsRefreshing client = lastHeard client >= threshold
          needKeepAlive = Map.filter needsRefreshing clients
          sendSignal node = atomically $ do
                P.send (node ^. L.stsc . L.pOutput)
                       KeepAlive

      void (T.traverse sendSignal needKeepAlive)



-- | Periodically check whether the worker is allowed to be; if not, kill its
--   thread.
--
--   Starts by giving the worker a grace period after it is created. If it is
--   not entered as an upstream neighbour in this time, it is killed.
--   If it is in the DB, periodically check whether this hasn't changed.
workerWatcher :: Environment -> From -> ThreadId -> IO ()
workerWatcher env from tid =
      race (delay tickrate) waitForEntry >>= \case
            Left  _ -> kill
            Right _ -> watch


      where waitForEntry = atomically $ do
                  known <- fmap (Map.member from) (readTVar usnDB)
                  when (not known) retry

            tickrate = env ^. L.config . L.longTickRate

            timeout = env ^. L.config . L.poolTimeout

            usnDB = env ^. L.upstream

            isTimedOut (Timestamp now) =
                  let check (Just (Timestamp past)) = now - past > timeout
                      check _ = True -- Node not even present in DB
                  in  atomically (check . Map.lookup from <$> readTVar usnDB)

            watch = delay tickrate >> makeTimestamp >>= isTimedOut >>= \case
                          True  -> kill
                          False -> watch

            kill = do atomically (modifyTVar usnDB (Map.delete from))
                      killThread tid