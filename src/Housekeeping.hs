-- | Watch the databases and clean up dead or orphaned entries.

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Housekeeping (dsnHousekeeping) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM hiding (check)
import qualified Data.Foldable as F
import           Control.Monad
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Traversable as T

import qualified Pipes.Concurrent as P

import           Control.Lens.Operators
import qualified Control.Lens as L
import qualified Types.Lens as L

import           Types
import           Utilities
import           Utilities.IOQueue





-- | Housekeeping of the downstream node database. Makes sure other nodes know
--   this node is still running and has them as its neighbour, removes dead
--   DSNs.
dsnHousekeeping :: Environment -> IO ()
dsnHousekeeping env = forever $ do
      t <- makeTimestamp
      removeTimedOutDsn   env t
      removeTerminatedDsn env
      prune               env
      sendKeepAlive       env t
      delay (env ^. L.config . L.mediumTickRate)



-- | Remove timed out nodes from the DSN DB.
removeTimedOutDsn :: Environment
                   -> Timestamp
                   -> IO ()
removeTimedOutDsn env (Timestamp now) = do

      let dsnDB = env ^. L.downstream
      dsns <- atomically (readTVar dsnDB)

      let (kill, keep) = Map.partition isTimedOut dsns
      F.for_ kill (^! L.clientAsync . L.act cancel)
      atomically (writeTVar dsnDB keep)

      unless (Map.null kill) $ atomically $ toIO env Debug $
            STDLOG "Downstream neighbour housekilled. This is likely a bug, as\
                   \ clients should clean themselves up after termination."
                    -- TODO: Verify this claim

      where

      isTimedOut client =
            let Timestamp clientTimestamp = client ^. L.clientTimestamp
                threshold = env ^. L.config . L.poolTimeout
            in  now - clientTimestamp > threshold



-- | Remove nodes whose threads have terminated from the DSB DB.
removeTerminatedDsn :: Environment -> IO ()
removeTerminatedDsn env = do

      let dsnDB = env ^. L.downstream
      dsns <- atomically (readTVar dsnDB)

      polledClients <- T.traverse (poll . L.view L.clientAsync) dsns
      let deadNodes = Map.filter isDead polledClients
          isDead = isJust

      atomically $ modifyTVar dsnDB (`Map.difference` deadNodes)

      unless (Map.null deadNodes) $
            atomically . toIO env Debug $
                 STDLOG "Client housekilled. This may be a bug\
                        \ (client should cleanup itself)."
                        -- TODO: Verify this claim



-- | If the DSN pool is larger than the minimum amount of neighbours, ask
--   random DSNs whether the connection can be dropped.
--
--   (The amount of DSNs contacted is equivalent to the excess of connections.)
prune :: Environment -> IO ()
prune env = atomically $ do
      usnSize <- usnDBSize env
      let minN = env ^. L.config . L.minNeighbours
          excess = usnSize - minN
          prunes = iSqrt
      F.for_ [1..prunes excess]
             (\_i -> P.send (env ^. L.st1c . L.pOutput)
                            Prune)



-- | Send 'KeepAlive' signals to downstream nodes (DSNs) so they can update
--   their "last heard of" timestamp
sendKeepAlive :: Environment -> Timestamp -> IO ()
sendKeepAlive env (Timestamp now) = do

      -- Get a map of all registered downstream clients
      clients <- atomically (readTVar (env ^. L.downstream))

      -- Find out which ones haven't been contacted in a while
      let needKeepAlive = Map.filter needsRefreshing clients

      F.for_ needKeepAlive sendSignal

      where

      lastHeard client = let Timestamp t = client ^. L.clientTimestamp
                         in  now - t
      threshold = env ^. L.config . L.poolTimeout . L.to (`quot` 4) -- TODO: make the factor an option
      needsRefreshing client = lastHeard client >= threshold
      sendSignal node = atomically (P.send (node ^. L.stsc . L.pOutput)
                                           KeepAlive)
