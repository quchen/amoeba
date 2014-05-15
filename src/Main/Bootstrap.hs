-- | Main entry point for the bootstrap server.
--
--   Starts a bootstrap server, which is the entry point for new nodes into the
--   network.
--
--   The configuration is set like an ordinary node. The server port is what
--   will become the bootstrap server's port, and the rest of the configuration
--   is passed over to the node pool, which will open nodes at successive ports.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Main.Bootstrap (main) where

import           Control.Concurrent hiding (yield)
import           Control.Concurrent.Async
import           Control.Monad
import           Text.Printf

import           Pipes.Network.TCP (Socket)
import qualified Pipes.Network.TCP as PN

import           Control.Lens.Operators
import qualified Control.Lens as L
import qualified Types.Lens as L

import           NodePool
import           Types
import           Utilities
import qualified Config.Getter as Config



main :: IO ()
main = bootstrapServerMain



bootstrapServerMain :: IO ()
bootstrapServerMain = do

      config <- Config.bootstrap


      prepareOutputBuffers
      (output, _) <- outputThread (config ^. L.nodeConfig . L.maxChanSize)

      let poolSize = config ^. L.poolConfig . L.poolSize
      ldc <- newChan
      terminationTrigger <- newTerminationTrigger
      nodePool poolSize
               (config ^. L.nodeConfig)
               ldc
               output
               (Just terminationTrigger)

      toIO' output (STDLOG (printf "Starting bootstrap server with %d nodes"
                                   poolSize))
      (rThread, restart) <- restarter (config ^. L.restartMinimumPeriod)
                                      terminationTrigger
      bootstrapServer config output ldc restart
      cancel rThread -- Not really necessary since this is the end of 'main'
                     -- and the thread would be killed automatically when the
                     -- program finishes, but might be useful just to be safe
                     -- in all possible futures.



-- | Stores an action that restarts a random node. Provided mostly for more
--   explicit naming.
newtype Restarter = Restarter { runRestarter :: IO () }



-- | Restarts nodes in the own pool as more external nodes connect. This ensures
--   that the bootstrap server pool won't predominantly connect to itself, but
--   open up to the general network over time.
--
--   Waits a certain amount of time, and then kills a random pool node when a
--   new node is bootstrapped.
--
--   Does not block.
restarter :: Microseconds         -- ^ Minimum amount of time between
                                  --   consecutive restarts
          -> TerminationTrigger   -- ^ Will make the pool kill a pool node when
                                  --   filled. Written to by the restarter,
                                  --   read by the node pool.
          -> IO (Async (), Restarter) -- ^ Thread async, and restart trigger
                                      --   that when executed restarts a
                                      --   (semi-random) node.
restarter minPeriod (TerminationTrigger outgoingTrigger) = do

      -- When the incoming trigger is filled, the outgoing trigger will be
      -- attempted to be filled. This is only possible if the minimum amount
      -- of time since the last restart has passed. If the trigger is already
      -- filled, triggering it again does nothing.
      incomingTrigger <- newEmptyMVar
      restarterThread <- async (forever (do delay minPeriod
                                            _ <- takeMVar incomingTrigger
                                            yell 44 "Restart triggered!"
                                            tryPutMVar outgoingTrigger ()))

      let restart = Restarter (void (tryPutMVar incomingTrigger ()))

      return (restarterThread, restart)



bootstrapServer :: BootstrapConfig
                -> IOQueue
                -> Chan NormalSignal
                -> Restarter -- ^ Restarting action, see "restarter"
                -> IO ()
bootstrapServer config ioq ldc restart =
      PN.listen (PN.Host "127.0.0.1")
                (config ^. L.nodeConfig . L.serverPort . L.to show)
                (\(sock, addr) -> do
                      toIO' ioq (STDLOG (printf "Bootstrap server listening on %s"
                                                (show addr)))
                      bssLoop config ioq 1 sock ldc restart)



-- | BSS = bootstrap server
bssLoop
      :: BootstrapConfig   -- ^ Configuration to determine how many requests to
                           --   send out per new node
      -> IOQueue
      -> Int               -- ^ Number of total clients served
      -> Socket            -- ^ Socket to listen on for bootstrap requests
      -> Chan NormalSignal -- ^ LDC to the node pool
      -> Restarter         -- ^ Restarting action, see "restarter"
      -> IO r
bssLoop config ioq counter' serverSock ldc restart = go counter' where


      -- Number of times this loop runs with simplified settings
      -- to help the node pool buildup.
      preRestart = (*) (config ^. L.poolConfig . L.poolSize)
                       (config ^. L.nodeConfig . L.maxNeighbours)

      go !counter = do

            let

                -- The first couple of new nodes should not bounce, as there are
                -- not enough nodes to relay the requests (hence the queues fill
                -- up and the nodes block indefinitely.
                nodeConfig | counter <= preRestart = L.set L.hardBounces 0 cfg
                           | otherwise = cfg
                           where cfg = config ^. L.nodeConfig

                -- If nodes are restarted when the server goes up there are
                -- multi-connections to non-existing nodes, and the network dies off
                -- after a couple of cycles for some reason. Disable restarting for
                -- the first couple of nodes.
                attemptRestart
                      | counter <= preRestart = return ()
                      | counter `isMultipleOf` (config ^. L.restartEvery) =
                                                            runRestarter restart
                      | otherwise = return ()

                a `isMultipleOf` b | b > 0 = a `rem` b == 0
                                   | otherwise = True
                                             -- This default ensures that even
                                             -- nonsensical config options don't
                                             -- make the program misbehave.
                                             -- Instead, they just restart on
                                             -- every new node (if the minimum
                                             -- restarting time has passed).

                bootstrapRequestH socket node = do
                      dispatchSignal nodeConfig node ldc
                      send socket OK

            _tid <- PN.acceptFork serverSock $ \(clientSock, _clientAddr) ->
                  receive clientSock >>= \case
                        Just (BootstrapRequest benefactor) -> do
                              toIO' ioq
                                    (STDLOG (printf "Sending requests on behalf\
                                                          \ of %s"
                                                    (show benefactor)))
                              bootstrapRequestH clientSock benefactor
                              attemptRestart
                              toIO' ioq (STDLOG (printf "Request %d served"
                                                        counter))
                        Just _other_signal -> do
                              toIO' ioq (STDLOG "Non-BootstrapRequest signal\
                                                \ received")
                        _no_signal -> do
                              toIO' ioq (STDLOG "No signal received")

            go (counter+1)



-- | Send bootstrap requests on behalf of the new node to the node pool
dispatchSignal :: NodeConfig
               -> To -- ^ Benefactor, i.e. 'BootstrapRequest' issuer's server
                     --   address
               -> Chan NormalSignal
               -> IO ()
dispatchSignal config to ldc = order Incoming >> order Outgoing
      where order dir = writeChan ldc (edgeRequest config to dir)



-- | Construct a new edge request
edgeRequest :: NodeConfig
            -> To
            -> Direction
            -> NormalSignal
edgeRequest config to dir = EdgeRequest to edgeData
      where edgeData      = EdgeData dir bounceParam
            bounceParam   = HardBounce (config ^. L.hardBounces)