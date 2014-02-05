-- | Starts a bootstrap server, which is the entry point for new nodes into the
--   network.
--
--   The configuration is set like an ordinary node. The server port is what
--   will become the bootstrap server's port, and the rest of the configuration
--   is passed over to the node pool, which will open nodes at successive ports.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent hiding (yield)
import Control.Monad
import Text.Printf

import Pipes.Network.TCP (Socket)
import qualified Pipes.Network.TCP as PN

import NodePool
import Utilities
import Types
import qualified Config.Getter as Config
import Config.OptionModifier (HasNodeConfig(..), HasPoolConfig(..))



main :: IO ()
main = bootstrapServerMain



bootstrapServerMain :: IO ()
bootstrapServerMain = do

      config <- Config.bootstrap

      prepareOutputBuffers
      (output, _) <- outputThread (_maxChanSize (_nodeConfig config))

      ldc <- newChan
      terminate <- newEmptyMVar
      nodePool (_poolSize (_poolConfig config))
               (_nodeConfig config)
               ldc
               output
               terminate

      toIO' output (STDLOG (printf "Starting bootstrap server with %d nodes"
                                   (_poolSize (_poolConfig config))))
      (_rthread, restart) <- restarter (_restartMinimumPeriod config)
                                       terminate
      bootstrapServer config output ldc restart



-- | Restarts nodes in the own pool as more external nodes connect. This ensures
--   that the bootstrap server pool won't predominantly connect to itself, but
--   open up to the general network over time.
--
--   Waits a certain amount of time, and then kills a random pool node when a
--   new node is bootstrapped.
--
--   Does not block.
restarter :: Int                  -- ^ Minimum amount of time between
                                  --   consecutive restarts
          -> MVar ()              -- ^ Will kill a pool node when filled
          -> IO (Async (), IO ()) -- ^ Thread async, and action that when
                                  --   executed restarts a node.
restarter minPeriod trigger = do
      newClient <- newEmptyMVar
      thread <- async (go newClient)
      let restart = void (tryPutMVar newClient ())
      return (thread, restart)

      where go c = forever $ do
                  delay minPeriod -- Cooldown TODO: Read from config
                  void (takeMVar c)
                  yell 44 "Restart triggered!"
                  tryPutMVar trigger ()



bootstrapServer :: BootstrapConfig
                -> IOQueue
                -> Chan NormalSignal
                -> IO () -- ^ Restarting action, see "restarter"
                -> IO ()
bootstrapServer config ioq ldc restart =
      PN.listen (PN.Host "127.0.0.1")
                (show (_serverPort (_nodeConfig config)))
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
      -> IO ()             -- ^ Restarting action, see "restarter"
      -> IO r
bssLoop config ioq counter' serverSock ldc restartTrigger = go counter' where

      go !counter = do

            let
                -- Number of times this loop runs with simplified settings
                -- to help the node pool buildup.
                preRestart = _poolSize (_poolConfig config)
                             *
                             _maxNeighbours (_nodeConfig config)

                -- The first couple of new nodes should not bounce, as there are
                -- not enough nodes to relay the requests (hence the queues fill
                -- up and the nodes block indefinitely.
                nodeConfig | counter <= preRestart = (_nodeConfig config) { _bounces = 0 }
                           | otherwise             = _nodeConfig config

                -- If nodes are restarted when the server goes up there are
                -- multi-connections to non-existing nodes, and the network dies off
                -- after a couple of cycles for some reason. Disable restarting for
                -- the first couple of nodes.
                restartMaybe _ | counter <= preRestart = return ()
                restartMaybe c = when (c `rem` _restartEvery config == 0)
                                      restartTrigger

                bootstrapRequestH socket node = do
                      dispatchSignal nodeConfig node ldc
                      send socket OK

            PN.acceptFork serverSock $ \(clientSock, _clientAddr) ->
                  receive clientSock >>= \case
                        Just (BootstrapRequest benefactor) -> do
                              toIO' ioq
                                    (STDLOG (printf "Sending requests on behalf\
                                                          \ of %s"
                                                    (show benefactor)))
                              bootstrapRequestH clientSock benefactor
                              restartMaybe counter
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
            bounceParam   = HardBounce (_bounces config)