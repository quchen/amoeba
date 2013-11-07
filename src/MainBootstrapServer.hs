-- | Starts a bootstrap server, which is the entry point for new nodes into the
--   network.
--
--   The configuration is set like an ordinary node. The server port is what
--   will become the bootstrap server's port, and the rest of the configuration
--   is passed over to the node pool, which will open nodes at successive ports.

{-# LANGUAGE LambdaCase #-}

module Main where

import Data.IORef
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent hiding (yield)
import Control.Monad

import Pipes.Network.TCP (Socket)
import qualified Pipes.Network.TCP as PN

import NodePool
import CmdArgParser
import Utilities
import Types

main :: IO ()
main = bootstrapServerMain



bootstrapServerMain :: IO ()
bootstrapServerMain = do
      config <- parseArgs
      ldc <- newChan
      terminate <- newEmptyMVar
      let poolSize = _minNeighbours config * 2
      output <- newTBQueueIO (_maxChanSize config)
      nodePool poolSize config ldc output terminate
      putStrLn $ "Starting bootstrap server with " ++ show poolSize ++ " nodes"
      async $ restartLoop terminate
      bootstrapServer config ldc



-- | Restarts nodes in the pool periodically (round robin). The purpose of this
--   is getting rid of too high interconnectedness in the node pool when there
--   is a larger network present.
restartLoop :: MVar () -> IO ()
restartLoop trigger = forever $ delay (10*10^6) >> yell 34 "restart sent" >> tryPutMVar trigger ()



bootstrapServer :: Config
                -> Chan NormalSignal
                -> IO ()
bootstrapServer config ldc =
      PN.listen (PN.Host "127.0.0.1")
                (show $ _serverPort config)
                $ \(sock, addr) -> do
                        putStrLn $ "Bootstrap server listening on " ++ show addr
                        counter <- newIORef 1
                        bootstrapServerLoop config counter sock ldc



bootstrapServerLoop :: Config  -- ^ Configuration to determine how many requests
                               --   to send out per new node
                    -> IORef Integer -- ^ Number of total clients served
                    -> Socket  -- ^ Socket to listen on for bootstrap requests
                    -> Chan NormalSignal -- ^  LDC to the node pool
                    -> IO r
bootstrapServerLoop config counter serverSock ldc = forever $ do

      count <- readIORef counter

      -- The first couple of new nodes should not bounce, as there are not
      -- enough nodes to relay the requests (hence the queues fill up and the
      -- nodes block indefinitely.
      let config' = if count <= fromIntegral (_minNeighbours config)
                then config { _bounces = 0 }
                else config
          bootstrapRequestH socket node = do
                dispatchSignal config' node ldc
                send socket OK

      success <- PN.accept serverSock $ \(clientSock, _clientAddr) ->
            receive clientSock >>= \case
                  Just (BootstrapRequest benefactor) -> do
                        bootstrapRequestH clientSock benefactor
                        putStrLn $ "Client " ++ show count ++ " served"
                        return True
                  Just _other_signal -> do
                        putStrLn "Non-BootstrapRequest signal received"
                        return False
                  _no_signal -> do
                        putStrLn "Non-BootstrapRequest signal received"
                        return False

      delay (10^5)

      when success $ modifyIORef' counter (+1)



-- | Send bootstrap requests on behalf of the new node to the node pool
dispatchSignal :: Config
               -> To -- ^ Benefactor, i.e. 'BootstrapRequest' issuer's server
                     --   address
               -> Chan NormalSignal
               -> IO ()
dispatchSignal config to ldc = order Incoming >> order Outgoing
      where order dir = forM_ [1.._minNeighbours config] $ \_ ->
                              writeChan ldc $ edgeRequest config to dir




-- | Construct an edge request
edgeRequest :: Config
            -> To
            -> Direction
            -> NormalSignal
edgeRequest config to dir = EdgeRequest to $
                            EdgeData dir $
                            Left $ _bounces config



