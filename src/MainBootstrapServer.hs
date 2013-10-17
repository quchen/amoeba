{-# LANGUAGE LambdaCase #-}

module Main where

import Network
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad

import NodePool
import CmdArgParser
import Node
import Types
import Utilities

main :: IO ()
main = bootstrapServerMain



bootstrapServerMain :: IO ()
bootstrapServerMain = do
      config <- parseArgs
      ldc <- newChan
      terminate <- newEmptyMVar
      let poolSize = _minNeighbours config * 2
      startNodePool poolSize config ldc terminate
      putStrLn $ "Starting bootstrap server with " ++ show poolSize ++ " nodes"
      async $ restartLoop terminate
      bootstrapServer config ldc



-- | Restarts nodes in the pool periodically (round robin). The purpose of this
--   is getting rid of too high interconnectedness in the node pool when there
--   is a larger network present.
restartLoop :: MVar () -> IO ()
restartLoop trigger = forever $ threadDelay (10*10^6) >> yell 34 "restart sent" >> tryPutMVar trigger ()



bootstrapServer :: Config
                -> Chan NormalSignal
                -> IO ()
bootstrapServer config ldc =
      let initialize = listenOn . PortNumber $ _serverPort config
          release = sClose
      in  bracket initialize release $ \s -> bootstrapServerLoop config 0 s ldc



bootstrapServerLoop :: Config  -- ^ Configuration to determine how many requests
                               --   to send out per new node
                    -> Integer -- ^ Number of total clients served
                    -> Socket  -- ^ Socket to listen on for bootstrap requests
                    -> Chan NormalSignal -- ^  LDC to the node pool
                    -> IO ()
bootstrapServerLoop config numServed socket ldc = do

      -- The first couple of new nodes should not bounce, as there are not
      -- enough nodes to relay the requests (hence the queues fill up and the
      -- nodes block indefinitely.
      let config' = if numServed <= fromIntegral (_minNeighbours config)
                then config { _bounces = 0 }
                else config

      putStrLn "Waiting for clients"
      (h, host, port) <- accept socket
      putStrLn "New client"
      response <- receive' h >>= \case
            BootstrapRequest port -> do dispatchSignal config' host port ldc
                                        return $ YourHostIs host
            _ -> do putStrLn "Non-BootstrapRequest signal received"
                    return $ IAddedYou -- FIXME Nonsense, but makes the typechecker happy.
                                       -- Added here to allow the above putStrLn; in
                                       -- production this case should simply be ignored.
      send' h response
      putStrLn $ "Client " ++ show (numServed + 1) ++ " served"
      bootstrapServerLoop config (numServed + 1) socket ldc



-- | Send bootstrap requests on behalf of the new node to the node pool
dispatchSignal :: Config
               -> HostName
               -> PortNumber
               -> Chan NormalSignal
               -> IO ()
dispatchSignal config host port ldc = do
      let to = To $ Node host port
          order dir = forM_ [1.._minNeighbours config] $ \_ ->
                        writeChan ldc $ edgeRequest config to dir
      order Incoming
      order Outgoing



-- | Construct an edge request
edgeRequest :: Config
            -> To
            -> Direction
            -> NormalSignal
edgeRequest config to dir = EdgeRequest to $
                            EdgeData dir $
                            Left $ _bounces config



