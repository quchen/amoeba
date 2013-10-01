{-# LANGUAGE LambdaCase #-}

module Main where

import Network
import Control.Concurrent
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
      chan <- startNodePool 10 config
      putStrLn "Starting bootstrap server"
      bootstrapServer config chan



bootstrapServer :: Config -> Chan NormalSignal -> IO ()
bootstrapServer config chan =
      let initialize = listenOn . PortNumber $ _serverPort config
          release = sClose
      in  bracket initialize release $ \s -> bootstrapServerLoop config s chan



bootstrapServerLoop :: Config -> Socket -> Chan NormalSignal -> IO ()
bootstrapServerLoop config socket chan = forever $ do
      putStrLn "Waiting for clients"
      (h, host, port) <- accept socket
      putStrLn "New client"
      response <- receive' h >>= \case
            BootstrapRequest port -> do dispatchSignal config host port chan
                                        return $ YourHostIs host
            _ -> do putStrLn "Non-BootstrapRequest signal received"
                    return $ IAddedYou -- FIXME Nonsense, but makes the typechecker happy
      send' h response
      putStrLn "Client served"



-- | Send bootstrap requests on behalf of the new node to the node pool
dispatchSignal :: Config -> HostName -> PortNumber -> Chan NormalSignal -> IO ()
dispatchSignal config host port chan = do
      let to = To $ Node host port
          order dir = forM_ [1.._minNeighbours config] $ \_ -> do
                        writeChan chan $ edgeRequest config to dir
      order Incoming
      order Outgoing



-- | Construct an edge request
edgeRequest :: Config -> To -> Direction -> NormalSignal
edgeRequest config to dir = EdgeRequest to $
                            EdgeData dir $
                            Left $ _bounces config



