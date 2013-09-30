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
      chan <- startNodePool 4 config
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
                                        return OK
            _ -> putStrLn "Non-BootstrapRequest signal received" >> return Error
      send' h response
      putStrLn "Client served"



dispatchSignal :: Config -> HostName -> PortNumber -> Chan NormalSignal -> IO ()
dispatchSignal config host port chan = do
      let to = To $ Node host port
      forM_ [1..3] $ \_ -> writeChan chan $ edgeRequest config to Incoming
      forM_ [1..3] $ \_ -> writeChan chan $ edgeRequest config to Outgoing



edgeRequest :: Config -> To -> Direction -> NormalSignal
edgeRequest config to dir = EdgeRequest to $
                            EdgeData dir $
                            Left $ _minNeighbours config



