{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           System.IO
import           Text.Printf
import qualified Data.Foldable as F

import qualified Pipes.Concurrent as P
import qualified Network.Simple.TCP as N

import Utilities
import CmdArgParser (parseBSArgs)
import Types
import NodePool



main :: IO ()
main = drawingServerMain

drawingServerMain :: IO ()
drawingServerMain = do

      -- Preliminaries
      bsConfig <- parseBSArgs -- TODO: Drawing server config parser
      (output, _) <- outputThread (_maxChanSize (_nodeConfig bsConfig))

      -- Node pool
      ldc <- newChan
      terminate <- newEmptyMVar -- TODO: Never actually used. Refactor node pool?
      nodePool (_poolSize bsConfig)
               (_nodeConfig bsConfig)
               ldc
               output
               terminate

      -- Bootstrap service
      printf "Starting drawing server with %d nodes"
             (_poolSize bsConfig)
      drawingServer bsConfig output ldc


drawingServer :: BSConfig
              -> IOQueue
              -> Chan NormalSignal
              -> IO ()
drawingServer config ioq ldc = do
      -- Server to graph worker
      stg <- spawn (P.Bounded (_maxChanSize (_nodeConfig config)))
      forkIO (graphWorker stg)

      let port = _serverPort (_nodeConfig config)
      N.listen (N.Host "127.0.0.1") (show port) $ \(socket, addr) -> do
            let selfTo = To (Node "127.0.0.1" port)
            forkIO (networkAsker (_poolSize config) selfTo ldc)
            incomingLoop ioq stg socket



incomingLoop :: IOQueue
             -> PChan (To, Set To)
             -> N.Socket
             -> IO ()
incomingLoop ioq stg serverSock = forever $ do
      N.acceptFork serverSock $ \(clientSock, _clientAddr) -> do
            receive clientSock >>= \case
                  Just (NeighbourList node neighbours) -> do
                        yell 42 "NODE DATA"
                        toIO' ioq (putStrLn ("Received node data from" ++ show node))
                        atomically (void (P.send (_pOutput stg) (node, neighbours)))
                  Just _other_signal -> toIO' ioq (putStrLn "Invalid signal received")
                  _no_signal -> toIO' ioq (putStrLn "No signal received")



graphWorker :: (Show a, Ord a) => PChan (a, Set a) -> IO ()
graphWorker stg = do
      t'graph <- newTVarIO (Graph Map.empty)
      forkIO (graphDrawer t'graph)
      forever . atomically $ do
            Just (node, neighbours) <- P.recv (_pInput stg) -- TODO: Error handling on Nothing
            modifyTVar t'graph
                       (\(Graph g) -> Graph (Map.insert node neighbours g))
            -- Listen for new neighbour lists, add them to graph
            -- Plot graph


-- | Read the graph and compiles it to .dot format
graphDrawer :: (Show a, Ord a) => TVar (Graph a) -> IO ()
graphDrawer t'graph = forever $ do
      threadDelay (10^6) -- TODO: Configurable
      graph <- atomically (readTVar t'graph)
      writeFile "network_graph.dot" (graphToDot graph) -- TODO: Make filename configurable



-- | Periodically send out flood messages to get the network data
networkAsker :: Int -> To -> Chan NormalSignal -> IO ()
networkAsker poolSize toSelf ldc = forever $ do
      threadDelay (10^6) -- TODO: Configurable
      t <- makeTimestamp
      let signal = Flood t (SendNeighbourList toSelf)
      forM_ [1..poolSize] $ \_ -> writeChan ldc signal




-- | Graph consisting of a set of nodes, each having a set of neighbours.
data Graph a = Graph (Map a (Set a))



-- | Dirty string-based hacks to convert a Graph to .dot
graphToDot :: Show a => Graph a -> String
graphToDot (Graph g) =
      dotBoilerplate . intercalate "\n\n" . map vertexToDot $ Map.assocs g



vertexToDot :: Show a => (a, Set a) -> String
vertexToDot (start, ends) = F.foldMap (edgeToDot start) ends



edgeToDot :: Show a => a -> a -> String
edgeToDot from to = printf "\t\"%s\" -> \"%s\"\n"
                           (show from)
                           (show to)



-- | Add meta info to a list of edges to make them into a proper .dot file
dotBoilerplate :: String -> String
dotBoilerplate str =
      "digraph G {\n\
      \\tnode [shape = box, color = gray, fontname = \"Courier\"];\n\
      \\tedge [fontname = \"Courier\"];\n\
      \" ++ str ++ "\n\
      \}\n"