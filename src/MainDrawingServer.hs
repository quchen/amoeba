-- | The drawing server asks willing nodes to send it a list of all neighbours.
--   The collective information can then be used to analyze the large-scale
--   structure of the entire network.

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Text.Printf
import qualified Data.Foldable as F

import qualified Pipes.Concurrent as P
import qualified Network.Simple.TCP as N

import Utilities
import Types
import NodePool
import qualified Config.Getter as Config
import Config.OptionModifier (HasNodeConfig(..), HasPoolConfig(..))



main :: IO ()
main = drawingServerMain

drawingServerMain :: IO ()
drawingServerMain = do

      config <- Config.drawing

      (output, _) <- outputThread (_maxChanSize (_nodeConfig config))

      ldc <- newChan
      terminate <- newEmptyMVar -- TODO: Never actually used. Refactor node pool?
      nodePool (_poolSize (_poolConfig config))
               (_nodeConfig config)
               ldc
               output
               terminate

      printf "Starting drawing server with %d nodes"
             (_poolSize (_poolConfig config))
      drawingServer config output ldc


drawingServer :: DrawingConfig
              -> IOQueue
              -> Chan NormalSignal
              -> IO ()
drawingServer config ioq ldc = do
      -- Server to graph worker
      stg <- spawn (P.Bounded (_maxChanSize (_nodeConfig config)))
      forkIO (graphWorker stg)

      let port = _serverPort (_nodeConfig config)
      N.listen (N.Host "127.0.0.1") (show port) $ \(socket, _addr) -> do
            let selfTo = To (Node "127.0.0.1" port)
            forkIO (networkAsker (_poolSize (_poolConfig config)) selfTo ldc)
            incomingLoop ioq stg socket



incomingLoop :: IOQueue
             -> PChan (To, Set To)
             -> N.Socket
             -> IO ()
incomingLoop _ioq stg serverSock = forever $ do
      N.acceptFork serverSock $ \(clientSock, _clientAddr) -> do
            receive clientSock >>= \case
                  Just (NeighbourList node neighbours) -> do
                        -- toIO' ioq (putStrLn ("Received node data from" ++ show node))
                        atomically (void (P.send (_pOutput stg) (node, neighbours)))
                  Just _other_signal -> return () -- toIO' ioq (putStrLn "Invalid signal received")
                  _no_signal -> return () -- toIO' ioq (putStrLn "No signal received")



graphWorker :: PChan (To, Set To) -> IO ()
graphWorker stg = do
      t'graph <- newTVarIO (Graph Map.empty)
      forkIO (graphDrawer t'graph)
      forever $ makeTimestamp >>= \t -> atomically $ do
            Just (node, neighbours) <- P.recv (_pInput stg) -- TODO: Error handling on Nothing
            modifyTVar t'graph
                       (\(Graph g) -> Graph (Map.insert node (t, neighbours) g))
            -- Listen for new neighbour lists, add them to graph
            -- Plot graph


-- | Read the graph and compiles it to .dot format
graphDrawer :: TVar (Graph To) -> IO ()
graphDrawer t'graph = forever $ do
      threadDelay (10^6) -- TODO: Configurable
      cleanup t'graph
      graph <- atomically (readTVar t'graph)
      let graphSize (Graph g) = Map.size g
          s = graphSize graph
      printf "Drawing graph. Current network size: %d nodes" s -- TODO: Use IOQueue
      writeFile "network_graph.dot" (graphToDot graph) -- TODO: Make filename configurable



cleanup :: TVar (Graph To) -> IO ()
cleanup t'graph = do
      t <- makeTimestamp
      let timedOut (Timestamp now) (Timestamp lastInput, _) =
                now - lastInput > 3 -- TODO: read from config
      atomically (modifyTVar t'graph
                             (\(Graph g) -> Graph (Map.filter (not . timedOut t) g)))



-- | Periodically send out flood messages to get the network data
networkAsker :: Int -> To -> Chan NormalSignal -> IO ()
networkAsker poolSize toSelf ldc = forever $ do
      threadDelay (10^6) -- TODO: Configurable
      t <- makeTimestamp
      let signal = Flood t (SendNeighbourList toSelf)
      forM_ [1..poolSize] $ \_ -> writeChan ldc signal




-- | Graph consisting of a set of nodes, each having a set of neighbours.
data Graph a = Graph (Map a (Timestamp, Set a))



-- | Dirty string-based hacks to convert a Graph to .dot
graphToDot :: Graph To -> String
graphToDot (Graph g) =
      dotBoilerplate . intercalate "\n" . map (vertexToDot . stripTimestamp) $ Map.assocs g
      where stripTimestamp (a, (_t, b)) = (a,b)



vertexToDot :: (To, Set To) -> String
vertexToDot (start, ends) = F.foldMap (edgeToDot start) ends



edgeToDot :: To -> To -> String
edgeToDot from to =
      printf "\t\"%s\" -> \"%s\"\n"
             (show' from)
             (show' to)
      where show' :: To -> String
            show' (To (Node _host port)) = printf "%d" port
            -- TODO: print host as well (cut out for brevity/local testing)



-- | Add meta info to a list of edges to make them into a proper .dot file
dotBoilerplate :: String -> String
dotBoilerplate str =
      "digraph G {\n\
      \\tnode [shape = box, color = gray, fontname = \"Courier\"];\n\
      \\tedge [fontname = \"Courier\", len = 6];\n\
      \" ++ str ++ "\n\
      \}\n"