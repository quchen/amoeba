-- | Main entry point for the drawing server.
--
--   The drawing server asks willing nodes to send it a list of all neighbours.
--   The collective information can then be used to analyze the large-scale
--   structure of the entire network.
--
--   Local abbreviations:
--       STG = Server to graph.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Main.Drawing (main) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List (intercalate)
import qualified Data.Foldable as F
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Text.Printf

import qualified Network.Simple.TCP as N
import qualified Pipes.Concurrent as P

import           Control.Lens.Operators
import qualified Control.Lens as L
import qualified Types.Lens as L

import           NodePool
import           Types
import           Utilities
import qualified Config.Getter as Config





main :: IO ()
main = drawingServerMain



-- | Main entry point of the drawing server.
drawingServerMain :: IO ()
drawingServerMain = do

      config <- Config.drawing

      prepareOutputBuffers
      (output, _) <- outputThread (config ^. L.nodeConfig . L.maxChanSize)

      let poolSize = config ^. L.poolConfig . L.poolSize
      ldc <- newChan
      terminate <- newEmptyMVar -- TODO: Never actually used. Refactor node pool?
      nodePool poolSize
               (config ^. L.nodeConfig)
               ldc
               output
               terminate

      printf "Starting drawing server with %d nodes\n"poolSize
      drawingServer config output ldc



-- | Setup for the drawing server before it starts looping.
drawingServer :: DrawingConfig
              -> IOQueue
              -> Chan NormalSignal -- ^ Local direct connection to the node pool
              -> IO ()
drawingServer config ioq ldc = do
      -- Server to graph worker
      stg <- spawn (config ^. L.nodeConfig . L.maxChanSize . L.to P.Bounded)
      forkIO (graphWorker config ioq stg)

      let port = config ^. L.nodeConfig . L.serverPort
      N.listen (N.Host "127.0.0.1") (show port) $ \(socket, _addr) -> do
            let selfTo = To (Node "127.0.0.1" port)
            forkIO (networkAsker config
                                 (config ^. L.poolConfig . L.poolSize)
                                 selfTo
                                 ldc)
            incomingLoop ioq stg socket



-- | Drawing server loop; does the actual work after being set up by
--   "drawingServer".
incomingLoop :: IOQueue
             -> PChan (To, Set To) -- ^ (Node, DSNs of that node)
             -> N.Socket -- ^ Socket for incoming connections (used by nodes to
                         --   contact the drawing server upon request)
             -> IO ()
incomingLoop _ioq stg serverSock = forever $
      N.acceptFork serverSock $ \(clientSock, _clientAddr) ->
            receive clientSock >>= \case

                  -- Good answer
                  Just (NeighbourList node neighbours) -> do
                        -- toIO' ioq (putStrLn ("Received node data from" ++ show node))
                        (atomically . void) (P.send (stg ^. L.pOutput)
                                                    (node, neighbours))

                  -- Invalid answer
                  Just _ -> return () -- toIO' ioq (putStrLn "Invalid signal received")

                  -- No answer
                  _ -> return () -- toIO' ioq (putStrLn "No signal received")



-- | Listen on the incoming "PChan" and merge new information into the graph.
graphWorker :: DrawingConfig
            -> IOQueue
            -> PChan (To, Set To)
            -> IO ()
graphWorker config ioq stg = do
      t'graph <- newTVarIO (Graph Map.empty)
      forkIO (graphDrawer config ioq t'graph)
      forever $ makeTimestamp >>= \t -> atomically $ do
            Just (node, neighbours) <- P.recv (_pInput stg) -- TODO: Error handling on Nothing
            modifyTVar t'graph (insertNode t node neighbours)



-- | Read the graph and compiles it to .dot format
graphDrawer :: DrawingConfig
            -> IOQueue
            -> TVar (Graph To)
            -> IO ()
graphDrawer config ioq t'graph = (initialDelay >>) . forever $ do
      delay (config ^. L.drawEvery)
      cleanup config t'graph
      graph <- atomically (readTVar t'graph)
      let graphSize (Graph g) = Map.size g
          s = graphSize graph
      (toIO' ioq . STDLOG) (printf "Drawing graph. Current network size: %d nodes\n" s)
      writeFile (config ^. L.drawFilename)
                (graphToDot graph)

      where
            -- Delay the drawer a bit initially, so the 'networkAsker' can do
            -- its job first. If this isn't done, the drawer might draw the old
            -- state, while pretty much at the same time the new network
            -- answers come in.
            initialDelay = delay (config ^. L.nodeConfig . L.longTickRate)



-- | Remove edges that haven't been updated in some time.
cleanup :: DrawingConfig
        -> TVar (Graph To)
        -> IO ()
cleanup config t'graph = do
      t <- makeTimestamp
      let timedOut (Timestamp now) (Timestamp lastInput, _) =
                now - lastInput > _drawTimeout config
      atomically (modifyTVar t'graph (filterEdges (not . timedOut t)))



-- | Periodically send out flood messages to get the network data
networkAsker :: DrawingConfig
             -> Int -- ^ Own node pool size
             -> To -- ^ Own address for reverse connection
             -> Chan NormalSignal -- ^ LDC to the pool
             -> IO ()
networkAsker config poolSize toSelf ldc = forever $ do
      delay (_drawEvery config)
      t <- makeTimestamp
      let signal = Flood t (SendNeighbourList toSelf)
      forM_ [1..poolSize]
            (\_i -> writeChan ldc signal)




-- | Graph consisting of a set of nodes, each having a set of neighbours.
data Graph a = Graph (Map a (Timestamp, Set a))




-- #############################################################################
-- ###  Dot-file generation  ###################################################
-- #############################################################################


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
      \      node [shape = box, color = gray, fontname = \"Courier\"];\n\
      \      edge [fontname = \"Courier\", len = 6];\n\
      \      " ++ str ++ "\
      \}\n"



-- #############################################################################
-- ###  Graph modifying API  ###################################################
-- #############################################################################


-- | Filter a graph's edges by a predicate.
filterEdges :: ((Timestamp, Set To) -> Bool) -> Graph To -> Graph To
filterEdges p (Graph g) = Graph (Map.filter p g)



-- | Insert/replace node information in the graph.
insertNode :: Timestamp
           -> To        -- ^ Node
           -> (Set To)  -- ^ List of DSNs
           -> Graph To
           -> Graph To
insertNode t node neighbours (Graph g) = Graph (Map.insert node
                                                           (t, neighbours)
                                                           g)