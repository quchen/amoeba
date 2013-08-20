module Main (main) where


import           Control.Concurrent.STM
import           System.IO
import           Network
import           Control.Exception
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)

import Utilities
import Types


-- Graph consisting of a set of nodes, each having a set of neighbours.
data Graph a = Graph (Map a (Set a))

-- | Dirty string-based hacks to convert a Graph to .dot
graphToDot :: Show a => Graph a -> String
graphToDot (Graph g) = boilerplate . intercalate "\n\n" $ map (uncurry vertexToDot) $ Map.assocs g
      where boilerplate str = "digraph G {\nnode [shape = box, color = gray, fontname = \"Courier\"];\nedge [fontname = \"Courier\"];\n" ++ str ++ "\n}\n"

vertexToDot :: Show a => a -> Set a -> String
vertexToDot vertex edges = intercalate "\n" $ map (\x -> show vertex ++ " -> " ++ show x) $ Set.elems edges



main :: IO ()
main = do

      putStrLn "Network drawing server"

      -- Initialization
      putStrLn "Initializing"
      graph <- newTVarIO $ Graph Map.empty

      socket <- listenOn $ PortNumber 20000
      putStrLn "Starting server loop"
      drawingServerLoop socket graph



drawingServerLoop :: Socket -> TVar (Graph Node) -> IO ()
drawingServerLoop socket graph = do

      bracket (accept socket) (\(h, _, _) -> hClose h) $ \(h, _, _) -> do
            async <- getData h graph
            return ()


getData :: Handle -> TVar (Graph Node) -> IO ()
getData h graph = do

      vertex <- receive' h
      send' h OK

      let insert (v, n) (Graph g) = Graph $ Map.insert v n g
      atomically $ modifyTVar graph (insert vertex)