-- | Launches a node pool without further services. In other words, it's like
--   a bootstrap server without the bootstrapping part.

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
main = multiNodeMain

multiNodeMain :: IO ()
multiNodeMain = do

      -- Preliminaries
      bsConfig <- parseBSArgs -- TODO: Multi-node config parser
      (output, _) <- outputThread (_maxChanSize (_nodeConfig bsConfig))

      printf "Starting drawing server with %d nodes"
             (_poolSize bsConfig)

      -- Node pool
      ldc <- newChan
      terminate <- newEmptyMVar -- TODO: Never actually used. Refactor node pool?
      nodePool (_poolSize bsConfig)
               (_nodeConfig bsConfig)
               ldc
               output
               terminate
