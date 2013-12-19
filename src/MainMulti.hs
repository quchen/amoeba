-- | Launches a node pool without further services. In other words, it's like
--   a bootstrap server without the bootstrapping part.

module Main (main) where

import           Control.Concurrent
import           Text.Printf

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

      printf "Starting pool with %d nodes\n"
             (_poolSize bsConfig)

      -- Node pool
      ldc <- newChan
      terminate <- newEmptyMVar -- TODO: Never actually used. Refactor node pool?
      nodePool (_poolSize bsConfig)
               (_nodeConfig bsConfig)
               ldc
               output
               terminate
