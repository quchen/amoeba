-- | Launches a node pool without further services. In other words, it's like
--   a bootstrap server without the bootstrapping part.

module Main (main) where

import           Control.Monad
import           Control.Concurrent
import           Text.Printf

import Utilities
import qualified CmdArgParser as CmdArgParser
import qualified DefaultConfig as Default
import Types
import NodePool



main :: IO ()
main = multiNodeMain

multiNodeMain :: IO ()
multiNodeMain = do

      cmdArgs <- CmdArgParser.multiArgs
      let config = applyOptionModifier cmdArgs Default.multiConfig


      (output, _) <- outputThread (_maxChanSize (_nodeConfig config))

      printf "Starting pool with %d nodes\n"
             (_poolSize (_poolConfig config))

      ldc <- newChan
      terminate <- newEmptyMVar -- TODO: Never actually used. Refactor node pool?
      nodePool (_poolSize (_poolConfig config))
               (_nodeConfig config)
               ldc
               output
               terminate

      forever (threadDelay (10^7))
