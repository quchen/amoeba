-- | Main entry point for the multi-node client.
--
--   Launches a node pool without further services. In other words, it's like a
--   bootstrap server without the bootstrapping part.

module Main.Multi (main) where

import           Control.Monad
import           Control.Concurrent
import           Text.Printf

import Utilities
import qualified Config.Getter as Config
import NodePool

import qualified Types.Lens as L
import Control.Lens ((^.))



main :: IO ()
main = multiNodeMain

multiNodeMain :: IO ()
multiNodeMain = do

      config <- Config.multi

      prepareOutputBuffers
      (output, _) <- outputThread (config ^. L.nodeConfig . L.maxChanSize)

      let poolSize = config ^. L.poolConfig . L.poolSize
      printf "Starting pool with %d nodes" poolSize

      ldc <- newChan
      terminate <- newEmptyMVar -- TODO: Never actually used. Refactor node pool?
      nodePool poolSize
               (config ^. L.nodeConfig)
               ldc
               output
               terminate

      forever (threadDelay (10^7))
