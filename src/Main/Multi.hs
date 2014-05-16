-- | Main entry point for the multi-node client.
--
--   Launches a node pool without further services. In other words, it's like a
--   bootstrap server without the bootstrapping part.

{-# LANGUAGE NumDecimals #-}

module Main.Multi (main) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import qualified Data.Traversable as T
import           Text.Printf

import           NodePool
import           Utilities
import qualified Config.Getter as Config

import           Control.Lens.Operators
import qualified Types.Lens as L

import           Types (Microseconds(..))



main :: IO ()
main = multiNodeMain

multiNodeMain :: IO ()
multiNodeMain = do

      config <- Config.multi

      prepareOutputBuffers
      (output, oThread) <- outputThread (config ^. L.nodeConfig . L.maxChanSize)
      (`finally` cancel oThread) $ do

      let poolSize = config ^. L.poolConfig . L.poolSize
      printf "Starting pool with %d nodes" poolSize

      ldc <- newChan
      npThread <- async (nodePool poolSize
                                  (config ^. L.nodeConfig)
                                  ldc
                                  output
                                  Nothing) -- No termination trigger

      void (forever (delay (Microseconds 10e8)))
            `finally` (wait npThread >>= T.traverse cancel)