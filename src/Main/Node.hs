-- | Main entry module point for an ordinary node.

module Main.Node (main) where

import           Control.Concurrent.Async
import           Control.Exception

import           Node
import           Types
import           Utilities
import qualified Config.Getter as Config

-- | Start a single node.
main :: IO ()
main = do

      config <- Config.node

      prepareOutputBuffers
      (output, oThread) <- outputThread (_maxChanSize config)
      startNode Nothing output config
            `finally` cancel oThread
