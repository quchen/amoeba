-- | Main module for an ordinary node.

module Main (main) where

import qualified Config.Getter as Config
import Node
import Types
import Utilities

-- | Starts a single node.
main :: IO ()
main = do

      config <- Config.node

      prepareOutputBuffers
      (output, _) <- outputThread (_maxChanSize config)
      startNode Nothing output config
