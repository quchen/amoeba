-- | Main entry module point for an ordinary node.

module Main.Node (main) where

import qualified Config.Getter as Config
import Node
import Types
import Utilities

-- | Start a single node.
main :: IO ()
main = do

      config <- Config.node

      prepareOutputBuffers
      (output, _) <- outputThread (_maxChanSize config)
      startNode Nothing output config
