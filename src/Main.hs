-- | Main module for an ordinary node.

module Main (main) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import CmdArgParser
import Node
import Types
import Utilities

-- | Starts a single node.
main :: IO ()
main = do
      config <- parseNodeArgs
      (output, _) <- outputThread (_maxChanSize config)
      startNode Nothing output config
