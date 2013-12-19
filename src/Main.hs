-- | Main module for an ordinary node.

module Main (main) where

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
