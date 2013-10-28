-- | Main module for an ordinary node.

module Main (main) where

import Control.Concurrent.Async
import CmdArgParser
import Node
import Types

-- | Starts a single node.
main :: IO ()
main = do config <- parseArgs
          startNode Nothing config


