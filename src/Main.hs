-- | Main module for an ordinary node.

module Main where

import CmdArgParser
import Node

-- | Starts a single node.
main :: IO ()
main = parseArgs >>= startNode Nothing >> return ()
