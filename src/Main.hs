-- | Main module for an ordinary node.

module Main (main) where

import CmdArgParser
import Node
import Types

-- | Starts a single node.
main :: IO ()
main = parseArgs >>= startNode Nothing . setBootstrap >> return ()

-- | Hardcoded bootstrap servers
setBootstrap x = x { _bootstrapServers = [To $ Node "localhost" 20000] }
-- TODO: proper discovery
