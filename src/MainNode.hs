-- | Main module for an ordinary node.

module Main (main) where

import qualified Config.CmdArgParser as CmdArgParser
import qualified Config.DefaultConfig as Default
import Config.OptionModifier
import Node
import Types
import Utilities

-- | Starts a single node.
main :: IO ()
main = do

      cmdArgs <- CmdArgParser.nodeArgs
      let config = applyOptionModifier cmdArgs Default.nodeConfig

      (output, _) <- outputThread (_maxChanSize config)
      startNode Nothing output config
