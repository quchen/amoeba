-- | (Only) API to read configurations from all available sources. Intended as
--   a qualified import, e.g. as \"Config\".


-- TODO: Warn/error on bad parameters instead of blindly taking them
--
--       - short < medium < long tickrate, timeout > long
--       - Port should be between 0 and 2^16-1
--       - poolsize > minneighbours: the BS server can't satisfy itself otherwise
--       - restartEvery should be larger than 0.
--           - 1: Every new neighbour triggers a restart (if coolown is over).
--           - 2: Same as 1, since the restart makes one node reconnect to the
--                pool
--           - 3: Every other new node triggers restart
--               TODO: subtract this offset of 1 everywhereto make configuration
--                     easier?

module Config.Getter (
        node
      , bootstrap
      , drawing
      , multi
) where

import qualified Data.Traversable as T
import Data.Monoid

import qualified Config.ConfigFile   as File
import qualified Config.CmdArgParser as CmdArg
import qualified Config.Default      as Default
import Config.OptionModifier
import Types


runModifier :: a -- ^ Default config
            -> [IO (OptionModifier a)] -- ^ List of modifiers
            -> IO a
runModifier defaultConfig mods = do
      mod <- (fmap mconcat . T.sequenceA) mods
      return (applyOptionModifier mod defaultConfig)



node :: IO NodeConfig
node = runModifier Default.nodeConfig mods
      where mods = [ CmdArg.nodeModifier, File.nodeModifier]



bootstrap :: IO BootstrapConfig
bootstrap = runModifier Default.bootstrapConfig mods
      where mods = [ CmdArg.bootstrapModifier, File.bootstrapModifier]



drawing :: IO DrawingConfig
drawing = runModifier Default.drawingConfig mods
      where mods = [ CmdArg.drawingModifier, File.drawingModifier]



multi :: IO MultiConfig
multi = runModifier Default.multiConfig mods
      where mods = [ CmdArg.multiModifier, File.multiModifier]
