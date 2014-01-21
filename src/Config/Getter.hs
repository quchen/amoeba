-- | (Only) API to read configurations from all available sources. Intended as
--   a qualified import, e.g. as \"Config\".
--
--   Throws an exception on failure (which hopefully crashes the program unless
--   I get the glorious idea of a catchall for some reason).

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
import qualified Config.Verifier     as Verify
import Config.OptionModifier
import Types



runModifier :: a -- ^ Default config
            -> [IO (OptionModifier a)] -- ^ List of modifiers
            -> IO a
runModifier defaultConfig ioMods = do
      mods <- (fmap mconcat . T.sequenceA) ioMods
      return (applyOptionModifier mods defaultConfig)



node :: IO NodeConfig
node = do let mods = [ File.nodeModifier, CmdArg.nodeModifier ]
          cfg <- runModifier Default.nodeConfig mods
          Verify.node cfg
          return cfg



bootstrap :: IO BootstrapConfig
bootstrap = do let mods = [ File.bootstrapModifier, CmdArg.bootstrapModifier ]
               cfg <- runModifier Default.bootstrapConfig mods
               Verify.bootstrap cfg
               return cfg



drawing :: IO DrawingConfig
drawing = do let mods = [ File.drawingModifier, CmdArg.drawingModifier ]
             cfg <- runModifier Default.drawingConfig mods
             Verify.drawing cfg
             return cfg



multi :: IO MultiConfig
multi = do let mods = [ File.multiModifier, CmdArg.multiModifier ]
           cfg <- runModifier Default.multiConfig mods
           Verify.multi cfg
           return cfg
