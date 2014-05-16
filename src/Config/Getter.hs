-- | (Only) API to read configurations from all available sources. Intended as
--   a qualified import, e.g. as \"Config\".
--
--   Throws an exception on failure (which hopefully crashes the program unless
--   I get the glorious idea of a catchall for some reason).

{-# LANGUAGE OverloadedStrings #-}

module Config.Getter (
        node
      , bootstrap
      , drawing
      , multi
) where

import qualified Data.Traversable as T
import Data.Monoid
import Control.Monad
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Control.Lens.Operators
import qualified Types.Lens as L

import qualified Config.ConfigFile   as File
import qualified Config.CmdArgParser as CmdArg
import qualified Config.Default      as Default
import qualified Config.Verify       as Verify
import Config.OptionModifier
import Types



runModifier :: config                       -- ^ Default config
            -> [IO (OptionModifier config)] -- ^ List of modifiers
            -> IO config
runModifier defaultConfig ioMods = do
      mods <- (fmap mconcat . T.sequenceA) ioMods
      return (applyOptionModifier mods defaultConfig)



node :: IO NodeConfig
node = do let mods = [ File.nodeModifier, CmdArg.nodeModifier ]
          cfg <- runModifier Default.nodeConfig mods
          Verify.node cfg
          printIfVerbose cfg cfg
          return cfg



bootstrap :: IO BootstrapConfig
bootstrap = do let mods = [ File.bootstrapModifier, CmdArg.bootstrapModifier ]
               cfg <- runModifier Default.bootstrapConfig mods
               Verify.bootstrap cfg
               printIfVerbose (cfg ^. L.nodeConfig) cfg
               return cfg



drawing :: IO DrawingConfig
drawing = do let mods = [ File.drawingModifier, CmdArg.drawingModifier ]
             cfg <- runModifier Default.drawingConfig mods
             Verify.drawing cfg
             printIfVerbose (cfg ^. L.nodeConfig) cfg
             return cfg



multi :: IO MultiConfig
multi = do let mods = [ File.multiModifier, CmdArg.multiModifier ]
           cfg <- runModifier Default.multiConfig mods
           Verify.multi cfg
           printIfVerbose (cfg ^. L.nodeConfig) cfg
           return cfg


-- | Print a configuration if the predicate is met.
printIfVerbose :: PrettyShow config => NodeConfig -> config -> IO ()
printIfVerbose nodeCfg cfg =
      when (nodeCfg ^. L.verbosity >= Debug)
           (Text.putStrLn (Text.unlines
                 [ "\ESC[31m###  Configuration:  #############\ESC[0m"
                 , pretty cfg
                 , "\ESC[31m##################################\ESC[0m"
                 ]))