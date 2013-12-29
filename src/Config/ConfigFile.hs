-- | Read configuration settings from a configuration file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Config.ConfigFile (
        nodeConfig
      , bootstrapConfig
      , drawingConfig
      , multiConfig
) where

import Data.Char (toLower)
import Data.Word
import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as Text
import qualified Data.Traversable as T

import qualified Config.DefaultConfig as Default
import qualified Types as Ty
import Config.OptionModifier




loadConfigs :: [C.Worth FilePath] -> IO C.Config
loadConfigs files = C.load files



-- | Get the node option modifier from the top level of the configuration file
nodeConfig :: C.Config -> IO (OptionModifier Ty.NodeConfig)
nodeConfig = runReaderT (nodeModifier [])

-- | Get the bootstrap option modifier by first reading the top level of the
--   configuration file, and then overwriting the values obtained by what is
--   found under the \"bootstrap\" prefix.
bootstrapConfig :: C.Config -> IO (OptionModifier Ty.BootstrapConfig)
bootstrapConfig = runReaderT bootstrapModifier

-- | Get the drawing option modifier by first reading the top level of the
--   configuration file, and then overwriting the values obtained by what is
--   found under the \"drawing\" prefix.
drawingConfig :: C.Config -> IO (OptionModifier Ty.DrawingConfig)
drawingConfig = runReaderT drawingModifier

-- | Get the multi option modifier by first reading the top level of the
--   configuration file, and then overwriting the values obtained by what is
--   found under the \"multi\" prefix.
multiConfig :: C.Config -> IO (OptionModifier Ty.MultiConfig)
multiConfig = runReaderT multiModifier





-- | Get the pool node modifier given a certain "Prefix".
nodeModifier :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
nodeModifier prefixes = (fmap mconcat . T.sequenceA) mods
      where mods = map ($ prefixes)
                  [ serverPort
                  , minNeighbours
                  , maxNeighbours
                  , maxChanSize
                  , bounces
                  , acceptP
                  , maxSoftBounces
                  , shortTickRate
                  , mediumTickRate
                  , longTickRate
                  , poolTimeout
                  , verbosity
                  , \_ -> pure mempty -- TODO: Specify bootstrap servers
                  , floodMessageCache
                  ]



-- | Get the pool modifier given a certain "Prefix".
poolModifier :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.PoolConfig)
poolModifier prefixes = (fmap mconcat . T.sequenceA) mods
      where mods = map ($ prefixes) [ poolSize ]



-- | Read the general config, before overwriting it with values with the
--   @bootstrap@ prefix, i.e. for the @foo@ setting first looks for @foo@ and
--   then for @bootstrap.foo@.
bootstrapModifier :: ReaderT C.Config IO (OptionModifier Ty.BootstrapConfig)
bootstrapModifier = (fmap mconcat . T.sequenceA) mods
      where mods = [ liftNodeModifier <$> nodeModifier noPrefix
                   , liftPoolModifier <$> poolModifier noPrefix
                   , restartEvery prefix
                   , restartMinimumPeriod prefix
                   , liftNodeModifier <$> nodeModifier prefix
                   , liftPoolModifier <$> poolModifier prefix
                   ]
            noPrefix = []
            prefix = ["bootstrap"]



-- | Same as "bootstrapModifier", but for the drawing server (using the
--   @drawing@ prefix).
drawingModifier :: ReaderT C.Config IO (OptionModifier Ty.DrawingConfig)
drawingModifier = (fmap mconcat . T.sequenceA) mods
      where mods = [ liftNodeModifier <$> nodeModifier noPrefix
                   , liftPoolModifier <$> poolModifier noPrefix
                   , liftNodeModifier <$> nodeModifier prefix
                   , liftPoolModifier <$> poolModifier prefix
                   ]
            noPrefix = []
            prefix = ["drawing"]



-- | Same as "bootstrapModifier", but for the multi client (using the
--   @multi@ prefix).
multiModifier :: ReaderT C.Config IO (OptionModifier Ty.MultiConfig)
multiModifier = (fmap mconcat . T.sequenceA) mods
      where mods = [ liftNodeModifier <$> nodeModifier noPrefix
                   , liftPoolModifier <$> poolModifier noPrefix
                   , liftNodeModifier <$> nodeModifier prefix
                   , liftPoolModifier <$> poolModifier prefix
                   ]
            noPrefix = []
            prefix = ["drawing"]



-- | A prefix is what leads to an option in a hierarchy; they're similar to
--   parent directories. For example the expression "foo.bar.qux" corresponds to
--   @[\"foo\", \"bar\"] :: Prefixes@, with the option name being @qux@.
type Prefixes = [C.Name]



lookupC :: C.Configured a
        => Prefixes -- ^ List of prefixes, e.g. ["foo", "bar"] for foo.bar.*
        -> C.Name   -- ^ Option name
        -> ReaderT C.Config IO (Maybe a)
lookupC prefixes name = do
      cfg <- ask
      let fullName | null prefixes = name
                   | otherwise = Text.intercalate "." prefixes <> "." <> name
      liftIO (C.lookup cfg fullName)



serverPort' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
serverPort' prefixes = lookupC prefixes "serverPort"

serverPort :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
serverPort prefixes = toModifier <$> serverPort' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._serverPort = x })
            toModifier _ = mempty



minNeighbours' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
minNeighbours' prefixes = lookupC prefixes "minNeighbours"

minNeighbours :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
minNeighbours prefixes = toModifier <$> minNeighbours' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._minNeighbours = x })
            toModifier _ = mempty



maxNeighbours' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
maxNeighbours' prefixes = lookupC prefixes "maxNeighbours"

maxNeighbours :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
maxNeighbours prefixes = toModifier <$> maxNeighbours' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._maxNeighbours = x })
            toModifier _ = mempty



verbosity' :: Prefixes -> ReaderT C.Config IO (Maybe Ty.Verbosity)
verbosity' prefixes = do
      result <- lookupC prefixes "verbosity"
      return $ case result of
            Just x  -> parseVerbosity x
            Nothing -> Nothing

      where
            strToLower = map toLower
            parseVerbosity (strToLower -> y)
                  | y == "mute"    = Just Ty.Mute
                  | y == "quiet"   = Just Ty.Quiet
                  | y == "default" = Just Ty.Default
                  | y == "debug"   = Just Ty.Debug
                  | y == "chatty"  = Just Ty.Chatty
            parseVerbosity _ = Nothing

verbosity :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
verbosity prefixes = toModifier <$> verbosity' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._verbosity = x })
            toModifier _ = mempty



maxChanSize' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
maxChanSize' prefixes = lookupC prefixes "maxChanSize"

maxChanSize :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
maxChanSize prefixes = toModifier <$> maxChanSize' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._maxChanSize = x })
            toModifier _ = mempty



bounces' :: Prefixes -> ReaderT C.Config IO (Maybe Word)
bounces' prefixes = lookupC prefixes "bounces"

bounces :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
bounces prefixes = toModifier <$> bounces' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._bounces = x })
            toModifier _ = mempty



acceptP' :: Prefixes -> ReaderT C.Config IO (Maybe Double)
acceptP' prefixes = lookupC prefixes "acceptP"

acceptP :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
acceptP prefixes = toModifier <$> acceptP' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._acceptP = x })
            toModifier _ = mempty



maxSoftBounces' :: Prefixes -> ReaderT C.Config IO (Maybe Word)
maxSoftBounces' prefixes = lookupC prefixes "maxSoftBounces"

maxSoftBounces :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
maxSoftBounces prefixes = toModifier <$> maxSoftBounces' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._maxSoftBounces = x })
            toModifier _ = mempty



shortTickRate' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
shortTickRate' prefixes = lookupC prefixes "shortTickRate"

shortTickRate :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
shortTickRate prefixes = toModifier <$> shortTickRate' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._shortTickRate = x })
            toModifier _ = mempty



mediumTickRate' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
mediumTickRate' prefixes = lookupC prefixes "mediumTickRate"

mediumTickRate :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
mediumTickRate prefixes = toModifier <$> mediumTickRate' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._mediumTickRate = x })
            toModifier _ = mempty



longTickRate' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
longTickRate' prefixes = lookupC prefixes "longTickRate"

longTickRate :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
longTickRate prefixes = toModifier <$> longTickRate' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._longTickRate = x })
            toModifier _ = mempty



poolTimeout' :: Prefixes -> ReaderT C.Config IO (Maybe Double)
poolTimeout' prefixes = lookupC prefixes "poolTimeout"

poolTimeout :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
poolTimeout prefixes = toModifier <$> poolTimeout' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._poolTimeout = x })
            toModifier _ = mempty



floodMessageCache' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
floodMessageCache' prefixes = lookupC prefixes "floodMessageCache"

floodMessageCache :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
floodMessageCache prefixes = toModifier <$> floodMessageCache' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._floodMessageCache = x })
            toModifier _ = mempty


-- TODO
bootstrapServers' :: Prefixes -> ReaderT C.Config IO (Maybe a)
bootstrapServers' prefixes = undefined

bootstrapServers :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
bootstrapServers prefixes = undefined
      where
            foo = toModifier <$> bootstrapServers' prefixes
            toModifier (Just x) = OptionModifier (\c -> c { Ty._bootstrapServers = x })
            toModifier _ = mempty



poolSize' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
poolSize' prefixes = lookupC prefixes "poolSize"

poolSize :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.PoolConfig)
poolSize prefixes = toModifier <$> poolSize' prefixes
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._poolSize = x })
            toModifier _ = mempty



restartEvery' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
restartEvery' prefixes = lookupC prefixes "restartEvery"

restartEvery :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.BootstrapConfig)
restartEvery _ = toModifier <$> restartEvery' ["bootstrap"]
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._restartEvery = x })
            toModifier _ = mempty



restartMinimumPeriod' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
restartMinimumPeriod' prefixes = lookupC prefixes "restartMinimumPeriod"

restartMinimumPeriod :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.BootstrapConfig)
restartMinimumPeriod _ = toModifier <$> restartMinimumPeriod' ["bootstrap"]
      where
            toModifier (Just x) = OptionModifier (\c -> c { Ty._restartMinimumPeriod = x })
            toModifier _ = mempty

