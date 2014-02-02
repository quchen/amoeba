-- | Read configuration settings from a configuration file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Config.ConfigFile (
        nodeModifier
      , bootstrapModifier
      , drawingModifier
      , multiModifier
) where

import Data.Char (toLower)
import Data.Word
import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad.Reader

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as Text
import qualified Data.Traversable as T

import qualified Types as Ty
import Config.OptionModifier



-- | Files to read the config from. The later in the list, the higher the
--   precedence of the contained settings.
configFiles = [ C.Optional "$(HOME)/.local/share/amoeba/amoeba.cfg"
              , C.Optional "$(HOME)/.amoeba.cfg"
              , C.Optional "amoeba.cfg"
              ]



-- | Get the node option modifier from the top level of the configuration file
nodeModifier :: IO (OptionModifier Ty.NodeConfig)
nodeModifier = C.load configFiles >>= runReaderT (nodeModifier' [])

-- | Get the bootstrap option modifier by first reading the top level of the
--   configuration file, and then overwriting the values obtained by what is
--   found under the \"bootstrap\" prefix.
bootstrapModifier :: IO (OptionModifier Ty.BootstrapConfig)
bootstrapModifier = C.load configFiles >>= runReaderT bootstrapModifier'

-- | Get the drawing option modifier by first reading the top level of the
--   configuration file, and then overwriting the values obtained by what is
--   found under the \"drawing\" prefix.
drawingModifier :: IO (OptionModifier Ty.DrawingConfig)
drawingModifier = C.load configFiles >>= runReaderT drawingModifier'

-- | Get the multi option modifier by first reading the top level of the
--   configuration file, and then overwriting the values obtained by what is
--   found under the \"multi\" prefix.
multiModifier :: IO (OptionModifier Ty.MultiConfig)
multiModifier = C.load configFiles >>= runReaderT multiModifier'





-- | Get the pool node modifier given a certain "Prefix".
nodeModifier' :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
nodeModifier' prefixes = (fmap mconcat . T.sequenceA) mods where
      mods = map ($ prefixes)
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
poolModifier' :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.PoolConfig)
poolModifier' prefixes = (fmap mconcat . T.sequenceA) mods where
      mods = map ($ prefixes) [ poolSize ]



-- | Read the general config, before overwriting it with values with the
--   @bootstrap@ prefix, i.e. for the @foo@ setting first looks for @foo@ and
--   then for @bootstrap.foo@.
bootstrapModifier' :: ReaderT C.Config IO (OptionModifier Ty.BootstrapConfig)
bootstrapModifier' = (fmap mconcat . T.sequenceA) mods where
      mods = [ liftNodeModifier <$> nodeModifier' noPrefix
             , liftPoolModifier <$> poolModifier' noPrefix
             , restartEvery prefix
             , restartMinimumPeriod prefix
             , liftNodeModifier <$> nodeModifier' prefix
             , liftPoolModifier <$> poolModifier' prefix
             ]
      noPrefix = []
      prefix = ["bootstrap"]



-- | Same as "bootstrapModifier", but for the drawing server (using the
--   @drawing@ prefix).
drawingModifier' :: ReaderT C.Config IO (OptionModifier Ty.DrawingConfig)
drawingModifier' = (fmap mconcat . T.sequenceA) mods where
      mods = [ liftNodeModifier <$> nodeModifier' noPrefix
             , liftPoolModifier <$> poolModifier' noPrefix
             , liftNodeModifier <$> nodeModifier' prefix
             , liftPoolModifier <$> poolModifier' prefix
             ]
      noPrefix = []
      prefix = ["drawing"]



-- | Same as "bootstrapModifier", but for the multi client (using the
--   @multi@ prefix).
multiModifier' :: ReaderT C.Config IO (OptionModifier Ty.MultiConfig)
multiModifier' = (fmap mconcat . T.sequenceA) mods where
      mods = [ liftNodeModifier <$> nodeModifier' noPrefix
             , liftPoolModifier <$> poolModifier' noPrefix
             , liftNodeModifier <$> nodeModifier' prefix
             , liftPoolModifier <$> poolModifier' prefix
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
lookupC prefixes name = ask >>= \cfg -> liftIO (C.lookup cfg fullName) where
      fullName | null prefixes = name
               | otherwise = Text.intercalate "." prefixes <> "." <> name



serverPort' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
serverPort' prefixes = lookupC prefixes "serverPort"

serverPort :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
serverPort prefixes = toModifier <$> serverPort' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._serverPort = x })
      toModifier Nothing  = mempty



minNeighbours' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
minNeighbours' prefixes = lookupC prefixes "minNeighbours"

minNeighbours :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
minNeighbours prefixes = toModifier <$> minNeighbours' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._minNeighbours = x })
      toModifier Nothing  = mempty



maxNeighbours' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
maxNeighbours' prefixes = lookupC prefixes "maxNeighbours"

maxNeighbours :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
maxNeighbours prefixes = toModifier <$> maxNeighbours' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._maxNeighbours = x })
      toModifier Nothing  = mempty



verbosity' :: Prefixes -> ReaderT C.Config IO (Maybe Ty.Verbosity)
verbosity' prefixes = fmap (maybe Nothing parseVerbosity)
                           (lookupC prefixes "verbosity")

      where parseVerbosity :: String -> Maybe Ty.Verbosity
            parseVerbosity (map toLower -> x)
                  | x == "mute"    = Just Ty.Mute
                  | x == "quiet"   = Just Ty.Quiet
                  | x == "default" = Just Ty.Default
                  | x == "debug"   = Just Ty.Debug
                  | x == "chatty"  = Just Ty.Chatty
                  | otherwise      = Nothing



verbosity :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
verbosity prefixes = toModifier <$> verbosity' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._verbosity = x })
      toModifier Nothing  = mempty



maxChanSize' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
maxChanSize' prefixes = lookupC prefixes "maxChanSize"

maxChanSize :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
maxChanSize prefixes = toModifier <$> maxChanSize' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._maxChanSize = x })
      toModifier Nothing  = mempty



bounces' :: Prefixes -> ReaderT C.Config IO (Maybe Word)
bounces' prefixes = lookupC prefixes "bounces"

bounces :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
bounces prefixes = toModifier <$> bounces' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._bounces = x })
      toModifier Nothing  = mempty



acceptP' :: Prefixes -> ReaderT C.Config IO (Maybe Double)
acceptP' prefixes = lookupC prefixes "acceptP"

acceptP :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
acceptP prefixes = toModifier <$> acceptP' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._acceptP = x })
      toModifier Nothing  = mempty



maxSoftBounces' :: Prefixes -> ReaderT C.Config IO (Maybe Word)
maxSoftBounces' prefixes = lookupC prefixes "maxSoftBounces"

maxSoftBounces :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
maxSoftBounces prefixes = toModifier <$> maxSoftBounces' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._maxSoftBounces = x })
      toModifier Nothing  = mempty



shortTickRate' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
shortTickRate' prefixes = lookupC prefixes "shortTickRate"

shortTickRate :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
shortTickRate prefixes = toModifier <$> shortTickRate' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._shortTickRate = x })
      toModifier Nothing  = mempty



mediumTickRate' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
mediumTickRate' prefixes = lookupC prefixes "mediumTickRate"

mediumTickRate :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
mediumTickRate prefixes = toModifier <$> mediumTickRate' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._mediumTickRate = x })
      toModifier Nothing  = mempty



longTickRate' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
longTickRate' prefixes = lookupC prefixes "longTickRate"

longTickRate :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
longTickRate prefixes = toModifier <$> longTickRate' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._longTickRate = x })
      toModifier Nothing  = mempty



poolTimeout' :: Prefixes -> ReaderT C.Config IO (Maybe Double)
poolTimeout' prefixes = lookupC prefixes "poolTimeout"

poolTimeout :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
poolTimeout prefixes = toModifier <$> poolTimeout' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._poolTimeout = x })
      toModifier Nothing  = mempty



floodMessageCache' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
floodMessageCache' prefixes = lookupC prefixes "floodMessageCache"

floodMessageCache :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
floodMessageCache prefixes = toModifier <$> floodMessageCache' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._floodMessageCache = x })
      toModifier Nothing  = mempty


-- TODO
bootstrapServers' :: Prefixes -> ReaderT C.Config IO (Maybe a)
bootstrapServers' prefixes = undefined prefixes

bootstrapServers :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
bootstrapServers prefixes = undefined where
      _foo = toModifier <$> bootstrapServers' prefixes
      toModifier (Just x) = OptionModifier (\c -> c { Ty._bootstrapServers = x })
      toModifier Nothing  = mempty



poolSize' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
poolSize' prefixes = lookupC prefixes "poolSize"

poolSize :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.PoolConfig)
poolSize prefixes = toModifier <$> poolSize' prefixes where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._poolSize = x })
      toModifier Nothing  = mempty



restartEvery' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
restartEvery' prefixes = lookupC prefixes "restartEvery"

restartEvery :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.BootstrapConfig)
restartEvery _ = toModifier <$> restartEvery' ["bootstrap"] where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._restartEvery = x })
      toModifier Nothing  = mempty



restartMinimumPeriod' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
restartMinimumPeriod' prefixes = lookupC prefixes "restartMinimumPeriod"

restartMinimumPeriod :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.BootstrapConfig)
restartMinimumPeriod _ = toModifier <$> restartMinimumPeriod' ["bootstrap"] where
      toModifier (Just x) = OptionModifier (\c -> c { Ty._restartMinimumPeriod = x })
      toModifier Nothing  = mempty

