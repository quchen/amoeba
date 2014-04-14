-- | Read configuration settings from a configuration file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK show-extensions #-}

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
import Control.Monad.Reader
import qualified Data.Set as Set

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as Text
import qualified Data.Traversable as T

import Control.Lens hiding (to)
import qualified Types.Lens as L

import qualified Types as Ty
import Config.OptionModifier
import qualified Config.AddressParser as AP


-- | Files to read the config from. The later in the list, the higher the
--   precedence of the contained settings.
configFiles :: [C.Worth FilePath]
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
                  , hardBounces
                  , acceptP
                  , maxSoftBounces
                  , shortTickRate
                  , mediumTickRate
                  , longTickRate
                  , poolTimeout
                  , verbosity
                  , bootstrapServers
                  , floodMessageCache
                  ]



-- | Get the pool modifier given a certain "Prefix".
poolModifier' :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.PoolConfig)
poolModifier' prefixes = (fmap mconcat . T.sequenceA) mods where
      mods = map ($ prefixes) [ poolSize ]



noPrefix :: [a]
noPrefix = []



-- | Read the general config, before overwriting it with values with the
--   @bootstrap@ prefix, i.e. for the @foo@ setting first looks for @foo@ and
--   then for @bootstrap.foo@.
bootstrapModifier' :: ReaderT C.Config IO (OptionModifier Ty.BootstrapConfig)
bootstrapModifier' = (fmap mconcat . T.sequenceA) mods where
      mods = [ liftModifier L.nodeConfig <$> nodeModifier' noPrefix
             , liftModifier L.poolConfig <$> poolModifier' noPrefix
             , restartEvery prefix
             , restartMinimumPeriod prefix
             , liftModifier L.nodeConfig <$> nodeModifier' prefix
             , liftModifier L.poolConfig <$> poolModifier' prefix
             ]
      prefix = ["bootstrap"]



-- | Same as "bootstrapModifier", but for the drawing server (using the
--   @drawing@ prefix).
drawingModifier' :: ReaderT C.Config IO (OptionModifier Ty.DrawingConfig)
drawingModifier' = (fmap mconcat . T.sequenceA) mods where
      mods = [ liftModifier L.nodeConfig <$> nodeModifier' noPrefix
             , liftModifier L.poolConfig <$> poolModifier' noPrefix
             , drawEvery prefix
             , drawFilename prefix
             , drawTimeout prefix
             , liftModifier L.nodeConfig <$> nodeModifier' prefix
             , liftModifier L.poolConfig <$> poolModifier' prefix
             ]
      prefix = ["drawing"]



-- | Same as "bootstrapModifier", but for the multi client (using the
--   @multi@ prefix).
multiModifier' :: ReaderT C.Config IO (OptionModifier Ty.MultiConfig)
multiModifier' = (fmap mconcat . T.sequenceA) mods where
      mods = [ liftModifier L.nodeConfig <$> nodeModifier' noPrefix
             , liftModifier L.poolConfig <$> poolModifier' noPrefix
             , liftModifier L.nodeConfig <$> nodeModifier' prefix
             , liftModifier L.poolConfig <$> poolModifier' prefix
             ]
      prefix = ["multi"]



-- | A prefix is what leads to an option in a hierarchy; they're similar to
--   parent directories. For example the expression "foo.bar.qux" corresponds to
--   @[\"foo\", \"bar\"] :: Prefixes@, with the option name being @qux@.
type Prefixes = [C.Name]



-- | Look up a certain setting, possibly with a list of parent prefixes.
lookupC :: C.Configured a
        => Prefixes -- ^ List of prefixes, e.g. ["foo", "bar"] for foo.bar.*
        -> C.Name   -- ^ Option name
        -> ReaderT C.Config IO (Maybe a)
lookupC prefixes name = ask >>= \cfg -> liftIO (C.lookup cfg fullName) where
      fullName | null prefixes = name
               | otherwise = Text.intercalate "." prefixes <> "." <> name



-- | Convert a field and a value to an 'OptionModifier' to set that field to
--   that value. 'mempty' if no value is given.
toSetter :: ASetter a a c b  -- ^ Lens to a field
         -> Maybe b          -- ^ New value of the field
         -> OptionModifier a -- ^ 'OptionModifier' to apply the lens
toSetter l (Just x) = OptionModifier (l .~ x)
toSetter _ Nothing  = mempty





serverPort' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
serverPort' prefixes = lookupC prefixes "serverPort"

serverPort :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
serverPort prefixes = toSetter L.serverPort <$> serverPort' prefixes where



minNeighbours' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
minNeighbours' prefixes = lookupC prefixes "minNeighbours"

minNeighbours :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
minNeighbours prefixes = toSetter L.minNeighbours <$> minNeighbours' prefixes where



maxNeighbours' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
maxNeighbours' prefixes = lookupC prefixes "maxNeighbours"

maxNeighbours :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
maxNeighbours prefixes = toSetter L.maxNeighbours <$> maxNeighbours' prefixes where



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
verbosity prefixes = toSetter L.verbosity <$> verbosity' prefixes where



maxChanSize' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
maxChanSize' prefixes = lookupC prefixes "maxChanSize"

maxChanSize :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
maxChanSize prefixes = toSetter L.maxChanSize <$> maxChanSize' prefixes where



hardBounces' :: Prefixes -> ReaderT C.Config IO (Maybe Word)
hardBounces' prefixes = lookupC prefixes "hardBounces"

hardBounces :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
hardBounces prefixes = toSetter L.hardBounces <$> hardBounces' prefixes where



acceptP' :: Prefixes -> ReaderT C.Config IO (Maybe Double)
acceptP' prefixes = lookupC prefixes "acceptP"

acceptP :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
acceptP prefixes = toSetter L.acceptP <$> acceptP' prefixes where



maxSoftBounces' :: Prefixes -> ReaderT C.Config IO (Maybe Word)
maxSoftBounces' prefixes = lookupC prefixes "maxSoftBounces"

maxSoftBounces :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
maxSoftBounces prefixes = toSetter L.maxSoftBounces <$> maxSoftBounces' prefixes where



shortTickRate' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
shortTickRate' prefixes = lookupC prefixes "shortTickRate"

shortTickRate :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
shortTickRate prefixes = toSetter L.shortTickRate <$> shortTickRate' prefixes where



mediumTickRate' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
mediumTickRate' prefixes = lookupC prefixes "mediumTickRate"

mediumTickRate :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
mediumTickRate prefixes = toSetter L.mediumTickRate <$> mediumTickRate' prefixes where



longTickRate' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
longTickRate' prefixes = lookupC prefixes "longTickRate"

longTickRate :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
longTickRate prefixes = toSetter L.longTickRate <$> longTickRate' prefixes where



poolTimeout' :: Prefixes -> ReaderT C.Config IO (Maybe Double)
poolTimeout' prefixes = lookupC prefixes "poolTimeout"

poolTimeout :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
poolTimeout prefixes = toSetter L.poolTimeout <$> poolTimeout' prefixes where



floodMessageCache' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
floodMessageCache' prefixes = lookupC prefixes "floodMessageCache"

floodMessageCache :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
floodMessageCache prefixes = toSetter L.floodMessageCache <$> floodMessageCache' prefixes where



bootstrapServers' :: Prefixes -> ReaderT C.Config IO (Set.Set Ty.To)
bootstrapServers' prefixes = fmap m'valueToTo
                                  (lookupC prefixes "bootstrapServers")

      where

      m'valueToTo :: Maybe C.Value -> Set.Set Ty.To
      m'valueToTo = maybe Set.empty valueToTo

      -- Convert a "C.Value" to a "Set.Set" of "Ty.To" addresses.
      valueToTo :: C.Value -> Set.Set Ty.To
      valueToTo (C.String text) =
            either (const Set.empty)
                   Set.singleton
                   (parseAddrText text)
      valueToTo (C.List vals) = foldr go Set.empty vals where
            go (C.String text) xs = case parseAddrText text of
                  Right to -> Set.insert to xs
                  Left  _r -> xs
            go _else xs = xs
      valueToTo _else = Set.empty
      parseAddrText = AP.parseAddress . Text.unpack

bootstrapServers :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.NodeConfig)
bootstrapServers prefixes = appendBSS  <$> bootstrapServers' prefixes where
      appendBSS x = OptionModifier (L.bootstrapServers <>~ x)



poolSize' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
poolSize' prefixes = lookupC prefixes "poolSize"

poolSize :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.PoolConfig)
poolSize prefixes = toSetter L.poolSize <$> poolSize' prefixes where



restartEvery' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
restartEvery' prefixes = lookupC prefixes "restartEvery"

restartEvery :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.BootstrapConfig)
restartEvery _ = toSetter L.restartEvery <$> restartEvery' ["bootstrap"] where



restartMinimumPeriod' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
restartMinimumPeriod' prefixes = lookupC prefixes "restartMinimumPeriod"

restartMinimumPeriod :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.BootstrapConfig)
restartMinimumPeriod _ = toSetter L.restartMinimumPeriod <$> restartMinimumPeriod' ["bootstrap"] where



drawEvery' :: Prefixes -> ReaderT C.Config IO (Maybe Int)
drawEvery' prefixes = lookupC prefixes "drawEvery"

drawEvery :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.DrawingConfig)
drawEvery _ = toSetter L.drawEvery <$> drawEvery' ["drawing"] where



drawFilename' :: Prefixes -> ReaderT C.Config IO (Maybe FilePath)
drawFilename' prefixes = lookupC prefixes "drawFilename"

drawFilename :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.DrawingConfig)
drawFilename _ = toSetter L.drawFilename <$> drawFilename' ["drawing"] where



drawTimeout' :: Prefixes -> ReaderT C.Config IO (Maybe Double)
drawTimeout' prefixes = (fmap . fmap) (fromIntegral :: Int -> Double)
                                      (lookupC prefixes "drawTimeout")

drawTimeout :: Prefixes -> ReaderT C.Config IO (OptionModifier Ty.DrawingConfig)
drawTimeout _ = toSetter L.drawTimeout <$> drawTimeout' ["drawing"] where
