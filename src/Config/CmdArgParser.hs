-- | Parser for command line arguments. The idea is to use the parser to
--   generate option modifier functions that can then be applied to the default
--   options.
--
--   This module is intended to be used qualified, e.g. as \"CmdArgParser\" to
--   make nice names such as \"CmdArgParser.'nodeModifier'\".

module Config.CmdArgParser (
        nodeModifier
      , multiModifier
      , bootstrapModifier
      , drawingModifier
) where

import Options.Applicative
import Data.Monoid
import Text.Printf
import qualified Data.Set as Set
import qualified Data.Traversable as T
import Data.Char (toLower)
import Text.Read (readEither)

import Control.Lens

import qualified Types as Ty
import qualified Types.Lens as L
import Config.OptionModifier
import qualified Config.AddressParser as AddressParser



runArgParser :: Parser a -- ^ Parser
             -> String   -- ^ Short help description
             -> String   -- ^ Long help description
             -> IO a
runArgParser parser shortDescr longDescr = execParser parser' where
      parser' = info (helper <*> parser) infoMod
      infoMod = mconcat
            [ fullDesc
            , progDesc shortDescr
            , header longDescr
            ]



nodeModifier :: IO (OptionModifier Ty.NodeConfig)
nodeModifier = runArgParser nodeModifier' s l where
      s = "Amoeba client"
      l = "Launch a single node in an Amoeba network"



multiModifier :: IO (OptionModifier Ty.MultiConfig)
multiModifier = runArgParser multiModifier' s l where
      s = "Amoeba multi-node client"
      l = "Launch multiple independent Amoeba nodes"



bootstrapModifier :: IO (OptionModifier Ty.BootstrapConfig)
bootstrapModifier = runArgParser bootstrapModifier' s l where
      s = "Amoeba bootstrap server"
      l = "Start a bootstrap server to allow new nodes to\
                \ connect to an existing network"



drawingModifier :: IO (OptionModifier Ty.DrawingConfig)
drawingModifier = runArgParser drawingModifier' s l where
      s = "Amoeba bootstrap server"
      l = "Start a bootstrap server to allow new nodes to connect to an\
                \ existing network"



-- #############################################################################
-- ###  Parsers  ###############################################################
-- #############################################################################



nodeModifier' :: Parser (OptionModifier Ty.NodeConfig)
nodeModifier' = (fmap mconcat . T.sequenceA) mods where
      mods = [ port
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
             , bootstrapServer
             , floodCacheSize
             ]



poolModifier' :: Parser (OptionModifier Ty.PoolConfig)
poolModifier' = (fmap mconcat . T.sequenceA) mods where
      mods = [ poolSize ]



bootstrapModifier' :: Parser (OptionModifier Ty.BootstrapConfig)
bootstrapModifier' = (fmap mconcat . T.sequenceA) mods where
      mods = [ restartEvery
             , restartMinimumPeriod
             , liftModifier L.nodeConfig <$> nodeModifier'
             , liftModifier L.poolConfig <$> poolModifier'
             ]



multiModifier' :: Parser (OptionModifier Ty.MultiConfig)
multiModifier' = (fmap mconcat . T.sequenceA)  mods where
      mods = [ liftModifier L.nodeConfig <$> nodeModifier'
             , liftModifier L.poolConfig <$> poolModifier'
             ]



drawingModifier' :: Parser (OptionModifier Ty.DrawingConfig)
drawingModifier' = (fmap mconcat . T.sequenceA) mods where
      mods = [ liftModifier L.nodeConfig <$> nodeModifier'
             , liftModifier L.poolConfig <$> poolModifier'
             , drawingInterval
             , drawingFilename
             , drawingTimeout
             ]



-- #############################################################################
-- ###  Parser bits  ###########################################################
-- #############################################################################



defaultValue :: Monoid m => Parser m
defaultValue = pure mempty



-- | Convert a field and a value to an 'OptionModifier' to set that field to
--   that value.
toSetter :: ASetter a a c b  -- ^ Lens to a field
         -> b                -- ^ New value of the field
         -> OptionModifier a -- ^ 'OptionModifier' to apply the lens
toSetter l x = OptionModifier (l .~ x)



port :: Parser (OptionModifier Ty.NodeConfig)
port = v <|> defaultValue where
      v = toSetter L.serverPort <$> (option . mconcat)
            [ long    "port"
            , short   'p'
            , metavar "PORT"
            , help    "Server port"
            ]



restartEvery :: Parser (OptionModifier Ty.BootstrapConfig)
restartEvery = v <|> defaultValue where
      v = toSetter L.restartEvery <$> (nullOption . mconcat)
            [ reader  positive
            , long    "restart-every"
            , metavar "(Int > 0)"
            , help    "Restart a random pool node every n new nodes."
            , hidden
            ]




drawingInterval :: Parser (OptionModifier Ty.DrawingConfig)
drawingInterval = v <|> defaultValue where
      v = toSetter L.drawEvery <$> (nullOption . mconcat)
            [ reader  positive
            , long    "draw-every"
            , metavar "[ms]"
            , help    "Tickrate for drawing the current network to file\
                      \ and sending out neighbour information requests."
            , hidden
            ]



drawingTimeout :: Parser (OptionModifier Ty.DrawingConfig)
drawingTimeout = v <|> defaultValue where
      v = toSetter L.drawTimeout . fromIntegral <$> (nullOption . mconcat)
            [ reader  positive
            , long    "draw-timeout"
            , metavar "[s]"
            , help    "Timeout for removing nodes that haven't sent data to the\
                      \ drawing server"
            , hidden
            ]



drawingFilename :: Parser (OptionModifier Ty.DrawingConfig)
drawingFilename = v <|> defaultValue where
      v = toSetter L.drawFilename <$> (strOption . mconcat)
            [ long    "drawing-filename"
            , metavar "(filename)"
            , help    "File to write the network data to"
            ]



restartMinimumPeriod :: Parser (OptionModifier Ty.BootstrapConfig)
restartMinimumPeriod = v <|> defaultValue where
      v = toSetter L.restartMinimumPeriod <$> (nullOption . mconcat)
            [ reader  positive
            , long    "restart-minperiod"
            , metavar "[ms]"
            , help    "Restart a random pool node every n new nodes.\
                      \ (Note that a restart is one new node by itself\
                      \ already.)"
            , hidden
            ]



poolSize :: Parser (OptionModifier Ty.PoolConfig)
poolSize = v <|> defaultValue where
      v = toSetter L.poolSize <$> (nullOption . mconcat)
            [ reader  positive
            , long    "poolsize"
            , short   'n'
            , metavar "(Int > 0)"
            , help    "Number of nodes in the pool"
            ]



minNeighbours :: Parser (OptionModifier Ty.NodeConfig)
minNeighbours = v <|> defaultValue where
      v = toSetter L.minNeighbours <$> (nullOption . mconcat)
            [ reader  positive
            , long    "minn"
            , metavar "(Int > 0)"
            , help    "Minimum amount of neighbours (up-/downstream\
                      \ separate)"
            ]



maxNeighbours :: Parser (OptionModifier Ty.NodeConfig)
maxNeighbours = v <|> defaultValue where
      v = toSetter L.maxNeighbours <$> (nullOption . mconcat)
            [ reader  positive
            , long    "maxn"
            , metavar "(Int > 0)"
            , help    "Maximum amount of neighbours (up-/downstream\
                      \ separate)"
            ]


maxChanSize :: Parser (OptionModifier Ty.NodeConfig)
maxChanSize = v <|> defaultValue where
      v = toSetter L.maxChanSize <$> (nullOption . mconcat)
            [ reader  positive
            , long    "chansize"
            , metavar "(Int > 0)"
            , help    "Maximum communication channel size"
            , hidden
            ]


floodCacheSize :: Parser (OptionModifier Ty.NodeConfig)
floodCacheSize = v <|> defaultValue where
      v = toSetter L.floodMessageCache <$> (nullOption . mconcat)
            [ reader  nonnegative
            , long    "floodcache"
            , metavar "(Int >= 0)"
            , help    "Number of past flood messages to cache"
            , hidden
            ]


bounces :: Parser (OptionModifier Ty.NodeConfig)
bounces = v <|> defaultValue where
      v = toSetter L.hardBounces <$> (nullOption . mconcat)
            [ reader  nonnegative
            , long    "hardbounces"
            , metavar "(Int >= 0)"
            , help    "Minimum edge search hard bounces"
            , hidden
            ]


maxSoftBounces :: Parser (OptionModifier Ty.NodeConfig)
maxSoftBounces = v <|> defaultValue where
      v = toSetter L.maxSoftBounces <$> (nullOption . mconcat)
            [ reader  positive
            , long    "hbounce"
            , metavar "(Int > 0)"
            , help    "Maximum edge search soft bounces"
            , hidden
            ]


acceptP :: Parser (OptionModifier Ty.NodeConfig)
acceptP = v <|> defaultValue where
      v = toSetter L.acceptP <$> (nullOption . mconcat)
            [ reader  probability
            , long    "acceptp"
            , metavar "(0 < p <= 1)"
            , help    "Edge request soft bounce acceptance probability"
            , hidden
            ]


shortTickRate :: Parser (OptionModifier Ty.NodeConfig)
shortTickRate = v <|> defaultValue where
      v = toSetter L.shortTickRate <$> (nullOption . mconcat)
            [ reader  positive
            , long    "stick"
            , metavar "[ms]"
            , help    "Tick rate of short loops"
            , hidden
            ]


mediumTickRate :: Parser (OptionModifier Ty.NodeConfig)
mediumTickRate = v <|> defaultValue where
      v = toSetter L.mediumTickRate <$> (nullOption . mconcat)
            [ reader  positive
            , long    "mtick"
            , metavar "[ms]"
            , help    "Tick rate of medium loops"
            , hidden
            ]


longTickRate :: Parser (OptionModifier Ty.NodeConfig)
longTickRate = v <|> defaultValue where
      v = toSetter L.longTickRate <$> (nullOption . mconcat)
            [ reader  positive
            , long    "ltick"
            , metavar "[ms]"
            , help    "Tick rate of long loops"
            , hidden
            ]


poolTimeout :: Parser (OptionModifier Ty.NodeConfig)
poolTimeout = v <|> defaultValue where
      v = toSetter L.poolTimeout <$> (nullOption . mconcat)
            [ reader  positive
            , long    "timeout"
            , metavar "[s]"
            , help    "Timeout for removal of nodes from the USN/DSN pool"
            , hidden
            ]


verbosity :: Parser (OptionModifier Ty.NodeConfig)
verbosity = v <|> defaultValue where
      v = toSetter L.verbosity <$> (nullOption . mconcat)
            [ reader  readVerbosity
            , long    "verbosity"
            , metavar "(mute|quiet|default|debug|chatty)"
            , help    "Verbosity level, increasing from left to right"
            ]



-- | Append arg parsed BSS address to config
bootstrapServer :: Parser (OptionModifier Ty.NodeConfig)
bootstrapServer = v <|> defaultValue where
      v = appendBSS <$> (nullOption . mconcat)
            [ reader  readAddress
            , long    "bootstrap"
            , metavar "(hostname)"
            , help    "Bootstrap server address"
            ]
      appendBSS x = OptionModifier (L.bootstrapServers <>~ x)





-- #############################################################################
-- ###  Readers  ###############################################################
-- #############################################################################



-- | Numerical value between 0 and 1 (inclusive)
probability :: (Num a, Ord a, Read a) => String -> ReadM a
probability x = case readEither x of
      Right y | y >= 0 && y <= 1 -> pure y
      Right _ -> readerError (printf "Bad probability %s; 0 <= p <= 1" x)
      Left  _ -> readerError (printf "Parse error on double %s" x)



-- | Strictly positive numerical value
positive :: (Num a, Ord a, Read a) => String -> ReadM a
positive x = case readEither x of
      Right y | y > 0 -> pure y
      Right _ -> readerError (printf "Positive number expected (%s given)" x)
      Left  _ -> readerError (printf "Parse error on integer %s" x)



-- | Non-negative numerical value
nonnegative :: (Num a, Ord a, Read a) => String -> ReadM a
nonnegative x = case readEither x of
      Right y | y >= 0 -> pure y
      Right _ -> readerError (printf "Nonnegative number expected (%s given)" x)
      Left  _ -> readerError (printf "Parse error on integer %s" x)



readVerbosity :: String -> ReadM Ty.Verbosity
readVerbosity x = case map toLower x of
      "mute"    -> pure Ty.Mute
      "quiet"   -> pure Ty.Quiet
      "default" -> pure Ty.Default
      "debug"   -> pure Ty.Debug
      "chatty"  -> pure Ty.Chatty
      _else     -> readerError (printf "Unrecognized verbosity level \"%d\"" x)



readAddress :: String -> ReadM (Set.Set Ty.To)
readAddress s = case AddressParser.parseAddress s of
      Left e     -> readerError ("Bad address: " ++ show e)
      Right addr -> pure (Set.singleton addr)
