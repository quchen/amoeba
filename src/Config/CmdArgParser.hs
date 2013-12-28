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


-- | Parser for command line arguments. The idea is to use the parser to
--   generate option modifier functions that can then be applied to the default
--   options.
--
--   This module is intended to be used qualified, e.g. as \"CmdArgParser\" to
--   make nice names such as \"CmdArgParser.nodeArgs\".

module Config.CmdArgParser (
        nodeArgs
      , multiArgs
      , bootstrapArgs
      , drawingArgs
) where

import Options.Applicative
import Data.Word
import Data.Monoid
import Text.Printf
import qualified Data.Set as Set
import qualified Data.Traversable as T
import Data.Char (toLower)
import Text.Read (readEither)

import qualified Config.DefaultConfig as Default
import qualified Types as Ty
import Config.OptionModifier


runArgParser :: Parser a -- ^ Parser
             -> String   -- ^ Short help description
             -> String   -- ^ Long help description
             -> IO a
runArgParser parser short long = execParser parser'
      where parser' = info (helper <*> parser) infoMod
            infoMod = mconcat
                  [ fullDesc
                  , progDesc short
                  , header long
                  ]



nodeArgs :: IO (OptionModifier Ty.NodeConfig)
nodeArgs = runArgParser nodeModifier short long
      where short = "Amoeba client"
            long  = "Launch a single node in an Amoeba network"



multiArgs :: IO (OptionModifier Ty.MultiConfig)
multiArgs = runArgParser multiConfig short long
      where short = "Amoeba multi-node client"
            long  = "Launch multiple independent Amoeba nodes"



bootstrapArgs :: IO (OptionModifier Ty.BootstrapConfig)
bootstrapArgs = runArgParser bootstrapConfig short long
      where short = "Amoeba bootstrap server"
            long  = "Start a bootstrap server to allow new nodes to\
                          \ connect to an existing network"



drawingArgs :: IO (OptionModifier Ty.DrawingConfig)
drawingArgs = runArgParser drawingConfig short long
      where short = "Amoeba bootstrap server"
            long  = "Start a bootstrap server to allow new nodes to\
                          \ connect to an existing network"



-- #############################################################################
-- ###  Parsers  ###############################################################
-- #############################################################################



nodeModifier :: Parser (OptionModifier Ty.NodeConfig)
nodeModifier = (fmap mconcat . T.sequenceA) mods
      where mods = [ port
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
                   , pure mempty
                   , floodMessageCache
                   ]
                   -- TODO: specify bootstrap servers via command line



poolModifier :: Parser (OptionModifier Ty.PoolConfig)
poolModifier = (fmap mconcat . T.sequenceA) mods
      where mods = [ poolSize ]



bootstrapConfig :: Parser (OptionModifier Ty.BootstrapConfig)
bootstrapConfig = (fmap mconcat . T.sequenceA) mods
      where mods = [ restartEvery
                   , restartMinimumPeriod
                   , liftNodeModifier <$> nodeModifier
                   , liftPoolModifier <$> poolModifier
                   ]



multiConfig :: Parser (OptionModifier Ty.MultiConfig)
multiConfig = (fmap mconcat . T.sequenceA)  mods
      where mods = [ liftNodeModifier <$> nodeModifier
                   , liftPoolModifier <$> poolModifier
                   ]



drawingConfig :: Parser (OptionModifier Ty.DrawingConfig)
drawingConfig = (fmap mconcat . T.sequenceA) mods
      where mods = [ liftNodeModifier <$> nodeModifier
                   , liftPoolModifier <$> poolModifier
                   ]



-- #############################################################################
-- ###  Parser bits  ###########################################################
-- #############################################################################



port :: Parser (OptionModifier Ty.NodeConfig)
port = toModifier <$> (option . mconcat)
      [ long    "port"
      , short   'p'
      , metavar "PORT"
      , help    "Server port"
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._serverPort = x })



restartEvery :: Parser (OptionModifier Ty.BootstrapConfig)
restartEvery = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , long    "restart-every"
      , showDefault
      , value   defaultValue
      , metavar "(Int > 0)"
      , help    "Restart a random pool node every n new nodes. (Note that a\
                \ restart is one new node by itself already.)"
      , hidden
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._restartEvery = x })
            defaultValue = Ty._restartEvery Default.bootstrapConfig




restartMinimumPeriod :: Parser (OptionModifier Ty.BootstrapConfig)
restartMinimumPeriod = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , long    "restart-minperiod"
      , showDefault
      , value   defaultValue
      , metavar "[ms]"
      , help    "Restart a random pool node every n new nodes. (Note that a\
                \ restart is one new node by itself already.)"
      , hidden
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._restartMinimumPeriod = x })
            defaultValue = Ty._restartMinimumPeriod Default.bootstrapConfig



poolSize :: Parser (OptionModifier Ty.PoolConfig)
poolSize = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , long    "poolsize"
      , short   'n'
      , showDefault
      , value   defaultValue
      , metavar "(Int > 0)"
      , help    "Number of nodes in the pool"
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._poolSize = x })
            defaultValue = Ty._poolSize Default.poolConfig



minNeighbours :: Parser (OptionModifier Ty.NodeConfig)
minNeighbours = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   defaultValue
      , long    "maxn"
      , metavar "(Int > 0)"
      , help    "Minimum amount of neighbours (up-/downstream separate)"
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._minNeighbours = x })
            defaultValue = Ty._minNeighbours Default.nodeConfig



maxNeighbours :: Parser (OptionModifier Ty.NodeConfig)
maxNeighbours = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   defaultValue
      , long    "minn"
      , metavar "(Int > 0)"
      , help    "Maximum amount of neighbours (up-/downstream separate)"
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._maxNeighbours = x })
            defaultValue = Ty._maxNeighbours Default.nodeConfig


maxChanSize :: Parser (OptionModifier Ty.NodeConfig)
maxChanSize = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   defaultValue
      , long    "chansize"
      , metavar "(Int > 0)"
      , help    "Maximum communication channel size"
      , hidden
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._maxChanSize = x })
            defaultValue = Ty._maxChanSize Default.nodeConfig


floodMessageCache :: Parser (OptionModifier Ty.NodeConfig)
floodMessageCache = toModifier <$> (nullOption . mconcat)
      [ reader nonnegative
      , showDefault
      , value   defaultValue
      , long    "floodcache"
      , metavar "(Int >= 0)"
      , help    "Number of past flood messages to cache"
      , hidden
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._floodMessageCache = x })
            defaultValue = Ty._floodMessageCache Default.nodeConfig


bounces :: Parser (OptionModifier Ty.NodeConfig)
bounces = toModifier <$> (nullOption . mconcat)
      [ reader nonnegative
      , showDefault
      , value   defaultValue
      , long    "bounces"
      , metavar "(Int >= 0)"
      , help    "Minimum edge search hard bounces"
      , hidden
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._bounces = x })
            defaultValue = Ty._bounces Default.nodeConfig


maxSoftBounces :: Parser (OptionModifier Ty.NodeConfig)
maxSoftBounces = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   defaultValue
      , long    "hbounce"
      , metavar "(Int > 0)"
      , help    "Maximum edge search soft bounces"
      , hidden
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._maxSoftBounces = x })
            defaultValue = Ty._maxSoftBounces Default.nodeConfig


acceptP :: Parser (OptionModifier Ty.NodeConfig)
acceptP = toModifier <$> (nullOption . mconcat)
      [ reader probability
      , showDefault
      , value   defaultValue
      , long    "acceptp"
      , metavar "(0 < p <= 1)"
      , help    "Edge request soft bounce acceptance probability"
      , hidden
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._acceptP = x })
            defaultValue = Ty._acceptP Default.nodeConfig


shortTickRate :: Parser (OptionModifier Ty.NodeConfig)
shortTickRate = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   defaultValue
      , long    "stick"
      , metavar "[ms]"
      , help    "Tick rate of short loops"
      , hidden
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._shortTickRate = x })
            defaultValue = Ty._shortTickRate Default.nodeConfig


mediumTickRate :: Parser (OptionModifier Ty.NodeConfig)
mediumTickRate = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   defaultValue
      , long    "mtick"
      , metavar "[ms]"
      , help    "Tick rate of medium loops"
      , hidden
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._mediumTickRate = x })
            defaultValue = Ty._mediumTickRate Default.nodeConfig


longTickRate :: Parser (OptionModifier Ty.NodeConfig)
longTickRate = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   defaultValue
      , long    "ltick"
      , metavar "[ms]"
      , help    "Tick rate of long loops"
      , hidden
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._longTickRate = x })
            defaultValue = Ty._longTickRate Default.nodeConfig


poolTimeout :: Parser (OptionModifier Ty.NodeConfig)
poolTimeout = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   defaultValue
      , long    "timeout"
      , metavar "[s]"
      , help    "Timeout for removal of nodes from the USN/DSN pool"
      , hidden
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._poolTimeout = x })
            defaultValue = Ty._poolTimeout Default.nodeConfig


verbosity :: Parser (OptionModifier Ty.NodeConfig)
verbosity = toModifier <$> (nullOption . mconcat)
      [ reader readVerbosity
      , value   defaultValue
      , long    "verbosity"
      , metavar "(mute|quiet|default|debug|chatty)"
      , help    "Verbosity level, increasing from left to right"
      ]
      where toModifier x = OptionModifier (\c -> c { Ty._verbosity = x })
            defaultValue = Ty._verbosity Default.nodeConfig



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