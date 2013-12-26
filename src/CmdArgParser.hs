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

module CmdArgParser (
        parseNodeArgs
      , parseMultiArgs
      , parseBootstrapArgs
      , parseDrawingArgs
) where

import Options.Applicative
import Data.Word
import Data.Monoid
import Text.Printf
import qualified Data.Set as Set
import qualified Data.Traversable as T
import Data.Char (toLower)
import Text.Read (readEither)

import qualified DefaultConfig as Default
import qualified Types as Ty


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



parseNodeArgs :: IO (Ty.OptionModifier Ty.NodeConfig)
parseNodeArgs = runArgParser nodeConfig short long
      where short = "Amoeba client"
            long  = "Launch a single node in an Amoeba network"



parseMultiArgs :: IO (Ty.OptionModifier Ty.MultiConfig)
parseMultiArgs = runArgParser multiConfig short long
      where short = "Amoeba multi-node client"
            long  = "Launch multiple independent Amoeba nodes"



parseBootstrapArgs :: IO (Ty.OptionModifier Ty.BootstrapConfig)
parseBootstrapArgs = runArgParser bootstrapConfig short long
      where short = "Amoeba bootstrap server"
            long  = "Start a bootstrap server to allow new nodes to\
                          \ connect to an existing network"



parseDrawingArgs :: IO (Ty.OptionModifier Ty.DrawingConfig)
parseDrawingArgs = runArgParser drawingConfig short long
      where short = "Amoeba bootstrap server"
            long  = "Start a bootstrap server to allow new nodes to\
                          \ connect to an existing network"



-- #############################################################################
-- ###  Parsers  ###############################################################
-- #############################################################################



nodeConfig :: Parser (Ty.OptionModifier Ty.NodeConfig)
nodeConfig = mconcat <$> T.sequenceA [ port
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



poolConfig :: Parser (Ty.OptionModifier Ty.PoolConfig)
poolConfig = mconcat <$> T.sequenceA [ poolSize ]



bootstrapConfig :: Parser (Ty.OptionModifier Ty.BootstrapConfig)
bootstrapConfig = mconcat <$> T.sequenceA [ restartEvery
                                          , restartMinimumPeriod
                                          , liftNodeConfig <$> nodeConfig
                                          , liftPoolConfig <$> poolConfig
                                          ]
      where liftNodeConfig (Ty.OptionModifier x) = Ty.OptionModifier ( \c -> c { Ty._bootstrapNodeConfig = x (Ty._bootstrapNodeConfig c) } )
            liftPoolConfig (Ty.OptionModifier x) = Ty.OptionModifier ( \c -> c { Ty._bootstrapPoolConfig = x (Ty._bootstrapPoolConfig c) } )



multiConfig :: Parser (Ty.OptionModifier Ty.MultiConfig)
multiConfig = mconcat <$> T.sequenceA [ liftNodeConfig <$> nodeConfig
                                      , liftPoolConfig <$> poolConfig
                                      ]
      where liftNodeConfig (Ty.OptionModifier x) = Ty.OptionModifier ( \c -> c { Ty._multiNodeConfig = x (Ty._multiNodeConfig c) } )
            liftPoolConfig (Ty.OptionModifier x) = Ty.OptionModifier ( \c -> c { Ty._multiPoolConfig = x (Ty._multiPoolConfig c) } )



drawingConfig :: Parser (Ty.OptionModifier Ty.DrawingConfig)
drawingConfig = mconcat <$> T.sequenceA [ liftNodeConfig <$> nodeConfig
                                        , liftPoolConfig <$> poolConfig
                                        ]
      where liftNodeConfig (Ty.OptionModifier x) = Ty.OptionModifier ( \c -> c { Ty._drawingNodeConfig = x (Ty._drawingNodeConfig c) } )
            liftPoolConfig (Ty.OptionModifier x) = Ty.OptionModifier ( \c -> c { Ty._drawingPoolConfig = x (Ty._drawingPoolConfig c) } )



-- #############################################################################
-- ###  Parser bits  ###########################################################
-- #############################################################################



port :: Parser (Ty.OptionModifier Ty.NodeConfig)
port = toModifier <$> (option . mconcat)
      [ long    "port"
      , short   'p'
      , metavar "PORT"
      , help    "Server port"
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._serverPort = x })



restartEvery :: Parser (Ty.OptionModifier Ty.BootstrapConfig)
restartEvery = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , long    "restart-every"
      , showDefault
      , value   (Ty._restartEvery Default.bootstrapConfig)
      , metavar "(Int > 0)"
      , help    "Restart a random pool node every n new nodes. (Note that a restart is one new node by itself already.)"
      , hidden
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._restartEvery = x })




restartMinimumPeriod :: Parser (Ty.OptionModifier Ty.BootstrapConfig)
restartMinimumPeriod = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , long    "restart-minperiod"
      , showDefault
      , value   (Ty._restartMinimumPeriod Default.bootstrapConfig)
      , metavar "[ms]"
      , help    "Restart a random pool node every n new nodes. (Note that a restart is one new node by itself already.)"
      , hidden
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._restartMinimumPeriod = x })



poolSize :: Parser (Ty.OptionModifier Ty.PoolConfig)
poolSize = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , long    "poolsize"
      , short   'n'
      , showDefault
      , value   (Ty._poolSize Default.poolConfig)
      , metavar "(Int > 0)"
      , help    "Number of nodes in the pool"
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._poolSize = x })



minNeighbours :: Parser (Ty.OptionModifier Ty.NodeConfig)
minNeighbours = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (Ty._minNeighbours Default.nodeConfig)
      , long    "maxn"
      , metavar "(Int > 0)"
      , help    "Minimum amount of neighbours (up-/downstream separate)"
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._minNeighbours = x })



maxNeighbours :: Parser (Ty.OptionModifier Ty.NodeConfig)
maxNeighbours = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (Ty._maxNeighbours Default.nodeConfig)
      , long    "minn"
      , metavar "(Int > 0)"
      , help    "Maximum amount of neighbours (up-/downstream separate)"
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._maxNeighbours = x })




maxChanSize :: Parser (Ty.OptionModifier Ty.NodeConfig)
maxChanSize = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (Ty._maxChanSize Default.nodeConfig)
      , long    "chansize"
      , metavar "(Int > 0)"
      , help    "Maximum communication channel size"
      , hidden
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._maxChanSize = x })



floodMessageCache :: Parser (Ty.OptionModifier Ty.NodeConfig)
floodMessageCache = toModifier <$> (nullOption . mconcat)
      [ reader nonnegative
      , showDefault
      , value   (Ty._floodMessageCache Default.nodeConfig)
      , long    "floodcache"
      , metavar "(Int >= 0)"
      , help    "Number of past flood messages to cache"
      , hidden
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._floodMessageCache = x })



bounces :: Parser (Ty.OptionModifier Ty.NodeConfig)
bounces = toModifier <$> (nullOption . mconcat)
      [ reader nonnegative
      , showDefault
      , value   (Ty._bounces Default.nodeConfig)
      , long    "bounces"
      , metavar "(Int >= 0)"
      , help    "Minimum edge search hard bounces"
      , hidden
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._bounces = x })



maxSoftBounces :: Parser (Ty.OptionModifier Ty.NodeConfig)
maxSoftBounces = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (Ty._maxSoftBounces Default.nodeConfig)
      , long    "hbounce"
      , metavar "(Int > 0)"
      , help    "Maximum edge search soft bounces"
      , hidden
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._maxSoftBounces = x })



acceptP :: Parser (Ty.OptionModifier Ty.NodeConfig)
acceptP = toModifier <$> (nullOption . mconcat)
      [ reader probability
      , showDefault
      , value   (Ty._acceptP Default.nodeConfig)
      , long    "acceptp"
      , metavar "(0 < p <= 1)"
      , help    "Edge request soft bounce acceptance probability"
      , hidden
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._acceptP = x })



shortTickRate :: Parser (Ty.OptionModifier Ty.NodeConfig)
shortTickRate = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (Ty._shortTickRate Default.nodeConfig)
      , long    "stick"
      , metavar "[ms]"
      , help    "Tick rate of short loops"
      , hidden
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._shortTickRate = x })



mediumTickRate :: Parser (Ty.OptionModifier Ty.NodeConfig)
mediumTickRate = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (Ty._mediumTickRate Default.nodeConfig)
      , long    "mtick"
      , metavar "[ms]"
      , help    "Tick rate of medium loops"
      , hidden
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._mediumTickRate = x })



longTickRate :: Parser (Ty.OptionModifier Ty.NodeConfig)
longTickRate = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (Ty._longTickRate Default.nodeConfig)
      , long    "ltick"
      , metavar "[ms]"
      , help    "Tick rate of long loops"
      , hidden
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._longTickRate = x })



poolTimeout :: Parser (Ty.OptionModifier Ty.NodeConfig)
poolTimeout = toModifier <$> (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (Ty._poolTimeout Default.nodeConfig)
      , long    "timeout"
      , metavar "[s]"
      , help    "Timeout for removal of nodes from the USN/DSN pool"
      , hidden
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._poolTimeout = x })



verbosity :: Parser (Ty.OptionModifier Ty.NodeConfig)
verbosity = toModifier <$> (nullOption . mconcat)
      [ reader readVerbosity
      , value   (Ty._verbosity Default.nodeConfig)
      , long    "verbosity"
      , metavar "(mute|quiet|default|debug|chatty)"
      , help    "Verbosity level, increasing from left to right"
      ]
      where toModifier x = Ty.OptionModifier (\c -> c { Ty._verbosity = x })





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