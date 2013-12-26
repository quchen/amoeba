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
import Data.Char (toLower)
import Text.Read (readEither)

import qualified DefaultConfig as Default
import qualified Types as T


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



parseNodeArgs :: IO T.NodeConfig
parseNodeArgs = runArgParser nodeConfig short long
      where short = "Amoeba client"
            long  = "Launch a single node in an Amoeba network"



parseMultiArgs :: IO T.MultiConfig
parseMultiArgs = runArgParser multiConfig short long
      where short = "Amoeba multi-node client"
            long  = "Launch multiple independent Amoeba nodes"



parseBootstrapArgs :: IO T.BootstrapConfig
parseBootstrapArgs = runArgParser bootstrapConfig short long
      where short = "Amoeba bootstrap server"
            long  = "Start a bootstrap server to allow new nodes to\
                          \ connect to an existing network"



parseDrawingArgs :: IO T.DrawingConfig
parseDrawingArgs = runArgParser drawingConfig short long
      where short = "Amoeba bootstrap server"
            long  = "Start a bootstrap server to allow new nodes to\
                          \ connect to an existing network"



-- #############################################################################
-- ###  Parsers  ###############################################################
-- #############################################################################



nodeConfig :: Parser T.NodeConfig
nodeConfig = T.NodeConfig
     <$> port
     <*> minNeighbours
     <*> maxNeighbours
     <*> maxChanSize
     <*> bounces
     <*> acceptP
     <*> maxSoftBounces
     <*> tickRate 's' "short"  T._shortTickRate
     <*> tickRate 'm' "medium" T._mediumTickRate
     <*> tickRate 'l' "long"   T._longTickRate
     <*> poolTimeout
     <*> verbosity
     <*> pure Set.empty -- TODO: specify bootstrap servers via command line
     <*> floodMessageCache



poolConfig :: Parser T.PoolConfig
poolConfig = T.PoolConfig <$> poolSize



bootstrapConfig :: Parser T.BootstrapConfig
bootstrapConfig = T.BootstrapConfig
      <$> restartEvery
      <*> restartMinimumPeriod
      <*> nodeConfig
      <*> poolConfig



multiConfig :: Parser T.MultiConfig
multiConfig = T.MultiConfig <$> nodeConfig <*> poolConfig



drawingConfig :: Parser T.DrawingConfig
drawingConfig = T.DrawingConfig <$> nodeConfig <*> poolConfig



port :: Parser Int
port = (option . mconcat)
      [ long    "port"
      , short   'p'
      , metavar "PORT"
      , help    "Server port"
      ]



restartEvery :: Parser Int
restartEvery = (nullOption . mconcat)
      [ reader positive
      , long    "restart-every"
      , showDefault
      , value   (T._restartEvery Default.bootstrapConfig)
      , metavar "(Int > 0)"
      , help    "Restart a random pool node every n new nodes. (Note that a restart is one new node by itself already.)"
      , hidden
      ]



restartMinimumPeriod :: Parser Int
restartMinimumPeriod = (nullOption . mconcat)
      [ reader positive
      , long    "restart-minperiod"
      , showDefault
      , value   (T._restartMinimumPeriod Default.bootstrapConfig)
      , metavar "[ms]"
      , help    "Restart a random pool node every n new nodes. (Note that a restart is one new node by itself already.)"
      , hidden
      ]



poolSize :: Parser Int
poolSize = (nullOption . mconcat)
      [ reader positive
      , long    "poolsize"
      , short   'n'
      , showDefault
      , value   (T._poolSize Default.poolConfig)
      , metavar "(Int > 0)"
      , help    "Number of nodes in the pool"
      ]



minNeighbours :: Parser Int
minNeighbours = (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (T._maxNeighbours Default.nodeConfig)
      , long    "maxn"
      , metavar "(Int > 0)"
      , help    "Minimum amount of neighbours (up-/downstream separate)"
      ]



maxNeighbours :: Parser Int
maxNeighbours = (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (T._minNeighbours Default.nodeConfig)
      , long    "minn"
      , metavar "(Int > 0)"
      , help    "Maximum amount of neighbours (up-/downstream separate)"
      ]



maxChanSize :: Parser Int
maxChanSize = (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (T._maxChanSize Default.nodeConfig)
      , long    "chansize"
      , metavar "(Int > 0)"
      , help    "Maximum communication channel size"
      , hidden
      ]



floodMessageCache :: Parser Int
floodMessageCache = (nullOption . mconcat)
      [ reader nonnegative
      , showDefault
      , value   (T._floodMessageCache Default.nodeConfig)
      , long    "floodcache"
      , metavar "(Int >= 0)"
      , help    "Number of past flood messages to cache"
      , hidden
      ]



bounces :: Parser Word
bounces = (nullOption . mconcat)
      [ reader nonnegative
      , showDefault
      , value   (T._bounces Default.nodeConfig)
      , long    "bounces"
      , metavar "(Int >= 0)"
      , help    "Minimum edge search hard bounces"
      , hidden
      ]



maxSoftBounces :: Parser Word
maxSoftBounces = (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (T._maxSoftBounces Default.nodeConfig)
      , long    "hbounce"
      , metavar "(Int > 0)"
      , help    "Maximum edge search soft bounces"
      , hidden
      ]



acceptP :: Parser Double
acceptP = (nullOption . mconcat)
      [ reader probability
      , showDefault
      , value   (T._acceptP Default.nodeConfig)
      , long    "acceptp"
      , metavar "(0 < p <= 1)"
      , help    "Edge request soft bounce acceptance probability"
      , hidden
      ]


-- | Generate s/m/l tickrate parser. Example use:
--
--   > tickRate 's' "short"  T._shortTickRate
tickRate :: Char              -- ^ short parameter name
         -> String            -- ^ Long parameter name
         -> (T.NodeConfig -> Int) -- ^ Accessor to get the default value
         -> Parser Int
tickRate shortName name getter = (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (getter Default.nodeConfig)
      , long    (shortName : "tick")
      , metavar "[ms]"
      , help    ("Tick rate of " ++ name ++ " loops")
      , hidden
      ]



poolTimeout :: Parser Double
poolTimeout = (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (T._poolTimeout Default.nodeConfig)
      , long    "timeout"
      , metavar "[s]"
      , help    "Timeout threshold"
      , hidden
      ]



verbosity :: Parser T.Verbosity
verbosity = (nullOption . mconcat)
      [ reader readVerbosity
      , value   (T._verbosity Default.nodeConfig)
      , long    "verbosity"
      , metavar "(mute|quiet|default|debug|chatty)"
      , help    "Verbosity level, increasing from left to right"
      ]





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



readVerbosity :: String -> ReadM T.Verbosity
readVerbosity x = case map toLower x of
      "mute"    -> pure T.Mute
      "quiet"   -> pure T.Quiet
      "default" -> pure T.Default
      "debug"   -> pure T.Debug
      "chatty"  -> pure T.Chatty
      _else     -> readerError (printf "Unrecognized verbosity level \"%d\"" x)