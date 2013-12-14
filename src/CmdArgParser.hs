module CmdArgParser (
        parseNodeArgs
      , parseBSArgs
) where

import Options.Applicative
import Data.Word
import Data.Monoid
import Text.Printf
import Data.Char (toLower)
import Text.Read (readEither)

import qualified Types as T



parseNodeArgs :: IO T.Config
parseNodeArgs = execParser parser
      where parser = info (helper <*> nodeConfig) infoMod
            infoMod = mconcat
                  [ fullDesc
                  , progDesc "Amoeba client"
                  , header "Launch a single node in an Amoeba network"
                  ]



parseBSArgs :: IO (T.Config, T.BSConfig)
parseBSArgs = execParser parser
      where parser = info (helper <*> p) infoMod
            p = (,) <$> nodeConfig <*> bsConfig
            infoMod = mconcat
                  [ fullDesc
                  , progDesc "Amoeba bootstrap server"
                  , header "Start a bootstrap server to allow new nodes to\
                           \ connect to an existing network"
                  ]



-- | Default node configuration, used to set the values of optional parameters.
defaultNodeConfig :: T.Config
defaultNodeConfig = T.Config {
        T._serverPort        = 21000
      , T._maxNeighbours     = 6
      , T._minNeighbours     = 3
      , T._maxChanSize       = 100
      , T._bounces           = 1
      , T._acceptP           = 0.5
      , T._maxSoftBounces    = 10
      , T._shortTickRate     = 1 * 10^5 `div` 5
      , T._mediumTickRate    = 3 * 10^5 `div` 5
      , T._longTickRate      = 10^6     `div` 5
      , T._poolTimeout       = 5
      , T._verbosity         = T.Debug -- TODO: Change back to Default for production
      , T._bootstrapServers  = []
      }



-- | Default bootstrap server config
defaultBSConfig :: T.BSConfig
defaultBSConfig = T.BSConfig {
        T._poolSize = 5
      }



nodeConfig :: Parser T.Config
nodeConfig = T.Config
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
     <*> pure [] -- TODO: specify bootstrap servers via command line


bsConfig :: Parser T.BSConfig
bsConfig = T.BSConfig
      <$> poolSize


-- TODO: ensure epositive
port :: Parser Int
port = (option . mconcat)
      [ long    "port"
      , short   'p'
      , metavar "PORT"
      , help    "Server port"
      ]



poolSize :: Parser Int
poolSize = (option . mconcat)
      [ long    "poolsize"
      , short   'n'
      , metavar "<INT > 0>"
      , help    "Number of nodes in the pool"
      ]



-- TODO: ensure epositive
minNeighbours :: Parser Int
minNeighbours = (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (T._maxNeighbours defaultNodeConfig)
      , long    "maxn"
      , metavar "<INT > 0>"
      , help    "Minimum amount of neighbours (up-/downstream separate)"
      ]



-- TODO: ensure epositive
maxNeighbours :: Parser Int
maxNeighbours = (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (T._minNeighbours defaultNodeConfig)
      , long    "minn"
      , metavar "<INT > 0>"
      , help    "Maximum amount of neighbours (up-/downstream separate)"
      ]



-- TODO: ensure epositive
maxChanSize :: Parser Int
maxChanSize = (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (T._maxChanSize defaultNodeConfig)
      , long    "chansize"
      , metavar "<INT > 0>"
      , help    "Maximum communication channel size"
      ]



bounces :: Parser Word
bounces = (nullOption . mconcat)
      [ reader nonnegative
      , showDefault
      , value   (T._bounces defaultNodeConfig)
      , long    "bounces"
      , metavar "<INT >= 0>"
      , help    "Minimum edge search hard bounces"
      ]



maxSoftBounces :: Parser Word
maxSoftBounces = (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (T._maxSoftBounces defaultNodeConfig)
      , long    "hbounce"
      , metavar "<INT > 0>"
      , help    "Maximum edge search soft bounces"
      ]



acceptP :: Parser Double
acceptP = (nullOption . mconcat)
      [ reader probability
      , showDefault
      , value   (T._acceptP defaultNodeConfig)
      , long    "acceptp"
      , metavar "<0 < p <= 1>"
      , help    "Edge request soft bounce acceptance probability"
      ]


-- | Function to get s/m/l tickrate parser. Example use:
--
--   > tickRate 's' "short"  T._shortTickRate
tickRate :: Char              -- ^ short parameter name
         -> String            -- ^ Long parameter name
         -> (T.Config -> Int) -- ^ Accessor to get the default value
         -> Parser Int
tickRate shortName name getter = (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (getter defaultNodeConfig)
      , long    (shortName : "tick")
      , metavar "MILLISECONDS"
      , help    ("Tick rate of " ++ name ++ " loops")
      ]



poolTimeout :: Parser Double
poolTimeout = (nullOption . mconcat)
      [ reader positive
      , showDefault
      , value   (T._poolTimeout defaultNodeConfig)
      , long    "timeout"
      , metavar "SECONDS"
      , help    "Timeout threshold"
      ]



verbosity :: Parser T.Verbosity
verbosity = (nullOption . mconcat)
      [ reader readVerbosity
      , value   (T._verbosity defaultNodeConfig)
      , long    "verbosity"
      , metavar "<mute|quiet|default|debug|chatty>"
      , help    "Verbosity level, increasing from left to right"
      ]



-- | 0 <= p <= 1
probability :: (Num a, Ord a, Read a) => String -> ReadM a
probability x = case readEither x of
      Right y | y >= 0 && y <= 1 -> pure y
      Right _ -> readerError (printf "Bad probability %s; 0 <= p <= 1" x)
      Left  _ -> readerError (printf "Parse error on double %s" x)



positive :: (Num a, Ord a, Read a) => String -> ReadM a
positive x = case readEither x of
      Right y | y > 0 -> pure y
      Right _ -> readerError (printf "Positive number expected (%s given)" x)
      Left  _ -> readerError (printf "Parse error on integer %s" x)



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