module CmdArgParser (parseArgs) where

import Options.Applicative
import Network
import Data.Word
import Data.Monoid
import Data.Char (toLower)
import Text.Read (readEither)

import qualified Types as T

parseArgs = execParser opts
      where opts = info (helper <*> config) $
                   fullDesc
                <> progDesc "Amoeba client"
                <> header "Launch a single node in an Amoeba network"



-- | Default configuration, used to set the values of optional parameters.
defaultConfig :: T.Config
defaultConfig = T.Config {
        T._serverPort        = 21000
      , T._maxNeighbours     = 6
      , T._minNeighbours     = 3
      , T._maxChanSize       = 100
      , T._bounces           = 1
      , T._acceptP           = 0.5
      , T._maxSoftBounces    = 10
      , T._shortTickRate     = 1 * 10^5
      , T._mediumTickRate    = 3 * 10^5
      , T._longTickRate      = 10^6
      , T._poolTimeout       = 5
      , T._verbosity         = T.Debug -- TODO: Change back to Default for production
      , T._bootstrapServers  = []
      }



config :: Parser T.Config
config = T.Config
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



port :: Parser Int
port = option $ mconcat [ long    "port"
                        , short   'p'
                        , metavar "PORT"
                        , help    "Server port"
                        ]

minNeighbours :: Parser Word
minNeighbours = nullOption $ mconcat
      [ reader positive
      , showDefault
      , value $ T._maxNeighbours defaultConfig
      , long    "maxn"
      , metavar "<INT > 0>"
      , help    "Minimum amount of neighbours (up-/downstream separate)"
      ]

maxNeighbours :: Parser Word
maxNeighbours = nullOption $ mconcat
      [ reader positive
      , showDefault
      , value $ T._minNeighbours defaultConfig
      , long    "minn"
      , metavar "<INT > 0>"
      , help    "Maximum amount of neighbours (up-/downstream separate)"
      ]

maxChanSize :: Parser Int
maxChanSize = nullOption $ mconcat
      [ reader positive
      , showDefault
      , value $ T._maxChanSize defaultConfig
      , long    "chansize"
      , metavar "<INT > 0>"
      , help    "Maximum communication channel size"
      ]

bounces :: Parser Word
bounces = nullOption $ mconcat
      [ reader nonnegative
      , showDefault
      , value $ T._bounces defaultConfig
      , long    "bounces"
      , metavar "<INT >= 0>"
      , help    "Minimum edge search hard bounces"
      ]

maxSoftBounces :: Parser Word
maxSoftBounces = nullOption $ mconcat
      [ reader positive
      , showDefault
      , value $ T._maxSoftBounces defaultConfig
      , long    "hbounce"
      , metavar "<INT > 0>"
      , help    "Maximum edge search soft bounces"
      ]




acceptP :: Parser Double
acceptP = nullOption $ mconcat
      [ reader probability
      , showDefault
      , value $ T._acceptP defaultConfig
      , long    "acceptp"
      , metavar "<0 < p <= 1>"
      , help    "Edge request soft bounce acceptance probability"
      ]

tickRate :: Char -> String -> (T.Config -> Int) -> Parser Int
tickRate shortName name getter = nullOption $ mconcat
      [ reader positive
      , showDefaultWith $ \val -> show val
      , value $ getter defaultConfig
      , long  $ shortName : "tick"
      , metavar "MILLISECONDS"
      , help  $ "Tick rate of " ++ name ++ " loops"
      ]

poolTimeout :: Parser Double
poolTimeout = nullOption $ mconcat
      [ reader positive
      , showDefault
      , value $ T._poolTimeout defaultConfig
      , long    "timeout"
      , metavar "SECONDS"
      , help    "Timeout threshold"
      ]

verbosity :: Parser T.Verbosity
verbosity = nullOption $ mconcat
      [ reader readVerbosity
      , value $ T._verbosity defaultConfig
      , long    "verbosity"
      , metavar "<mute|quiet|default|debug|chatty>"
      , help    "Verbosity level, increasing from left to right"
      ]



-- | Reader for a Double between 0 and 1
probability :: (Num a, Ord a, Read a) => String -> Either ParseError a
probability x = case readEither x of
      Right x' | x' >= 0 && x' <= 1 -> Right x'
      Right _  -> Left . ErrorMsg $ "Bad probability " ++ x ++ "; 0 <= p <= 1"
      Left _   -> Left . ErrorMsg $ "Parse error on double " ++ x



positive :: (Num a, Ord a, Read a) => String -> Either ParseError a
positive x = case readEither x of
      Right x' | x' > 0 -> Right x'
      Right _  -> Left . ErrorMsg $ "Positive number expected ( " ++ x ++ " given)"
      Left _   -> Left . ErrorMsg $ "Parse error on integer " ++ x



nonnegative :: (Num a, Ord a, Read a) => String -> Either ParseError a
nonnegative x = case readEither x of
      Right x' | x' >= 0 -> Right x'
      Right _  -> Left . ErrorMsg $ "Nonnegative number expected ( " ++ x ++ " given)"
      Left _   -> Left . ErrorMsg $ "Parse error on integer " ++ x


readVerbosity :: String -> Either ParseError T.Verbosity
readVerbosity x = case map toLower x of
      "mute"    -> Right T.Mute
      "quiet"   -> Right T.Quiet
      "default" -> Right T.Default
      "debug"   -> Right T.Debug
      "chatty"  -> Right T.Chatty
      _         -> Left . ErrorMsg $ "Unrecognized verbosity level \"" ++ x ++ "\""