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
                <> header "Testing optparse-applicative"

--data Config = Config {
--        _serverPort     :: PortNumber
--      , _maxNeighbours  :: Word
--      , _minNeighbours  :: Word
--      , _maxChanSize    :: Int
--      , _bounces        :: Word
--      , _acceptP        :: Double
--      , _poolTickRate   :: Int
--      , _keepAliveTickRate :: Int
--      , _poolTimeout    :: Double
--      , _verbosity      :: Verbosity
--} deriving (Show)
--data Verbosity = Chatty | Debug | Default | Quiet | Mute
--      deriving (Show)


config :: Parser T.Config
config = T.Config
     <$> port
     <*> minNeighbours
     <*> maxNeighbours
     <*> maxChanSize
     <*> bounces
     <*> acceptP
     <*> maxSoftBounces
     <*> poolTickRate
     <*> keepAliveTickRate
     <*> poolTimeout
     <*> verbosity



port :: Parser PortNumber
port = let toPN = fromIntegral :: Int -> PortNumber
       in fmap toPN . option $ mconcat
             [ long    "port"
             , short   'p'
             , metavar "PORT"
             , help    "Server port"
             ]

maxChanSize :: Parser Int
maxChanSize = nullOption $ mconcat
      [ reader positive
      , showDefault
      , value   10
      , long    "chansize"
      , metavar "<INT > 0>"
      , help    "Maximum communication channel size"
      ]

bounces :: Parser Word
bounces = nullOption $ mconcat
      [ reader nonnegative
      , showDefault
      , value   4
      , long    "hbounce"
      , metavar "<INT >= 0>"
      , help    "Maximum edge search hard bounces"
      ]

maxSoftBounces :: Parser Word
maxSoftBounces = nullOption $ mconcat
      [ reader positive
      , showDefault
      , value   10
      , long    "hbounce"
      , metavar "<INT > 0>"
      , help    "Maximum edge search soft bounces"
      ]




acceptP :: Parser Double
acceptP = nullOption $ mconcat
      [ reader probability
      , showDefault
      , value   0.5
      , long    "acceptp"
      , metavar "<0 < p <= 1>"
      , help    "Edge request soft bounce acceptance probability"
      ]

poolTickRate :: Parser Int
poolTickRate = nullOption $ mconcat
      [ reader positive
      , showDefaultWith $ \val -> show (val `quot` 10^6) ++ "e6"
      , value $ 3 * 10^6
      , long    "ptick"
      , metavar "MILLISECONDS"
      , help    "Tick rate of the client pool"
      ]

keepAliveTickRate :: Parser Int
keepAliveTickRate = nullOption $ mconcat
      [ reader positive
      , showDefaultWith $ \val -> show (val `quot` 10^6) ++ "e6"
      , value $ 3 * 10^6
      , long    "ktick"
      , metavar "MILLISECONDS"
      , help    "Tick rate for sending keep-alive signals"
      ]

poolTimeout :: Parser Double
poolTimeout = nullOption $ mconcat
      [ reader positive
      , showDefault
      , value   10
      , long    "timeout"
      , metavar "SECONDS"
      , help    "Timeout threshold"
      ]

verbosity :: Parser T.Verbosity
verbosity = nullOption $ mconcat
      [ reader readVerbosity
      , value   T.Default
      , long    "verbosity"
      , metavar "<mute|quiet|default|debug|chatty>"
      , help    "Verbosity level, increasing from left to right"
      ]

minNeighbours :: Parser Word
minNeighbours = nullOption $ mconcat
      [ reader positive
      , showDefault
      , value   5
      , long    "maxn"
      , metavar "<INT > 0>"
      , help    "Minimum amount of neighbours (up-/downstream separate)"
      ]

maxNeighbours :: Parser Word
maxNeighbours = nullOption $ mconcat
      [ reader positive
      , showDefault
      , value   20
      , long    "minn"
      , metavar "<INT > 0>"
      , help    "Maximum amount of neighbours (up-/downstream separate)"
      ]



-- | Reader for a Double between 0 and 1
probability :: (Num a, Ord a, Read a) => String -> Either ParseError a
probability x = case readEither x of
      Right x' | x' >= 0 && x' <= 1 -> Right x'
      Right x' -> Left . ErrorMsg $ "Bad probability " ++ x ++ "; 0 <= p <= 1"
      Left _   -> Left . ErrorMsg $ "Parse error on double " ++ x



positive :: (Num a, Ord a, Read a) => String -> Either ParseError a
positive x = case readEither x of
      Right x' | x' > 0 -> Right x'
      Right x' -> Left . ErrorMsg $ "Positive number expected ( " ++ x ++ " given)"
      Left _   -> Left . ErrorMsg $ "Parse error on integer " ++ x



nonnegative :: (Num a, Ord a, Read a) => String -> Either ParseError a
nonnegative x = case readEither x of
      Right x' | x' >= 0 -> Right x'
      Right x' -> Left . ErrorMsg $ "Nonnegative number expected ( " ++ x ++ " given)"
      Left _   -> Left . ErrorMsg $ "Parse error on integer " ++ x


readVerbosity :: String -> Either ParseError T.Verbosity
readVerbosity x = case map toLower x of
      "mute"    -> Right T.Mute
      "quiet"   -> Right T.Quiet
      "default" -> Right T.Default
      "debug"   -> Right T.Debug
      "chatty"  -> Right T.Chatty
      _         -> Left . ErrorMsg $ "Unrecognized verbosity level \"" ++ x ++ "\""