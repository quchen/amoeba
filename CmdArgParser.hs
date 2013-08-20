import Options.Applicative
import Network
import Data.Word
import Text.Read (readEither)

main = execParser opts >>= print
      where opts = info (helper <*> config) $
                   fullDesc
                <> progDesc "Amoeba client"
                <> header "Testing optparse-applicative"

data Config = Config {
        _serverPort     :: PortNumber
      --, _maxNeighbours  :: Word
      --, _minNeighbours  :: Word
      , _maxChanSize    :: Int
      , _bounces        :: Word
      , _acceptP        :: Double
      , _poolTickRate   :: Int
      , _keepAliveTickRate :: Int
      , _poolTimeout    :: Double
      --, _verbosity      :: Verbosity
} deriving (Show)
data Verbosity = Chatty | Debug | Default | Quiet | Mute
      deriving (Show)

config :: Parser Config
config = Config
     <$> port
     <*> maxChanSize
     <*> bounces
     <*> acceptP
     <*> poolTickRate
     <*> keepAliveTickRate
     <*> poolTimeout



port :: Parser PortNumber
port = let toPN = fromIntegral :: Int -> PortNumber
       in fmap toPN . option $ long    "port"
                            <> short   'p'
                            <> metavar "PORT"
                            <> help    "Server port"

-- TODO: Enforce positive
maxChanSize :: Parser Int
maxChanSize = option $ long    "chansize"
                    <> metavar "INT"
                    <> help    "Maximum communication channel size"

bounces :: Parser Word
bounces = option $ long    "hbounce"
                <> metavar "UINT"
                <> help    "Maximum edge search hard bounces"


-- TODO: Ensure 0 < p <= 1
acceptP :: Parser Double
acceptP = option $ long    "acceptp"
                <> metavar "[0<p<=1]"
                <> help    "Edge request soft bounce acceptance probability"

poolTickRate :: Parser Int
poolTickRate = option $ long    "ptick"
                     <> metavar "MILLISECONDS"
                     <> help    "Tick rate of the client pool"

keepAliveTickRate :: Parser Int
keepAliveTickRate = option $ long    "ktick"
                          <> metavar "MILLISECONDS"
                          <> help    "Tick rate for sending keep-alive signals"

-- TODO: Ensure positive
poolTimeout :: Parser Double
poolTimeout = option $ long    "timeout"
                    <> metavar "SECONDS"
                    <> help    "Timeout threshold"

verbosity :: Parser Verbosity
verbosity = undefined

maxNeighbours :: Parser Word
maxNeighbours = undefined

minNeighbours :: Parser Word
minNeighbours = undefined



-- | Reader for a Double between 0 and 1
probability :: String -> Either ParseError Double
probability x = case readEither x of
      Right x' | x' >= 0 && x' <= 1 -> Right x'
      Right x' -> Left . ErrorMsg $ "Bad probability " ++ x ++ "; 0 <= p <= 1"
      Left _   -> Left . ErrorMsg $ "Parse error on double " ++ x



positiveInt :: String -> Either ParseError Int
positiveInt x = case readEither x of
      Right x' | x' > 0 -> Right x'
      Right x' -> Left . ErrorMsg $ "Positive number expected ( " ++ x ++ " given)"
      Left _   -> Left . ErrorMsg $ "Parse error on integer " ++ x



nonzeroInt :: String -> Either ParseError Int
nonzeroInt x = case readEither x of
      Right x' | x' >= 0 -> Right x'
      Right x' -> Left . ErrorMsg $ "Nonzero number expected ( " ++ x ++ " given)"
      Left _   -> Left . ErrorMsg $ "Parse error on integer " ++ x