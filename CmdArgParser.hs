import Options.Applicative
import Network
import Data.Word

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