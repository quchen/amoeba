-- TODO: Lots of status/logging messages
-- TODO: The wire protocol uses Int64 length headers. Make the program robust
--       against too long messages that exceed the size. Maybe use
--       hGetContents after all?
-- TODO: Create "network snapshot" type message to generate GraphViz pictures
--       of how everything looks like
-- TODO: Randomly replace downstream neighbours
-- TODO: Create a new signal that makes every node send a list of neighbours to
--       a specific node, which then constructs a GraphViz representation of the
--       network
-- TODO: Write options parser
-- TODO: Make clients update their timestamps in the DB regularly
-- TODO: Decent logging
-- TODO: Error handling. Right now any exception kills everything because it's
--       rethrown in the parent thread (thanks to Async).
-- TODO: When there are no clients, the chans will be filled up with edge
--       requests all the way





module Main where



import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Network
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment (getArgs)

import Bootstrap
import ClientPool
import Server
import Types




main :: IO ()
main = startNode





-- | Node main function. Bootstraps, launches server loop, client pool,
--   terminal IO thread.
startNode :: IO ()
startNode = do

      let config = defaultConfig -- Can easily be changed to a command args parser

      -- TODO: Proper argument parsing
      args <- getArgs
      let port = if null args then _serverPort config
                              else fromIntegral $ read (head args)

      bracket (listenOn $ PortNumber port) sClose $ \socket -> do

            putStrLn "Starting bootstrap" -- IO thread doesn't exist yet
            host <- bootstrap config port
            putStrLn "Bootstrap finished" -- debugging

            -- Setup all the communication channels
            env <- initEnvironment (Node host port) config

            withAsync (serverLoop socket env)   $ \server  ->
             withAsync (outputThread $ _io env) $ \_output ->
             withAsync (clientPool env)         $ \_cPool  ->
             wait server
            -- NB: When the server finishes, the other asyncs are canceled by
            --     withAsync.






-- | Initializes node environment by setting up the communication channels etc.
initEnvironment :: Node -> Config -> IO Environment
initEnvironment node config = Environment

      <$> newTVarIO Map.empty -- Known nodes
      <*> newTVarIO Map.empty -- Nodes known by
      <*> newBroadcastTChanIO -- Channel to all clients
      <*> newTBQueueIO size   -- Channel to one client
      <*> newTBQueueIO size   -- Channel to the IO thread
      <*> newTVarIO Set.empty -- Previously handled queries
      <*> pure node           -- Own server's address
      <*> pure config

      where size = _maxChanSize config


defaultConfig :: Config
defaultConfig = Config {
        _serverPort        = 21000
      , _maxNeighbours     = 10
      , _minNeighbours     = 1
      , _maxChanSize       = 100
      , _bounces           = 3
      , _acceptP           = 0.5 -- TODO: Error if not 0 < p <= 1
      , _poolTickRate      = 1 * 10^6
      , _keepAliveTickRate = 3 * 10^5
      , _poolTimeout       = 10
      , _verbosity         = Chatty
}





-- | Dedicated (I)O thread to make sure messages aren't scrambled up. Reads
--   IO actions from a queue and executes them.
outputThread :: TBQueue (IO ()) -> IO ()
outputThread = forever . join . atomically . readTBQueue








