-- TODO: Refactoring :-)

-- TODO: Make sure the node doesn't connect to itself
-- TODO: Lots of status/logging messages
-- TODO: The wire protocol uses Int64 length headers. Make the program robust
--       against too long messages that exceed the size. Maybe use
--       hGetContents after all?
-- TODO: Create "network snapshot" type message to generate GraphViz pictures
--       of how everything looks like
-- TODO: Randomly replace downstream neighbours
-- TODO: Split into multiple modules

-- TO DO: Rewrite all the Environment parameters to ReaderT.


{-# LANGUAGE CPP #-}

-- {-# OPTIONS_GHC -ddump-splices #-} -- For Lens.TH debugging
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}



module Main where



import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Network

import Server (serverLoop, randomSocket)
import ClientPool
import Types
import Bootstrap (bootstrap)




main :: IO ()
main = startNode





-- | Node main function. Bootstraps, launches server loop, client pool,
--   terminal IO thread.
startNode :: IO ()
startNode = do

      let config = defaultConfig -- Can easily be changed to a command args parser

      bracket (randomSocket config) sClose $ \socket -> do

            ~(PortNumber port) <- socketPort socket
            putStrLn "Starting bootstrap" -- IO thread doesn't exist yet
            host <- bootstrap config port

            -- Setup all the communication channels
            env <- initEnvironment (Node host port) config

            -- Start server loop
            withAsync (serverLoop socket env) $ \server -> do
                  void . forkIO $ outputThread (_io env) -- Dedicated IO thread
                  void . forkIO $ clientPool env -- Client pool
                  wait server






-- | Initializes node environment by setting up the communication channels etc.
initEnvironment :: Node -> Config -> IO Environment
initEnvironment node config = Environment

      <$> newTVarIO Map.empty -- Known nodes
      <*> newTVarIO Map.empty -- Nodes known by plus last signal timestamps
      <*> newBroadcastTChanIO -- Channel to all clients
      <*> newTBQueueIO size   -- Channel to one client
      <*> newTBQueueIO size   -- Channel to the IO thread
      <*> newTVarIO Set.empty -- Previously handled queries
      <*> pure node           -- Own server's address
      <*> pure config

      where size = _maxChanSize config


defaultConfig :: Config
defaultConfig = Config {
        _maxNeighbours     = 10
      , _minNeighbours     = 5
      , _portRange         = (20000, 20100)
      , _maxChanSize       = 100
      , _maxRandomPorts    = 10
      , _bounces           = 3
      , _lambda            = 1.5 -- TODO: Error on lambda <= 1
      , _poolTickRate      = 1 * 10^6
      , _keepAliveTickRate = 3 * 10^5
      , _poolTimeout       = 10
}





-- | Dedicated (I)O thread to make sure messages aren't scrambled up. Reads
--   IO actions from a queue and executes them.
outputThread :: TBQueue (IO ()) -> IO ()
outputThread = forever . join . atomically . readTBQueue








