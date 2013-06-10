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

-- NOT TO DO: Rewrite all the NodeEnvironment parameters to Reader.
--       RESULT: Don't do it, the whole code is littered with liftIOs. The
--               explicit "ns" every time isn't so bad compared to it.


{-# LANGUAGE CPP #-}

-- {-# OPTIONS_GHC -ddump-splices #-} -- For Lens.TH debugging
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}



module Main where



-- Base/Platform
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy as BS
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word
import           GHC.Generics (Generic)
import           Network
import           System.IO
import           System.Random
import           Text.Printf

-- Hackage
import           Data.Binary

-- Botnet
import Server
import Client
import ClientPool
import Types




-- | Signifies a question, or a positive/negative answer to one.
data Predicate = Question | Yes | No
      deriving (Eq, Ord, Show, Generic)

instance Binary Predicate





main :: IO ()
main = startNode





-- | Node main function
startNode :: IO ()
startNode = bracket (randomSocket $ _maxRandomPorts config)
                    sClose $
                    \socket -> do

      ~(PortNumber port) <- socketPort socket
      putStrLn "Starting bootstrap" -- IO thread doesn't exist yet
      host <- bootstrap port

      env <- initEnvironment $ Node { _host = host, _port = port }

      -- Start server loop
      withAsync (serverLoop socket env) $ \server -> do
            void . forkIO $ localIO (_io env) -- Dedicated IO thread
            void . forkIO $ clientPool env -- Client pool
            wait server






-- | Initializes node environment by setting up the communication channels etc.
--
--   Warning: the result is non-total in the sense that it contains nonsense
--   data for its own address. Bootstrapping should occur immediately after
--   the environment is created to handle this!
initEnvironment :: Node -> IO NodeEnvironment
initEnvironment node = NodeEnvironment

      <$> newTVarIO Set.empty -- Known nodes
      <*> newTVarIO Map.empty -- Nodes known by plus last signal timestamps
      <*> newBroadcastTChanIO -- Channel to all clients
      <*> newTBQueueIO size   -- Channel to one client
      <*> newTBQueueIO size   -- Channel to the IO thread
      <*> newTVarIO Set.empty -- Previously handled queries

      <*> pure node           -- Own server's port
      <*> pure 10             -- Max neighbours
      <*> pure 5              -- Min neighbours
      <*> pure (20000, 20100) -- Port range
      <*> pure 100            -- Maximum channel size
      <*> pure 10             -- Maximum random ports to try
      <*> pure 3              -- Guaranteed bounces for edge search
      <*> pure 1.5            -- Exponential probability decay for edge search
      <*> pure (1 * 10^6)     -- Tickrate of the client pool in us
      <*> pure (3 * 10^5)     -- Tickrate of keep-alive signals in us
      <*> pure 10             -- Timeout for dead upstream nodes in s

      where size = _maxChanSize config





-- | Used for a dedicated IO thread. Reads IO actions from a queue and executes
--   them.
--
--   > localIO q = forever $ do action <- atomically $ readTBQueue q
--   >                          action
localIO :: TBQueue (IO ()) -> IO ()
localIO = forever . join . atomically . readTBQueue





-- | Sends an IO action to the IO thread.
toIO :: NodeEnvironment -> IO () -> STM ()
toIO env = writeTBQueue (_io env)


