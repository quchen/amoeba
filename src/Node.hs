-- TODO: Lots of status/logging messages
-- TODO: The wire protocol uses Int64 length headers. Make the program robust
--       against too long messages that exceed the size. Maybe use
--       hGetContents after all?
-- TODO: Randomly replace downstream neighbours
-- TODO: Randomly kick nodes if the maximum capacity is reached
-- TODO: Create a new signal that makes every node send a list of neighbours to
--       a specific node, which then constructs a GraphViz representation of the
--       network
-- TODO: Error handling. Right now any exception kills everything because it's
--       rethrown in the parent thread (thanks to Async).
-- TODO: When there are no clients, the chans will be filled up with edge
--       requests all the way. GHC 7.8 can easily generate an error using the
--       new "isFullTBQueue" function.
-- TODO: Restart bootstrapping process if all downstream neighbours are lost
--       (Wait some time for incoming edge requests though? They may contain
--       potential new downstream neighbours.)
-- TODO: Upstream neighbours are not rejected (enough?) when the pool is full
-- TODO: Instead of having the client pool clean up dead workers, each worker
--       should have an individual dead man switch thread. This should a) be
--       quicker to react and b) more in the spirit of something decentralized.
-- TODO: Make the timers slow/medium/fast instead of having an individual tick
--       rate for each process
-- TODO: Configuration sanity checks:
--           neighbours  -->  max >= min
--           tickrates   -->  short <= medium <= long





module Node (startNode) where



import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import           Network
import qualified Data.Set as Set

import Bootstrap
import ClientPool
import Server
import Types



-- | Node main function. Bootstraps, launches server loop, client pool,
--   terminal IO thread.
startNode :: Config -> IO Node
startNode config = do

      let port = _serverPort config
      putStrLn "Starting bootstrap" -- IO thread doesn't exist yet
      host <- bootstrap config port
      putStrLn "Bootstrap finished" -- debugging
      let self = Node host port
      env <- initEnvironment self config

      -- Fork entire node. Only here so this function can return early.
      async $ bracket (listenOn $ PortNumber port) sClose $ \socket -> do
       withAsync (serverLoop socket env) $ \server  -> do
        withAsync (outputThread $ _io env) $ \_output -> do
         withAsync (clientPool env) $ \_cPool  -> do
          wait server
            -- NB: When the server finishes, the other asyncs are canceled by
            --     withAsync.

      return self






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






-- | Dedicated (I)O thread to make sure messages aren't scrambled up. Reads
--   IO actions from a queue and executes them.
outputThread :: TBQueue (IO ()) -> IO ()
outputThread = forever . join . atomically . readTBQueue








