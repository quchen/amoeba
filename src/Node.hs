-- TODO: The wire protocol uses Int64 length headers. Make the program robust
--       against too long messages that exceed the size. Maybe use
--       hGetContents after all?
-- TODO: Randomly replace downstream neighbours?
-- TODO: Randomly kick nodes if the maximum capacity is reached
-- TODO: Create a new signal that makes every node send a list of neighbours to
--       a specific node, which then constructs a GraphViz representation of the
--       network
-- TODO: Error handling. Right now many exceptions kill the entire process
--       because there are no catches.
-- TODO: Restart bootstrapping process if all downstream neighbours are lost
--       (Wait some time for incoming edge requests though? They may contain
--       potential new downstream neighbours.)
-- TODO: Instead of having the client pool clean up dead workers, each worker
--       should have an individual dead man switch thread. This should a) be
--       quicker to react and b) more in the spirit of something decentralized.
-- TODO: Configuration sanity checks:
--           neighbours  -->  max >= min
--           tickrates   -->  short <= medium <= long




-- TODO export list
module Node where



import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Pipes.Concurrent as P
import qualified Pipes.Network.TCP as P

--import Bootstrap
--import ClientPool
--import Server
import Types
import Utilities


-- | Node main function. Bootstraps, launches server loop, client pool,
--   IO thread.
startNode :: Maybe (PChan NormalSignal) -- ^ Local direct connection (LDC)
          -> Config -- ^ Configuration, most likely given by command line
                    --   parameters
          -> IO ()
startNode ldc config = do

      let bootstrap  = undefined -- TODO
          server     = undefined -- TODO. New version of Server.startServer.
          clientPool = undefined

      let port = _serverPort config
      putStrLn "Starting bootstrap" -- IO thread doesn't exist yet
      host <- bootstrap config port
      putStrLn "Bootstrap finished" -- debugging
      let self = Node host port
      env <- initEnvironment self ldc config

      let listen' node = P.listen (P.Host $ _host node)
                                  (show   $ _port node)
      listen' self $ \(socket, serverAddr) -> do
            yell 32 $ "Server listening on " ++ show serverAddr
            asyncMany [ server env socket
                      , outputThread $ _io env
                      , clientPool env
                      ]
                      wait



-- | Initializes node environment by setting up the communication channels etc.
initEnvironment :: Node
                -> Maybe (PChan NormalSignal)
                -> Config
                -> IO Environment
initEnvironment node ldc config = Environment

      <$> newTVarIO Map.empty -- Known nodes
      <*> newTVarIO Map.empty -- Nodes known by
      <*> spawnP buffer       -- Channel to all clients
      <*> spawnP buffer       -- Channel to one client
      <*> newTBQueueIO size   -- Channel to the IO thread
      <*> newTVarIO Set.empty -- Previously handled queries
      <*> pure node           -- Own server's address
      <*> pure ldc            -- (Maybe) local direct connection
      <*> pure config

      where size = _maxChanSize config
            buffer = P.Bounded size






-- | Dedicated (I)O thread to make sure messages aren't scrambled up. Reads
--   IO actions from a queue and executes them.
outputThread :: TBQueue (IO ()) -> IO ()
outputThread = forever . join . atomically . readTBQueue








