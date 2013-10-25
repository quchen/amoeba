-- TODO: Randomly replace downstream neighbours?
-- TODO: Randomly kick nodes if the maximum capacity is reached
-- TODO: Exception handling
-- TODO: Restart bootstrapping process if all downstream neighbours are lost
--       (Wait some time for incoming edge requests though? They may contain
--       potential new downstream neighbours.)
-- TODO: Try to make clients clean up better after themselves. Housekeeping is
--       a hack.
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
import qualified Data.Set as Set

import qualified Pipes.Concurrent as P
import qualified Pipes.Network.TCP as P

import Bootstrap
import ClientPool
import Server
import Types
import Utilities


-- | Node main function. Bootstraps, launches server loop, client pool,
--   IO thread.
startNode :: Maybe (PChan NormalSignal) -- ^ Local direct connection (LDC)
          -> Config -- ^ Configuration, most likely given by command line
                    --   parameters
          -> IO ()
startNode ldc config = do

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
      <*> spawn buffer        -- Channel read by all clients
      <*> newTBQueueIO size   -- Channel to the IO thread
      <*> newTVarIO Set.empty -- Previously handled queries
      <*> pure node           -- Own server's address
      <*> pure ldc            -- (Maybe) local direct connection
      <*> pure config

      where size = _maxChanSize config
            buffer = P.Bounded size





