-- TODO: Randomly replace downstream neighbours?
-- TODO: Randomly kick nodes if the maximum capacity is reached
-- TODO: Exception handling
-- TODO: Restart bootstrapping process if all downstream neighbours are lost
--       (Wait some time for incoming edge requests though? They may contain
--       potential new downstream neighbours.)
-- TODO: Configuration sanity checks:
--           neighbours  -->  max >= min
--           tickrates   -->  short <= medium <= long
-- TODO: Database utilities module




module Node (startNode) where



import           Text.Printf
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Control.Exception (assert)

import qualified Pipes.Concurrent as P

import qualified Pipes.Network.TCP as PN
import qualified Network.Socket as NS

import Bootstrap
import ClientPool
import Server
import Types
import Utilities


-- | Node main function. Bootstraps, launches server loop, client pool,
--   IO thread.
startNode :: Maybe (PChan NormalSignal) -- ^ Local direct connection (LDC)
          -> TBQueue (IO ()) -- ^ Channel to output thread
          -> Config -- ^ Configuration, most likely given by command line
                    --   parameters
          -> IO ()
startNode ldc output config = do

      let port = _serverPort config

      PN.listen (PN.Host "127.0.0.1") (show port) $ \(socket, addr) -> do

            (selfHost, selfPort) <- getSelfInfo addr
            assert (port == read selfPort) $ return ()
            let self = To (Node selfHost port)

            bootstrap config self

            env <- initEnvironment self ldc output config

            yell 32 $ "Node server listening on " ++ show self
            withAsync (server env socket) $ \serverThread ->
             withAsync (clientPool env) $ \_ ->
              wait serverThread



-- | Retrieve own server address
getSelfInfo :: PN.SockAddr -> IO (PN.HostName, PN.ServiceName)
getSelfInfo addr = fromJust' <$> NS.getNameInfo flags True True addr
      where flags = [ NS.NI_NUMERICHOST -- "IP address, not DNS"
                    , NS.NI_NUMERICSERV -- "Port as a number please"
                    ]
            fromJust' (Just x, Just y) = (x,y)
            fromJust' (x, y) =
                  let msg = "Address lookup failed! This is a bug.\
                            \ (Host: %s, Port: %s)"
                  in  error (printf msg (show x) (show y))



-- | Initializes node environment by setting up the communication channels etc.
initEnvironment :: To                         -- ^ Own address
                -> Maybe (PChan NormalSignal) -- ^ Local direct connection
                -> TBQueue (IO ())            -- ^ Channel to output thread
                -> Config
                -> IO Environment
initEnvironment node ldc output config = Environment

      <$> newTVarIO Map.empty -- Known nodes
      <*> newTVarIO Map.empty -- Nodes known by
      <*> spawn buffer        -- Channel read by all clients
      <*> pure output         -- Channel to the IO thread
      <*> newTVarIO Set.empty -- Previously handled queries
      <*> pure node           -- Own server's address
      <*> pure ldc            -- (Maybe) local direct connection
      <*> pure config

      where size = _maxChanSize config
            buffer = P.Bounded size





