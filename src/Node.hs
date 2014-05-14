-- | A node is a single participant in an Amoeba network.

module Node (startNode) where



import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception (assert)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Text.Printf

import qualified Pipes.Concurrent as P

import qualified Network.Socket as NS
import qualified Pipes.Network.TCP as PN

import           Bootstrap
import           ClientPool
import           Server
import           Types
import           Utilities


-- | Node main function. Bootstraps, launches server loop, client pool,
--   IO thread.
startNode :: Maybe (PChan NormalSignal) -- ^ Local direct connection (LDC)
          -> IOQueue -- ^ Channel to output thread
          -> NodeConfig
          -> IO ()
startNode ldc output config = do

      let port = _serverPort config

      PN.listen (PN.Host "127.0.0.1") (show port) $ \(socket, addr) -> do

            (selfHost, selfPort) <- getSelfInfo addr
            assert (port == read selfPort)
                   (return ())
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
                -> IOQueue                    -- ^ Channel to output thread
                -> NodeConfig
                -> IO Environment
initEnvironment node ldc output config = Environment

      <$> newTVarIO Map.empty -- Known nodes
      <*> newTVarIO Set.empty -- Nodes known by
      <*> spawn buffer        -- Channel read by all clients
      <*> pure output         -- Channel to the IO thread
      <*> newTVarIO Set.empty -- Previously handled queries
      <*> pure node           -- Own server's address
      <*> pure ldc            -- (Maybe) local direct connection
      <*> pure config

      where size = _maxChanSize config
            buffer = P.Bounded size





