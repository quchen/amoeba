-- | Maintains a number of completely independent nodes, and allows sending
--   signals to them easily.
--
--   Useful to add certain services to the network without requiring them to
--   manage their own connections. For example, a bootstrap server can use a
--   node pool to always keep up a certain amount of trusted nodes in the
--   network that it can use to help other nodes make an initial connection.

module NodePool (nodePool) where



import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad

import           Pipes
import qualified Pipes.Concurrent as P

import           Node
import           Types
import           Utilities



-- | Start a node pool of a certain size, and provide a channel to
--   communitcate with (random nodes in) it
nodePool :: Int     -- ^ Number of nodes in the pool (also the port range)
         -> NodeConfig  -- ^ Configuration for a single node. Of particular
                    --   importance is the port (nodes will be spawned
                    --   in the range [port+1, port+range]).
         -> Chan NormalSignal
                    -- ^ Local direct connection to one node (taking
                    --   turns). 'Chan' instead of 'TQueue' because of the
                    --   lack of fairness in STM.
         -> IOQueue -- ^ Channel to output thread
         -> MVar () -- ^ Termination lock. If the MVar is filled, the next
                    --   node (due to fairness) is killed and restarted,
                    --   see 'janitor'.
         -> IO ()
nodePool n config ldc output terminate =
      void . forkIO . forM_ [1..n] $ \portOffset -> do
            let port    = _serverPort config + fromIntegral portOffset
                config' = config { _serverPort = port }
            forkIO (janitor config' ldc output terminate)
            delay (_mediumTickRate config)



-- | Spawn a new node, restart it should it crash, and listen for signals
--   sent to it.
--
--   The termination parameter is useful to make the pool replace old nodes, for
--   example the initial bootstrap nodes are very interconnected, which is not
--   desirable. Restarting these nodes when there's an actual network leads to
--   more natural neighbourships.
janitor :: NodeConfig
        -> Chan NormalSignal -- ^ Local direct connection
        -> IOQueue           -- ^ Channel to output thread
        -> MVar ()           -- ^ Termination MVar. If filled, a node is
                             --   terminated (and replaced by a new one by the
                             --   janitor). Also see 'terminationWatch'.
        -> IO ()
janitor config fromPool output terminate = yellCatchall . forever $ do
      toNode <- spawn (P.Bounded (_maxChanSize config))
      (`catches` handlers) $
            withAsync (startNode (Just toNode) output config) $ \node ->
             withAsync (fromPool `pipeTo` toNode)             $ \_chanPipe ->
              withAsync (terminationWatch terminate node)     $ \_terminator ->
               wait node

      where
            handlers = [ Handler asyncException ]
            asyncException ThreadKilled = return ()
            asyncException e = throwIO e

            yellCatchall = handle (\(SomeException e) ->
                             yell 41 ("Janitor crashed! Exception: " ++ show e))



-- | Send everything from one channel to the other
pipeTo :: Chan  NormalSignal -- ^ From
       -> PChan NormalSignal -- ^ To
       -> IO ()
pipeTo input output = runEffect (fromChan input >-> P.toOutput toChan) where
      fromChan chan = forever (liftIO (readChan chan) >>= yield)
      toChan        = _pOutput output



-- | Terminate a thread when the MVar is filled
terminationWatch :: MVar () -> Async () -> IO ()
terminationWatch mVar thread = takeMVar mVar >> cancel thread
