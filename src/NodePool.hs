-- | Maintains a number of completely independent nodes, and allows sending
--   signals to them easily.
--
--   Useful to add certain services to the network without requiring them to
--   manage their own connections. For example, a bootstrap server can use a
--   node pool to always keep up a certain amount of trusted nodes in the
--   network that it can use to help other nodes make an initial connection.

module NodePool (nodePool) where



import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Word

import Pipes
import qualified Pipes.Concurrent as P

import Node
import Types
import Utilities



-- | Starts a node pool of a certain size, and provides a channel to
--   communitcate with (random nodes in) it
nodePool :: Int     -- ^ Number of nodes in the pool (also the port range)
         -> Config  -- ^ Configuration for a single node. Of particular
                    --   importance are the port (nodes will be spawned
                    --   in the range [port+1, port+range]).
         -> Chan NormalSignal
                    -- ^ Local direct connection to one node (taking
                    --   turns). 'Chan' instead of 'TQueue' because of the
                    --   lack of fairness in STM.
         -> TBQueue (IO ()) -- ^ Channel to output thread
         -> MVar () -- ^ Termination lock. If the MVar is filled, the next
                    --   node (due to fairness) is killed and restarted,
                    --   see 'janitor'.
         -> IO ()
nodePool n config ldc output terminate = forM_ [1..n] $ \portOffset ->
      let port = _serverPort config + fromIntegral portOffset
          config' = config { _serverPort = port }
      in  forkIO $ janitor config' ldc output terminate



-- | Spawns a new node, restarts it should it crash, and listens for signals
--   sent to it.
--
--   The termination parameter is useful to make the pool replace old nodes, for
--   example the initial bootstrap nodes are very interconnected, which is not
--   desirable. Restarting these nodes when there's an actual network leads to
--   more natural neighbourships.
janitor :: Config            -- ^ Node configuration
        -> Chan NormalSignal -- ^ Local direct connection
        -> TBQueue (IO ())   -- ^ Channel to output thread
        -> MVar ()           -- ^ Termination MVar. If filled, a node is
                             --   terminated (and replaced by a new one by the
                             --   janitor). Also see 'terminationWatch'.
        -> IO ()
janitor config fromPool output terminate = handle (\(SomeException e) -> yell 41 ("Janitor crashed! Exception: " ++ show e)) $
  forever $ do
      toNode <- spawn (P.Bounded $ _maxChanSize config)
      let handlers = [ Handler $ \ThreadKilled -> return ()
                     ]
      (`catches` handlers) $
            withAsync (startNode (Just toNode) output config) $ \node ->
             withAsync (fromPool `pipeTo` toNode) $ \_ ->
              withAsync (terminationWatch terminate node) $ \_ ->
               withAsync (statusReport config) $ \_ ->
                wait node



-- | Periodically say hello for DEBUG
statusReport :: Config -> IO ()
statusReport config = forever $ do
      delay (10^7)
      yell 35 $ "Janitor for " ++ show (_serverPort config) ++ " reporting in"



-- | Send everything from one channel to the other
pipeTo :: Chan NormalSignal -> PChan NormalSignal -> IO ()
pipeTo input output =

      runEffect $ fromChan input >-> P.toOutput (_pOutput output)

      where fromChan chan = forever $ do
                  signal <- liftIO $ readChan chan
                  yield signal



-- | Terminate a thread when the MVar is filled
terminationWatch :: MVar () -> Async () -> IO ()
terminationWatch mVar thread = takeMVar mVar >> cancel thread
