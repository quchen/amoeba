-- | Maintains a number of completely independent nodes, and allows sending
--   signals to them easily.
--
--   Useful to add certain services to the network without requiring them to
--   manage their own connections. For example, a bootstrap server can use a
--   node pool to always keep up a certain amount of trusted nodes in the
--   network that it can use to help other nodes make an initial connection.

module NodePool (startNodePool) where



import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Set as Set
import Data.Functor
import Data.Word
import Network
import System.IO

import Node
import Types
import Utilities



-- | Starts a node pool of a certain size, and provides a channel to
--   communitcate with (random nodes in) it
startNodePool :: Word    -- ^ Number of nodes in the pool (also the port range)
              -> Config  -- ^ Configuration for a single node. Of particular
                         --   importance are the port (nodes will be spawned
                         --   in the range [port+1, port+range]).
              -> Chan NormalSignal
                         -- ^ Local direct connection to one node (taking
                         --   turns). 'Chan' instead of 'TQueue' because of the
                         --   lack of fairness in STM.
              -> MVar () -- ^ Termination lock. If the MVar is filled, the next
                         --   node (due to fairness) is killed and restarted,
                         --   see 'janitor'.
              -> IO ()
startNodePool n config ldc terminate = forM_ [1..n] $ \portOffset ->
      let port = _serverPort config + fromIntegral portOffset
          config' = config { _serverPort = port }
      in  async $ janitor config' ldc terminate



-- | Spawns a new node, restarts it should it crash, and listens for signals
--   sent to it.
--
--   The termination parameter is useful to make the pool replace old nodes, for
--   example the initial bootstrap nodes are very interconnected, which is not
--   desirable. Restarting these nodes when there's an actual network leads to
--   more natural neighbourships.
janitor :: Config            -- ^ Node configuration
        -> Chan NormalSignal -- ^ Local direct connection
        -> MVar ()           -- ^ Termination MVar. If filled, a node is
                             --   terminated (and replaced by a new one by the
                             --   janitor). Also see 'terminationWatch'.
        -> IO ()
janitor config fromPool terminate = (handle $ \(SomeException e) -> yell 41 ("Janitor crashed! Exception: " ++ show e)) $
  forever $ do
      toNode <- newTBQueueIO (_maxChanSize config)
      let handlers = [ Handler $ \ThreadKilled -> return ()
                     ]
      (`catches` handlers) $
            bracket (startNode (Just toNode) config) cancel $ \node ->
             withAsync (fromPool `pipeTo` toNode) $ \_signal ->
              withAsync (terminationWatch terminate node) $ \_term ->
               withAsync (statusReport config) $ \_status ->
                wait node


-- | Periodically say hello for DEBUG
statusReport :: Config -> IO ()
statusReport config = forever $ do
      threadDelay (10^7)
      yell 35 $ "Janitor for " ++ show (_serverPort config) ++ " reporting in"


-- | Pipes everything from one channel to the other
pipeTo :: Chan NormalSignal -> TBQueue NormalSignal -> IO ()
pipeTo incoming outgoing = forever $ do
      signal <- readChan incoming
      atomically $ do
            --assertNotFull outgoing
            writeTBQueue outgoing signal

yellAndRethrow msg = handle handler
      where handler :: SomeException -> IO ()
            handler e = yell 41 msg >> throw e


-- | Terminate a thread when an MVar is filled
terminationWatch :: MVar () -> Async () -> IO ()
terminationWatch mVar thread = takeMVar mVar >> cancel thread
