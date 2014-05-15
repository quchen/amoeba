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
import qualified Data.Foldable as F

import           Pipes
import qualified Pipes.Concurrent as P

import           Control.Lens.Operators
-- import qualified Control.Lens as L
import qualified Types.Lens as L

import           Node
import           Types
import           Utilities
import           Utilities.IOQueue



-- | Start a node pool of a certain size, and provide a channel to
--   communitcate with (random nodes in) it.
nodePool :: Int     -- ^ Number of nodes in the pool (also the port range)
         -> NodeConfig  -- ^ Configuration for a single node. Of particular
                    --   importance is the port (nodes will be spawned
                    --   in the range [port+1, port+range]).
         -> Chan NormalSignal
                    -- ^ Local direct connection to one node (taking
                    --   turns). 'Chan' instead of 'TQueue' because of the
                    --   lack of fairness in STM.
         -> IOQueue -- ^ Channel to output thread
         -> Maybe TerminationTrigger
                    -- ^ If the 'MVar' contained in the 'TerminationTrigger' is
                    --   filled, a node is killed (and a new one is started by
                    --   its janitor).
         -> IO ()
nodePool n config ldc output m'terminate =
      F.for_ [1..n] $ \portOffset -> do
             -- Give nodes in the pool consecutive numbers, starting with
             -- <config port> + 1
             _ <- forkIO (janitor (config & L.serverPort +~ portOffset)
                                  ldc
                                  output
                                  m'terminate)
             delay (config ^. L.mediumTickRate)



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
        -> Maybe TerminationTrigger
        -> IO ()
janitor config fromPool output m'terminate = yellCatchall . forever $ do
      toNode <- spawn (P.Bounded (config ^. L.maxChanSize))
      (`catches` handlers) $
       withAsync (startNode (Just toNode) output config) $ \node ->
        withAsync (fromPool `pipeTo` toNode) $ \_chanPipe ->
         case m'terminate of
               Just t  -> withAsync (terminationWatch t node) (\_ -> wait node)
               Nothing -> wait node

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
      toChan        = output ^. L.pOutput



-- | Terminate a thread when the MVar is filled, and block until this happens.
terminationWatch :: TerminationTrigger -> Async () -> IO ()
terminationWatch (TerminationTrigger mVar) thread = do takeMVar mVar
                                                       cancel thread
