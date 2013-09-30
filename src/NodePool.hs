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
import Data.Word
import Network
import System.IO

import Node
import Types
import Utilities



-- | Starts a node pool of a certain size, and provides a channel to
--   communitcate with (random nodes in) it
startNodePool :: Word   -- ^ Number of nodes in the pool (also the port range)
              -> Config -- ^ Configuration for a single node. Of particular
                        --   importance are the port (nodes will be spawned
                        --   in the range [port+1, port+range]).
              -> IO (Chan NormalSignal) -- ^ Communication channel to send signals
startNodePool n config = do
      chan <- newChan
      forM_ [1..n] $ \portOffset ->
            let port = _serverPort config + fromIntegral portOffset
            in  do async $ janitor port config chan
      return chan



-- | Spawns a new node, restarts it should it crash, and listens for signals
--   sent to it.
janitor :: PortNumber -> Config -> Chan NormalSignal -> IO ()
janitor port config fromPool = forever $ do
      toNode <- newTBQueueIO (_maxChanSize config)
      let nodeConfig = config { _serverPort = port }
      (_, nodeThread) <- delay >> startNode (Just toNode) nodeConfig
      (`finally` cancel nodeThread) $ do
            withAsync (signalLoop fromPool toNode) $ \_signal -> wait nodeThread
      -- TODO: catch



-- | Gives the bootstrap server some time before its own node pool tries to
--   concat it. FIXME: this should not be necessary by design
delay :: IO ()
delay = threadDelay (10^6)



-- | Pipes everything from one channel to the
signalLoop :: Chan NormalSignal -> TBQueue NormalSignal -> IO ()
signalLoop incoming outgoing =
      forever $ readChan incoming >>= atomically . writeTBQueue outgoing