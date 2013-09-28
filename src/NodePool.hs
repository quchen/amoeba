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
startNodePool :: Word   -- ^ Number of nodes in the pool
              -> Config -- ^ Configuration for a single node. Of particular
                        --   importance are the port (which will be the start of
                        --   the port range used) and the secret (to send
                        --   signals to the nodes in the pool).
              -> IO (Chan Signal) -- ^ Communication channel to send signals
startNodePool n config = do

      chan <- newChan

      forM_ [1..n] $ \portOffset ->
            let port = _serverPort config + fromIntegral portOffset - 1
            in  janitor port config chan

      return chan


janitor :: PortNumber -> Config -> Chan Signal -> IO ()
janitor port config chan = do
      withAsync (signalLoop port chan) $ \_a1 ->
       withAsync (janitorLoop config) $ \_a2 ->
        return ()


-- | Reincarnates dead nodes until the end of time.
janitorLoop :: Config -> IO ()
janitorLoop config =
      forever $ withAsync (startNode config) wait


-- | Reads signals from the channel and sends them to the worker
signalLoop :: PortNumber -> Chan Signal -> IO ()
signalLoop port chan = forever $ do
      signal <- readChan chan
      let process = bracket (connectTo "localhost" (PortNumber port)) hClose $ \h -> do
                          send' h signal
                          void (receive' h :: IO ServerResponse)
                          -- TODO: Debug to make sure the signals are actually relayed
      withAsync process $ \_a -> return ()