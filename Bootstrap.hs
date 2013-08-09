-- | Provides functions for the client to connect to a Bootstrap server in order
--   to make the initial connection to the network.

{-# LANGUAGE LambdaCase #-}

module Bootstrap (
      sendBootstrapRequest
) where



import Data.Binary
import GHC.Generics (Generic)
import Network
import Control.Exception
import System.IO
import Data.Functor ((<$>))

import Types
import Utilities (send, receive, connectToNode)





-- Algorithm idea: Create a special Bootstrap signal. When receiving such a
-- signal, the receiving node sends out N Request/Announce signals, with the
-- origin set to the issuing node.
--
-- To combat abuse, the client should hold a timestamped version of the command,
-- and only accept a new one after X seconds.

-- | Send out
sendBootstrapRequest :: Config -> PortNumber -> IO HostName -- TODO: Make it an Either Error Hostname so the problem can be printed
sendBootstrapRequest config port = do

      -- TODO: Add a function to find a random bootstrap nodes

      -- Send out signal to the bootstrap node
      bNode <- getBootstrapServer

      -- Don't recurse directly in case of failure so that bracket can close
      -- the handle properly. Instead, bind the result to an identifier, and
      -- check it after the bracketing.
      result <- bracket (connectToNode  bNode) hClose $ \h -> do
            send h (BootstrapRequest port)
            (<$> receive h) $ \case
                  (YourHostIs host) -> Just host
                  _                 -> Nothing -- TODO: Error message "Bootstrap reply rubbish"
                     -- TODO: Handle timeouts, yell if pattern mismatch

      maybe (sendBootstrapRequest config port) return result


-- | Finds the address of a suitable bootstrap server.
getBootstrapServer :: IO Node
getBootstrapServer = undefined -- TODO: Implement bootstrap server discovery