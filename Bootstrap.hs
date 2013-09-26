-- | Provides functions for the client to connect to a Bootstrap server in order
--   to make the initial connection to the network.

{-# LANGUAGE LambdaCase #-}

module Bootstrap (
      bootstrap
) where



import Network
import Control.Exception
import System.IO

import Types
import Utilities (request', connectToNode)





-- | Send out a BootstrapRequest to a bootstrap server and handle the response.
bootstrap :: Config -> PortNumber -> IO HostName -- TODO: Make it an Either Error Hostname so the problem can be printed
bootstrap config port = do

      -- TODO: Add a function to find a random bootstrap nodes

      -- Send out signal to the bootstrap node
      bNode <- getBootstrapServer

      -- Don't recurse directly in case of failure so that bracket can close
      -- the handle properly. Instead, bind the result to an identifier, and
      -- check it after the bracketing.
      result <- bracket (connectToNode  bNode) hClose $ \h -> do

            -- See note [Why send port?]
            response <- request' h (BootstrapRequest port)
            return $ case response of
                  (YourHostIs host) -> Just host
                  _                 -> Nothing -- TODO: Error message "Bootstrap reply rubbish"
                     -- TODO: Handle timeouts, yell if pattern mismatch



      maybe (bootstrap config port) return result

-- [Why send port?]
--
-- To issue EdgeRequests in the name of the node to be bootstrapped, the server
-- has to be aware of a return address. While it can deduce the hostname from
-- the incoming connection, the port of the new node's server is unknown.
-- for that reason, the node has to provide it explicitly.


-- | Finds the address of a suitable bootstrap server.
getBootstrapServer :: IO To
getBootstrapServer = return . To $ Node "localhost" 20000
-- TODO: Make bootstrap server selection a little more complex :-)