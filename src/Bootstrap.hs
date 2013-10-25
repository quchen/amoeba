-- | Provides functions for the client to connect to a Bootstrap server in order
--   to make the initial connection to the network.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Bootstrap (
      bootstrap
) where



import Control.Concurrent
import Network
import Control.Exception
import System.IO
import Data.Either
import Data.Typeable

import Types
import Utilities (request', connectToNode)



data BadBootstrapResponse = BadBootstrapResponse
      deriving (Show, Typeable)

instance Exception BadBootstrapResponse



-- | Send out a BootstrapRequest to a bootstrap server and handle the response.
bootstrap :: Config -> PortNumber -> IO HostName -- TODO: Make it an Either Error Hostname so the problem can be printed
bootstrap config port = do

      -- Send out signal to the bootstrap node
      let bsServer = getBootstrapServer config

      -- Don't recurse directly in case of failure so that bracket can close
      -- the handle properly. Instead, bind the result to an identifier, and
      -- check it after the bracketing.
      --let try' :: IO a -> IO (Either BadBootstrapResponse a)
      --    try' = try
      result <- try $ bracket (connectToNode bsServer) hClose $ \h -> do

            -- See note [Why send port?]
            request' h (BootstrapRequest port) >>= \case
                  (YourHostIs host) -> return host
                  _                 -> throwIO BadBootstrapResponse

      case result of
            Left (SomeException e) -> do
                  putStrLn $ "Bootstrap failed (" ++ show e ++ ")\
                             \  (If the bootstrap server is valid,\
                             \ this is likely a bug.)"
                  threadDelay (_mediumTickRate config)
                  bootstrap config port
            Right r -> return r

-- [Why send port?]
--
-- To issue EdgeRequests in the name of the node to be bootstrapped, the server
-- has to be aware of a return address. While it can deduce the hostname from
-- the incoming connection, the port of the new node's server is unknown.
-- for that reason, the node has to provide it explicitly.



-- | Find the address of a suitable bootstrap server.
getBootstrapServer :: Config -> To
getBootstrapServer = head . _bootstrapServers
-- TODO: Make bootstrap server selection a little more complex :-)