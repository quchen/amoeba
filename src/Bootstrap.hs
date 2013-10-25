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
import Utilities



data BadBootstrapResponse = BadBootstrapResponse
      deriving (Show, Typeable)

instance Exception BadBootstrapResponse



-- | Send out a 'BootstrapRequest' to a bootstrap server and handle the
--   response.
bootstrap :: Config
          -> Int         -- ^ Own server port
          -> IO HostName -- ^ Own hostname
bootstrap config port = do

      bNode <- getBootstrapServer

      result <- connectToNode bNode $ \(socket, _) -> do
            request socket (BootstrapRequest port) >>= \case
                  Just (YourHostIs host) -> return (Just host)
                  Just _ -> putStrLn "Bad bootstrap server response. Bug!" >> return Nothing
                  Nothing -> return Nothing

      case result of
            Just host -> return host
            Nothing -> do
                  putStrLn $ "Bootstrap failed. This is likely a bug."
                  threadDelay (_mediumTickRate config)
                  bootstrap config port





-- | Find the address of a suitable bootstrap server.
getBootstrapServer :: IO To
getBootstrapServer = return . To $ Node "localhost" 20000
-- TODO: Make bootstrap server selection a little more complex :-)