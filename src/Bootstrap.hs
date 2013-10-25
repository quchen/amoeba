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
import Control.Monad

import Types
import Utilities



data BadBootstrapResponse = BadBootstrapResponse
      deriving (Show, Typeable)

instance Exception BadBootstrapResponse



-- | Send out a 'BootstrapRequest' to a bootstrap server and handle the
--   response.
bootstrap :: Config
          -> To -- Own address so other nodes can connect
          -> IO ()
bootstrap config self =
      do putStrLn "Starting bootstrap"
         go
         putStrLn "Bootstrap finished"

      where go = do
                  let bsServer = getBootstrapServer config
                  success <- connectToNode bsServer $ \(s, _) -> do
                        request s (BootstrapRequest self) >>= \case
                              Just OK -> return True
                              Just _  -> do
                                    putStrLn "Bad bootstrap server response.\
                                             \ Probably a bug."
                                    return False
                              Nothing -> return False
                  unless success $ do
                        putStrLn $ "Bootstrap failed. This is likely a bug."
                        threadDelay (_mediumTickRate config)
                        go




-- | Find the address of a suitable bootstrap server.
getBootstrapServer :: Config -> To
getBootstrapServer = head . _bootstrapServers
-- TODO: Make bootstrap server selection a little more complex :-)