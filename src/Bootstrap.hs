-- | Provides functions for the client to connect to a Bootstrap server in order
--   to make the initial connection to the network.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Bootstrap (
      bootstrap
) where



import Control.Concurrent
import Control.Exception
import System.IO.Error
import GHC.IO.Exception (ioe_description)
import Data.Typeable
import Control.Monad
import Text.Printf

import Types
import Utilities



data BootstrapError = BadResponse
                    | NoResponse
      deriving (Show, Typeable)

instance Exception BootstrapError



-- | Send out a 'BootstrapRequest' to a bootstrap server and handle the
--   response.
bootstrap :: Config
          -> To -- Own address so other nodes can connect
          -> IO ()
bootstrap config self =
      do putStrLn "Starting bootstrap"
         go
         putStrLn "Bootstrap finished"

      where handleMulti action = do catches action [ badResponseH
                                                   , noResponseH
                                                   , ioErrorH
                                                   ]
                                    threadDelay (_longTickRate config)
                                    go

            badResponseH = Handler $ \BadResponse -> do
                  putStrLn "Bad response from bootstrap server. Probably a bug."

            noResponseH = Handler $ \NoResponse -> do
                  putStrLn "No response from bootstrap server. Probably a bug."

            ioErrorH = Handler $ \e -> do
                  let _ = e :: IOException
                  printf "Cound not connect to bootstrap server (%s).\
                         \ Is it online?\n"
                         (ioe_description e)


            go = handleMulti $ forever $ do
                  let bsServer = getBootstrapServer config
                  connectToNode bsServer $ \(s, _) -> do
                        request s (BootstrapRequest self) >>= \case
                              Just OK -> return ()
                              Just _  -> throwIO BadResponse
                              Nothing -> throwIO NoResponse




-- | Find the address of a suitable bootstrap server.
getBootstrapServer :: Config -> To
getBootstrapServer = head . _bootstrapServers
-- TODO: Make bootstrap server selection a little more complex :-)