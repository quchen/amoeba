-- | Provides functions for the client to connect to a Bootstrap server in order
--   to make the initial connection to the network.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Bootstrap (bootstrap) where



import Control.Exception
import GHC.IO.Exception (ioe_description)
import Data.Typeable
import Text.Printf
import qualified Data.Set as Set

import Types
import Utilities



data BootstrapError = BadResponse
                    | NoResponse
      deriving (Show, Typeable)

instance Exception BootstrapError



-- | Send out a 'BootstrapRequest' to a bootstrap server and handle the
--   response.
bootstrap :: NodeConfig
          -> To -- Own address so other nodes can connect
          -> IO ()
bootstrap config self =
      do putStrLn "Starting bootstrap"
         go
         putStrLn "Bootstrap finished"

      where handleMulti action = do catches action [ bootstrapErrorH
                                                   , ioErrorH
                                                   ]

            retryBootstrap = delay (_longTickRate config) >> go

            bootstrapErrorH = Handler $ \case
                  BadResponse -> do
                        putStrLn "Bad response from bootstrap server. Probably\
                                 \ a bug."
                        retryBootstrap
                  NoResponse -> do
                        putStrLn "No response from bootstrap server (altough it\
                                 \ is online). Probably a bug."
                        retryBootstrap

            ioErrorH = Handler $ \e -> do
                  let _ = e :: IOException
                  printf "Cound not connect to bootstrap server (%s).\
                         \ Is it online?\n"
                         (ioe_description e)
                  retryBootstrap

            go = handleMulti $ do
                  let bsServer = getBootstrapServer config
                  connectToNode bsServer $ \(s, _) -> do
                        request s (BootstrapRequest self) >>= \case
                              Just OK -> return ()
                              Just _  -> throwIO BadResponse
                              Nothing -> throwIO NoResponse




-- | Find the address of a suitable bootstrap server.
-- TODO: Make bootstrap server selection a little more complex :-)
getBootstrapServer :: NodeConfig -> To
getBootstrapServer = const (To (Node "127.0.0.1" 20000))
