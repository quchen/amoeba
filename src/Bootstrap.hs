-- | Provides functions for the client to connect to a Bootstrap server in order
--   to make the initial connection to the network.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Bootstrap (bootstrap) where



import Control.Exception
import Control.Concurrent.Async
import GHC.IO.Exception (ioe_description)
import Data.Typeable
import Text.Printf
import System.Random
import Control.Monad
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
bootstrap config self = do putStrLn "Starting bootstrap"
                           mapConcurrently (const dispatch)
                                           [1 .. _maxNeighbours config]
                           putStrLn "Bootstrap finished"

      where

      dispatch = do

            bsServer <- getBootstrapServer config

            let handleMulti action = catches action [ bootstrapErrorH
                                                    , ioErrorH
                                                    ]

                retryBootstrap = delay (_longTickRate config) >> dispatch

                bootstrapErrorH = Handler $ \case
                      BadResponse -> do
                            printf "Bad response from bootstrap server %s.\
                                         \ This is a bug if the server is\
                                         \ actually a bootstrap server.\n"
                                   (showBss bsServer)
                            retryBootstrap
                      NoResponse -> do
                            printf "No response from bootstrap server %s\
                                         \ (altough it is online).\
                                         \ This is a bug if the server is\
                                         \ actually a bootstrap server.\n"
                                   (showBss bsServer)
                            retryBootstrap

                ioErrorH = Handler $ \e -> do
                      let _ = e :: IOException
                      printf "Could not connect to bootstrap server %s\
                                   \ (%s). Is it online?\n"
                             (showBss bsServer)
                             (ioe_description e)
                      retryBootstrap

                showBss :: To -> String
                showBss (To (Node h p)) = printf "%s:%d" h p

            handleMulti . connectToNode bsServer $ \(s, _) -> do
                  request s (BootstrapRequest self) >>= \case
                        Just OK -> return ()
                        Just _  -> throwIO BadResponse
                        Nothing -> throwIO NoResponse



-- | Find the address of a suitable bootstrap server.
-- TODO: Make bootstrap server selection a little more complex :-)
getBootstrapServer :: NodeConfig -> IO To
getBootstrapServer config = randomSetElement (_bootstrapServers config)
-- Fallback entry: return (To (Node "127.0.0.1" 20000))



randomSetElement :: Set.Set a -> IO a
randomSetElement set = do
      when (Set.null set) (error "No bootstrap servers known")  -- TODO: This is beyond awful
      let size = Set.size set
      i <- randomRIO (0, size-1)
      return (elemAt i set)



-- | Equivalent to "Set.elemAt", which is only present in later versions of
--   the containers package.
elemAt :: Int -> Set.Set a -> a
elemAt i set = Set.toList set !! i