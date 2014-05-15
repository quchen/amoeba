-- | Provides functions for the client to connect to a Bootstrap server in order
--   to make the initial connection to the network.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Bootstrap (bootstrap) where



import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.Monoid
import qualified Data.Set     as Set
import           Data.Typeable
import           GHC.IO.Exception (ioe_description)
import           System.Random

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Control.Lens.Operators
import qualified Control.Lens as L
import qualified Types.Lens   as L

import           Types
import           Utilities



data BootstrapError = BadResponse
                    | NoResponse
      deriving (Typeable)

instance Show BootstrapError where
      show BadResponse = "Bad response"
      show NoResponse  = "No response"

instance Exception BootstrapError



-- | Send '_maxNeighbours' 'BootstrapRequest's to bootstrap servers, and repeat
--   the process until each neighbour has issued one successful one.
bootstrap :: NodeConfig
          -> To -- Own address so other nodes can connect
          -> IO ()
bootstrap config self = do T.putStrLn "Starting bootstrap"
                           mapConcurrently (const dispatch) [1..maxN]
                           T.putStrLn "Bootstrap finished"

      where

      maxN = config ^. L.maxNeighbours

      retryBootstrap = delay (config ^. L.longTickRate) >> dispatch

      dispatch = do

            bsServer <- getBootstrapServer config

            let catchMulti action = catches action [ bootstrapErrorH
                                                   , ioExceptionH
                                                   ]

                bootstrapErrorH = Handler $ \case
                      BadResponse -> do
                            T.putStrLn (T.unwords
                                  [ "Bad response from bootstrap server"
                                  , showBss <> "."
                                  , "This is a bug This is a bug if the server"
                                  , "is actually a bootstrap server."
                                  ])
                            retryBootstrap
                      NoResponse -> do
                            T.putStrLn (T.unwords
                                  [ "No response from bootstrap server"
                                  , showBss
                                  , "(although it is online)."
                                  , "This is a bug if the server actually is"
                                  , "a bootstrap server."
                                  ])
                            retryBootstrap

                ioExceptionH = Handler $ \(e :: IOException) -> do
                      T.putStrLn (T.unwords
                            [ "Could not connect to bootstrap server"
                            , showBss
                            , "(" <> T.pack (ioe_description e) <> ")."
                            , "Is it online?"
                            ])
                      retryBootstrap

                showBss :: T.Text
                showBss = let To (Node h p) = bsServer
                          in  T.pack h <> ":" <> showT p

            catchMulti (connectToNode bsServer (\(s, _) ->
                  request s (BootstrapRequest self) >>= \case
                        Just OK -> return ()
                        Just _  -> throwIO BadResponse
                        Nothing -> throwIO NoResponse))



-- | Find the address of a suitable bootstrap server.
-- TODO: Make bootstrap server selection a little more complex :-)
getBootstrapServer :: NodeConfig -> IO To
getBootstrapServer = L.view (L.bootstrapServers . L.to randomSetElement)
-- Fallback entry: return (To (Node "127.0.0.1" 20000))



randomSetElement :: Set.Set a -> IO a
randomSetElement set = do
      when (Set.null set) (error "No bootstrap servers known")  -- TODO: This is beyond awful
      fmap (\i -> Set.elemAt i set)
           (randomRIO (0, Set.size set - 1))
