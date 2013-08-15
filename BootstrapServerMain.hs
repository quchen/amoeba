-- | The bootstrap server will be the first contact when a new client wants to
--   join the network. Bootstrap servers take no part in the network otherwise.


{-# LANGUAGE LambdaCase #-}


-- TODO: Sub-network of bootstrapping servers?
-- TODO: Make clients remember registered bootstrap servers, and only accept
--       non-upstream signals if they come from those

module Main (main) where


import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Functor
import           Data.Set (Set)
import           Network
import           System.IO
import           Control.Exception
import           System.Random
import           Data.Set (Set)
import qualified Data.Set as Set

import Utilities (receive, send, connectToNode)
import Types hiding (Environment)



data BSEnv = BSEnv { _knownNodes :: TVar (Set Node)
                   , _rng        :: TVar StdGen-- RNG or infinite stream of random numbers?
                   }

main :: IO ()
main = do


      putStrLn "Bootstrap server. Keep in mind that this is a *very* crude draft."

      -- Initialization
      putStrLn "Initializing"
      known <- newTVarIO Set.empty
      rng   <- newTVarIO =<< newStdGen
      let env = BSEnv known rng

      putStrLn "Forking housekeeping"
      async $ housekeeping env

      socket <- listenOn $ PortNumber 20000
      putStrLn "Starting server loop"
      bootstrapServerLoop env socket





bootstrapServerLoop :: BSEnv -> Socket -> IO ()
bootstrapServerLoop env socket = forever $ do

      bracket (accept socket)
              (\(h, _ , _) -> hClose h) $
              \(h, host , _) -> do

      putStrLn "Client connected"

      async $ handleRequest env h host


-- | Filter different special signals, process only BootstrapRequest
handleRequest :: BSEnv -> Handle -> HostName -> IO ()
handleRequest env h host = do

      receive h >>= \case
            BootstrapRequest port -> handleValidRequest env h host port
            BootstrapHelper  {}   -> return () -- illegal action
            YourHostIs       {}   -> return () -- illegal action


-- | Handles a valid request. Only called if the initial request is accepted.
handleValidRequest :: BSEnv -> Handle -> HostName -> PortNumber -> IO ()
handleValidRequest env h host port = do

      let beneficiary = Node host port

      -- If it's a bootstrap request, send a couple of edge requests to nodes
      sendEdgeRequest env beneficiary Incoming
      sendEdgeRequest env beneficiary Outgoing

      -- If everything's fine, add client to list of known nodes.
      atomically $ modifyTVar (_knownNodes env) $ Set.insert beneficiary
      send h (YourHostIs host)


-- TODO.
housekeeping :: BSEnv -> IO ()
housekeeping env = do

      -- Periodically ask known nodes whether they're still running, delete
      -- dead ones

      -- Ask other bootstrap servers for their nodes and other bootstrap
      -- servers?

      return ()



-- | Sends a single request to a random known node
sendEdgeRequest :: BSEnv -> Node -> Direction -> IO ()
sendEdgeRequest env beneficiary dir = do

      -- p = denial probability
      let p = 0.5 -- TODO: read this from some config
          signal = Special . BootstrapHelper $ EdgeRequest beneficiary (EdgeData dir (Right p))

      targetNode <- atomically $ randomNode env

      bracket (connectToNode targetNode) hClose $ \h -> do
            send h signal



-- | Gets a random node out of the list of known ones
randomNode :: BSEnv -> STM Node
randomNode env = do
      nodes <- readTVar (_knownNodes env)
      gen <- readTVar (_rng env)
      let (i, gen') = randomR (0, Set.size nodes) gen
      writeTVar (_rng env) gen'
      return $ Set.toList nodes !! i
            -- ^ Set.elemAt unavailable in Containers 0.5.0.0 :-(