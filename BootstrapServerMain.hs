-- | The bootstrap server will be the first contact when a new client wants to
--   join the network. Bootstrap servers take no part in the network otherwise.


{-# LANGUAGE LambdaCase #-}


-- TODO: Sub-network of bootstrapping servers?
-- TODO: Make clients remember registered bootstrap servers, and only accept
--       non-upstream signals if they come from those

module Main (main) where


import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as Set

import Utilities (receive, send)
import Types (SpecialSignal(..))



data BSEnv = BSEnv { _knownNodes :: TVar (Set Node)
                   , _rng        :: -- RNG or infinite stream of random numbers?
                   }

main :: IO ()
main = do
          -- Initialization
          known <- newTVarIO Set.empty
          rng <- newStdGen

          let env = BSEnv known rng

          -- Launch processes
          async $ housekeeping env
          bootstrapServerLoop env


-- TODO: Setup socket

bootstrapServerLoop = forever $ do

      bracket (accept socket)
              (\(h, _ , _) -> hClose h) $
              \(h, host , _) -> do

      async $ handleRequest h host


-- | Filter different special signals, process only BootstrapRequest
handleRequest h host = do

      receive h >>= \case
            BootstrapRequest port -> handleValidRequest h host port
            BootstrapHelper  {}   -> illegal
            YourHostIs       {}   -> illegal


-- | Handles a valid request. Only called if the initial request is accepted.
handleValidRequest h host port

      let node = Node host port

      -- If it's a bootstrap request, send a couple of edge requests to nodes

      -- If everything's fine, add client to list of known nodes.
      atomically $ modifyTVar (_knownNodes env) $ Set.insert node
      send h (YourHostIs host)



housekeeping = do

      -- Periodically ask known nodes whether they're still running, delete
      -- dead ones

      -- Ask other bootstrap servers for their nodes and other bootstrap
      -- servers?


-- | Sends a single request to a random known node
sendEdgeRequest beneficiary dir

      -- p = denial probability
      let signal = Special . BootstrapHelper $ EdgeRequest beneficiary (EdgeData dir (Right p))

      send (toNode) $ signal


