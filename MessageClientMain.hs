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

import Utilities
import Types



data BSEnv = BSEnv { _knownNodes :: TVar (Set To)
                   , _rng        :: TVar StdGen-- RNG or infinite stream of random numbers?
                   }

main :: IO ()
main = do

      putStrLn "Message sending server."
      putStrLn "Sends a message to localhost:21001."

      getMessage

getMessage = forever $ do
      message <- getLine
      let entryNode = To $ Node "localhost" 21001
      bracket (connectToNode entryNode) hClose $ \h -> do
            send' h . Normal . Flood . TextMessage $ message
            receive' h :: IO ServerResponse