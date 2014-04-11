-- | Verifies whether the invariants of a configuration are satisfied, in order
--   to avoid nonsense program behaviour because of human error.
--
--   This module is intended to be used qualified, e.g. as \"Verify\" to
--   make nice names such as \"Verify.nodeArgs\".

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Config.Verify (
        node
      , multi
      , bootstrap
      , drawing
) where


import Data.List
import Control.Exception
import Data.Typeable
import Control.Monad

import qualified Control.Lens as L
import qualified Types.Lens as L

import Types.Config




data ConfigError = PortRange
                 | TickRates
                 | PoolTimeout
                 | PoolSize
                 | RestartEvery
      deriving (Show, Typeable)

instance Exception ConfigError



-- | Verify integrity of a 'NodeConfig'.
node :: NodeConfig -> IO ()
node config = sequence_ [portRange, tickRates, poolTimeout, poolSize]


      where
            -- Is the server port in range?
            portRange = unless (p >= 0 && p < 2^16) (throwIO PortRange)
                  where p = _serverPort config

            -- Are the tickrates in the right order?
            -- (small <= medium <= long)
            tickRates = when (rates /= sort rates) (throwIO TickRates)
                  where rates = [ _shortTickRate  config
                                , _mediumTickRate config
                                , _longTickRate   config
                                ]

            -- Timeouts must be longer than the long tickrate (and should be so
            -- by a factor of about 3, nyquist etc.)
            poolTimeout = when (ltr > tout) (throwIO PoolTimeout)
                  where ltr  = fromIntegral (_longTickRate config) / 10^6
                        tout = _poolTimeout config

            -- minimum <= maximum neighbours
            poolSize = when (minN > maxN) (throwIO PoolSize)
                  where minN = _minNeighbours config
                        maxN = _maxNeighbours config



-- | Verify integrity of a configuration that contains a 'NodeConfig'.
containedNode :: L.HasNodeConfig nodeConfig NodeConfig
              => nodeConfig
              -> IO ()
containedNode = node . L.view L.nodeConfig



-- | Verify integrity of a 'MultiConfig'.
multi :: MultiConfig -> IO ()
multi = containedNode



-- | Verify integrity of a 'BootstrapConfig'.
bootstrap :: BootstrapConfig -> IO ()
bootstrap = containedNode



-- | Verify integrity of a 'DrawingConfig'.
drawing :: DrawingConfig -> IO ()
drawing = containedNode