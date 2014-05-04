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


import Control.Exception
import Data.Typeable
import Control.Monad

import qualified Control.Lens as L
import Control.Lens.Operators
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
                  where p = config ^. L.serverPort

            -- Are the tickrates in the right order?
            -- (small <= medium <= long)
            tickRates = unless (isSorted rates) (throwIO TickRates)
                  where rates = map (config ^.) [ L.shortTickRate
                                                , L.mediumTickRate
                                                , L.longTickRate
                                                ]
                        isSorted xs = and (zipWith (<=) xs (tail xs))
                        -- TODO: Test ^

            -- Timeouts must be longer than the long tickrate (and should be so
            -- by a factor of about 3, nyquist etc.)
            poolTimeout = when (ltr > tout) (throwIO PoolTimeout)
                  where ltr  = fromIntegral (config ^. L.longTickRate) / 10^6
                        tout = config ^. L.poolTimeout

            -- minimum <= maximum neighbours
            poolSize = when (minN > maxN) (throwIO PoolSize)
                  where minN = config ^. L.minNeighbours
                        maxN = config ^. L.maxNeighbours



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