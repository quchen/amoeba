-- | Verifies whether the invariants of a configuration are satisfied, in order
--   to avoid nonsense program behaviour because of human error.

{-# LANGUAGE DeriveDataTypeable #-}

module Config.Verifier (
        node
      , multi
      , bootstrap
      , drawing
) where


import Data.List
import Control.Exception
import Data.Typeable
import Control.Monad

import Types.Config
import Config.OptionModifier




data ConfigError = PortRange
                 | TickRates
                 | PoolTimeout
                 | PoolSize
                 | RestartEvery
      deriving (Show, Typeable)

instance Exception ConfigError



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



multi :: MultiConfig -> IO ()
multi config = node (_nodeConfig config)



bootstrap :: BootstrapConfig -> IO ()
bootstrap config = node (_nodeConfig config)



drawing :: DrawingConfig -> IO ()
drawing config = node (_nodeConfig config)