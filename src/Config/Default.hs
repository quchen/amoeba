-- | Default configurations, used by the command line arguments parser. This
--   module is intended to be used qualified, e.g. as \"Default\" to make nice
--   names such as \"Default.'nodeConfig'\".

{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Config.Default where


import qualified Data.Set as Set

import Types



-- | Default node configuration
nodeConfig :: NodeConfig
nodeConfig = NodeConfig {
        _serverPort        = 21000
      , _maxNeighbours     = 10
      , _minNeighbours     = 5
      , _maxChanSize       = 100
      , _hardBounces       = 2
      , _acceptP           = 0.5
      , _maxSoftBounces    = 10
      , _shortTickRate     = 1e5
      , _mediumTickRate    = 3e5
      , _longTickRate      = 1e6
      , _poolTimeout       = 5
      , _verbosity         = Default
      , _bootstrapServers  = Set.empty
      , _floodMessageCache = 1024
      }



-- | Default node pool configuration
poolConfig :: PoolConfig
poolConfig = PoolConfig {

        _poolSize = 8

      }



-- | Default multi-node client configuration
multiConfig :: MultiConfig
multiConfig = MultiConfig {
        _multiconfigNodeConfig = nodeConfig
      , _multiconfigPoolConfig = poolConfig
      }



-- | Default drawing server configuration
drawingConfig :: DrawingConfig
drawingConfig = DrawingConfig {
        _drawEvery         = 1e7
      , _drawFilename      = "network_graph.dot"
      , _drawTimeout       = 33 -- 33 seconds = 3 drawing attempts before timeout
      , _drawingconfigNodeConfig = nodeConfig
      , _drawingconfigPoolConfig = poolConfig
      }



-- | Default bootstrap server configuration
bootstrapConfig :: BootstrapConfig
bootstrapConfig = BootstrapConfig {
        _restartEvery         = 5
      , _restartMinimumPeriod = 1e6
      , _bootstrapconfigNodeConfig  = nodeConfig
      , _bootstrapconfigPoolConfig  = poolConfig
      }
