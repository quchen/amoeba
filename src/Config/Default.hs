-- | Default configurations, used by the command line arguments parser. This
--   module is intended to be used qualified, e.g. as \"Default\" to make nice
--   names such as \"Default.nodeConfig\".

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
      , _bounces           = 2
      , _acceptP           = 0.5
      , _maxSoftBounces    = 10
      , _shortTickRate     = 1 * 10^5
      , _mediumTickRate    = 3 * 10^5
      , _longTickRate      = 10^6
      , _poolTimeout       = 5
      , _verbosity         = Default
      , _bootstrapServers  = Set.empty
      , _floodMessageCache = 1024
      }



poolConfig :: PoolConfig
poolConfig = PoolConfig {

        _poolSize = 8

      }


multiConfig :: MultiConfig
multiConfig = MultiConfig {
        _multiNodeConfig = nodeConfig
      , _multiPoolConfig = poolConfig
      }



drawingConfig :: DrawingConfig
drawingConfig = DrawingConfig {
        _drawingNodeConfig = nodeConfig
      , _drawingPoolConfig = poolConfig
      }


-- | Default bootstrap server configuration
bootstrapConfig :: BootstrapConfig
bootstrapConfig = BootstrapConfig {
        _restartEvery         = 5
      , _restartMinimumPeriod = 10^6
      , _bootstrapNodeConfig  = nodeConfig
      , _bootstrapPoolConfig  = poolConfig
      }