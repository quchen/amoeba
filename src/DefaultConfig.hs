-- | Default configurations, used by the command line arguments parser.

module DefaultConfig where

import Types



-- | Default node configuration
nodeConfig :: Config
nodeConfig = Config {
        _serverPort        = 21000
      , _maxNeighbours     = 10
      , _minNeighbours     = 5
      , _maxChanSize       = 100
      , _bounces           = 1
      , _acceptP           = 0.5
      , _maxSoftBounces    = 10
      , _shortTickRate     = 1 * 10^5
      , _mediumTickRate    = 3 * 10^5
      , _longTickRate      = 10^6
      , _poolTimeout       = 5
      , _verbosity         = Debug -- TODO: Change back to Default for production
      , _bootstrapServers  = []
      , _floodMessageCache = 1024
      }



-- | Default bootstrap server configuration
bsConfig :: BSConfig
bsConfig = BSConfig {
        _poolSize             = 8
      , _restartEvery         = 5
      , _restartMinimumPeriod = 10^6
      , _nodeConfig           = nodeConfig
      }