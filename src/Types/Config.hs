-- | Environment/configuration types.

module Types.Config (


      -- * Environment
        Environment     (..)


      -- * Configurations

      , NodeConfig      (..)
      , PoolConfig      (..)
      , BootstrapConfig (..)
      , MultiConfig     (..)
      , DrawingConfig   (..)

) where



import Control.Concurrent.STM
import Data.Set
import Data.Map
import Data.Word

import Types.Signal
import Types.Misc





-- #############################################################################
-- ###  Environment  ###########################################################
-- #############################################################################



-- | State of the local node. Consists of a variety of communication channels,
--   the address of the node's server, and an initial program configuration.
data Environment = Environment {

      -- Mutable environment

        -- | Neighbours the current node knows, and when they have last been
        --   sent a signal
        _downstream :: TVar (Map To Client)

        -- | Nodes the current node knows it's a downstream neighbour of, or
        --   equivalently the set of upstream neighbours of the current node.
        --   Also carries a timestamp to keep track of when the last signal was
        --   received.
      , _upstream   :: TVar (Map From Timestamp)

        -- | Channel read by all clients. Sending a signal here will
        --   semi-randomly reach one of them.
      , _st1c       :: PChan NormalSignal

        -- | Send action to the output thread (so that concurrent prints don't
        --   get interleaved)
      , _io         :: IOQueue

        -- | Timestamped signals that have already been handled by the current
        --   node, and can thus be ignored if they come in again.
      , _handledFloods :: TVar (Set (Timestamp, FloodSignal))
                        -- Order of the tuple matters so that Timestamp is the
                        -- most significant in the Set's Ord type, and
                        -- Set.deleteMin works properly!

        -- | Own hostname/port
      , _self       :: To

        -- | Local direct connection (LDC) to a node. Used by NodePool.
      , _ldc        :: Maybe (PChan NormalSignal)

        -- | Program start configuration
      , _config     :: NodeConfig

      }





-- #############################################################################
-- ###  Configs  ###############################################################
-- #############################################################################



-- | Configuration parameters accessible before anything goes online.
data NodeConfig = NodeConfig {

        _serverPort     :: Int        -- ^ Port to open the server socket on

      , _maxNeighbours  :: Int        -- ^ The maximum number of neighbours. No
                                      --   new ones will be accepted once it's
                                      --   full.

      , _minNeighbours  :: Int        -- ^ The minimum number of neighbours. If
                                      --   the current number is smaller issue
                                      --   announce signals.

      , _maxChanSize    :: Int        -- ^ How many entries the bounded
                                      --   communication channels can hold

      , _bounces        :: Word       -- ^ Number of initial bounces

            -- TODO: Rename nounces to hardBounces

      , _acceptP        :: Double     -- ^ Edge request acceptance probability
                                      --   for the second bounce phase.

      , _maxSoftBounces :: Word       -- ^ How many times a soft-bounced request
                                      --   is maximally relayed before it is
                                      --   rejected

      , _shortTickRate  :: Int        -- ^ Tick interval in milliseconds for
                                      --   "short" loops.

      , _mediumTickRate :: Int        -- ^ Tick interval in milliseconds for
                                      --   "medium" loops, for example the
                                      --   client pool or the keep-alive loops.

      , _longTickRate   :: Int        -- ^ Tick interval in milliseconds for
                                      --   "long" loops.

      , _poolTimeout    :: Double     -- ^ Number of seconds before a
                                      --   non-responding node is considered
                                      --   gone

      , _verbosity      :: Verbosity  -- ^ Determines quantity of messages
                                      --   printed

      , _bootstrapServers :: Set To   -- ^ Addresses of bootstrap servers
                                      --   statically known

      , _floodMessageCache :: Int     -- ^ Number of past flood messages to
                                      --   store so duplicates can be discarded

      } deriving (Show)



-- | Node pool configuration
data PoolConfig = PoolConfig {

        _poolSize :: Int     -- ^ Number of nodes in the server's pool

      } deriving (Show)



-- | Configuration of the bootstrap server
data BootstrapConfig = BootstrapConfig {

        _bootstrapconfigNodeConfig :: NodeConfig
        -- Lens will create 'nodeConfig' out of this, stripped of the lowercase
        -- prefix.

      , _bootstrapconfigPoolConfig :: PoolConfig
        -- dito

      , _restartEvery :: Int -- ^ Every n bootstrap requests one client is
                             --   restarted at random

      , _restartMinimumPeriod :: Int -- ^ Limit the maximal frequency at which
                                     --   restarts can happen

      } deriving (Show)



-- | Multi client config
data MultiConfig = MultiConfig {

        _multiconfigNodeConfig :: NodeConfig
        -- Lens will create 'nodeConfig' out of this, stripped of the lowercase
        -- prefix.

      , _multiconfigPoolConfig :: PoolConfig
        -- dito

      } deriving (Show)



-- | Drawing server config
data DrawingConfig = DrawingConfig {

        _drawingconfigNodeConfig :: NodeConfig
        -- Lens will create 'nodeConfig' out of this, stripped of the lowercase
        -- prefix.

      , _drawingconfigPoolConfig :: PoolConfig
        -- dito

      , _drawEvery :: Int -- ^ Interval for sending out neighbour list
                          --   requests and drawing the currently known
                          --   state of the network

      , _drawFilename :: FilePath -- ^ Filename for the .dot file

      , _drawTimeout :: Double

      } deriving (Show)