-- | Environment/configuration types.

module Types.Config where

import Control.Concurrent.STM
import Data.Set
import Data.Map

import Types.Signal
import Types.Misc
import Data.Word





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
      , _config     :: Config

      }



-- | Configuration parameters accessible before anything goes online.
data Config = Config {

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

      , _bootstrapServers :: [To]     -- ^ Addresses of bootstrap servers
                                      --   statically known

      , _floodMessageCache :: Int     -- ^ Number of past flood messages to
                                      --   store so duplicates can be discarded

      }



-- ^ Configuration of the bootstrap server
data BSConfig = BSConfig {

        _poolSize :: Int     -- ^ Number of nodes in the server's pool

      , _restartEvery :: Int -- ^ Every n connected nodes, one client is
                             --   restarted at random

      , _restartMinimumPeriod :: Int -- ^ Limit the maximal frequency at which
                                     --   restarts can happen

      , _nodeConfig :: Config -- ^ Configuration of the node pool's nodes

      }