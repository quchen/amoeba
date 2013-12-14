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

        _downstream :: TVar (Map To Client)
                                       -- ^ Neighbours the current node knows,
                                       --   and when they have last been sent
                                       --   a signal

      , _upstream   :: TVar (Map From Timestamp)
                                       -- ^ Nodes the current node knows it's
                                       --   a downstream neighbour of, or
                                       --   equivalently the set of upstream
                                       --   neighbours of the current node.
                                       --   Also carries a timestamp to keep
                                       --   track of when the last signal was
                                       --   received.

      , _st1c       :: PChan NormalSignal -- ^ Channel read by all clients.
                                          --   Sending a signal here will
                                          --   semi-randomly reach one of them.

      , _io         :: TBQueue (IO ()) -- ^ Send action to the output thread
                                       --   (so that concurrent prints don't
                                       --   get interleaved)

      , _handledFloods :: TVar (Set (Timestamp, FloodSignal))
                                       -- ^ Timestamped signals that have
                                       --   already been handled by the current
                                       --   node, and can thus be ignored if
                                       --   they come in again.

      , _self       :: To              -- ^ Own hostname/port

      , _ldc        :: Maybe (PChan NormalSignal)
                                       -- ^ Local direct connection (LDC) to a
                                       --   node. Used by NodePool.

      , _config     :: Config          -- ^ Program start configuration

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

      }



-- ^ Configuration of the bootstrap server
data BSConfig = BSConfig {
        _poolSize :: Int -- ^ Number of nodes in the server's pool

      }