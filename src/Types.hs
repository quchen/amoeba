{-# LANGUAGE DeriveGeneric #-}


module Types where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Set
import Data.Map
import Network
import GHC.Generics (Generic)
import Data.Functor

import qualified Pipes.Concurrent as P

import Data.Binary





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

-- | Unifies everything the list of known nodes has to store
data Client = Client { _clientTimestamp :: Timestamp
                     , _clientAsync     :: Async ()
                     , _stsc            :: PChan NormalSignal
                     }


-- | Configuration parameters accessible before anything goes online.
data Config = Config {

        _serverPort     :: Int        -- ^ Port to open the server socket on

      , _maxNeighbours  :: Word       -- ^ The maximum number of neighbours. No
                                      --   new ones will be accepted once it's
                                      --   full.

      , _minNeighbours  :: Word       -- ^ The minimum number of neighbours. If
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

      } deriving (Show)




-- | Uniquely identifies a node in a network by providing the address of its
--   server.
data Node = Node { _host :: String -- ^ Hostname
                 , _port :: Int    -- ^ Port
                 } -- See Network.Simple.TCP for docs
                 deriving (Eq, Ord, Generic)

instance Show Node where
      show n = "Node " ++ _host n ++ ":" ++ show (_port n)

instance Binary Node





newtype Timestamp = Timestamp Double
      deriving (Eq, Ord, Show, Generic)

instance Binary Timestamp





instance Binary PortNumber where
      get = (fromIntegral :: Int -> PortNumber) <$> get
      put = put . (fromEnum :: PortNumber -> Int)



data Signal =

        Normal NormalSignal

      -- | Signals that are handled in a special way. For example 'IAddedYou'
      --   signals have to be processed because when they are received the other
      --   node is by definition not an upstream neighbour yet.
      | Special SpecialSignal

      deriving (Eq, Ord, Show, Generic)

instance Binary Signal


-- | Stores a signal to be executed by a node, e.g. print a message, search for
--   new neighbours etc.
data NormalSignal =

      -- | Query to add an edge to the network. The 'To' parameter is the
      --   issuing node's server address.
      --
      --   The name has been chosen because when an EdgeRequest is complete,
      --   the graph of nodes will have a new edge.
        EdgeRequest To EdgeData

      -- | Randomly sent to downstream nodes so the timestamps are refreshed,
      --   and the node is kept in the books as an upstream neighbour
      | KeepAlive

      -- | Current node is shutting down, remove it from your upstream
      --   neighbour pool.
      | ShuttingDown

      -- | Signals meant to be considered by every node in the network.
      | Flood Timestamp FloodSignal

      deriving (Eq, Ord, Show, Generic)

instance Binary NormalSignal




-- | These signals will be distributed over the entire network, with every node
--   distributing them to all its downstream neighbours.
data FloodSignal =

      -- | Simple text message
        TextMessage String

      -- | Used to send a drawing server a full list of all neighbours. The
      --   address is of the painting server.
      | NeighbourList To

      deriving (Eq, Ord, Show, Generic)

instance Binary FloodSignal






-- | Sent back to the client in response to an icoming signal
data ServerResponse =

        -- | Server response if the command received will be processed
        OK

        -- | Unspecified error
      | Error

        -- | Sent to node that tries to connect without being a registered
        --   upstream neighbour
      | Ignore

        -- | Signal OK, but can't be accepted for some reason. This is the
        --   equivalent of 'Ignore' for commands that do not need an existing
        --   neighbourship, such as 'Handshake'.
      | Denied

        -- | Signal not allowed. Issued for example when an ordinary node
        --   receives a 'BootstrapRequest'.
      | Illegal

      deriving (Eq, Ord, Show, Generic)

instance Binary ServerResponse




-- | Classifies special signals in order to process them differently. For
--   example, many of them do not need the sending node to be known in order
--   to be processed.
data SpecialSignal =

      -- | Initial request sent from a future client to a bootstrap server.
      --   The 'Node' parameter allows other nodes to connect.
        BootstrapRequest To

      -- | Ask another node to initiate a handshake
      | HandshakeRequest To

      -- | Initiates a handshake, with the goal of adding the recipient as a
      --   downstream neighbour.
      | Handshake

      deriving (Eq, Ord, Show, Generic)

instance Binary SpecialSignal





-- | An edge request consists of the direction of the edge to construct, and
--   a bounce parameter to keep track of how far the request travels through the
--   network.
data EdgeData = EdgeData {
        _direction   :: Direction
      , _bounceParam :: Either Word (Word, Double)
            -- ^ Left n: Hard bounces left, i.e. how many times more the
            --           request will definitely be relayed
            --   Right (n, p): n: Counter how many times the signal was bounced
            --                    in the soft phase; this can be used to swallow
            --                    requests that bounce indefinitely.
            --                 p: Acceptance probability
      }
      deriving (Eq, Ord, Show, Generic)

instance Binary EdgeData






-- | Direction of a query that establishes a new connection
data Direction = Outgoing | Incoming
      deriving (Eq, Ord, Show, Generic)

instance Binary Direction


-- REMOVED IN PIPE REWRITE
{-
-- | Used in loops that may end. Continue means looping, Terminate hops out.
--   Functions will typically have the return type @IO Proceed@, indicating that
--   they are part of a loop that may terminate under certain conditions.
data Proceed = Continue | Terminate
-}


-- | Signifies a question, or a positive/negative answer to one.
data Predicate = Question | Yes | No
      deriving (Eq, Ord, Show, Generic)

instance Binary Predicate


-- | How many messages should be printed?
data Verbosity = Chatty  -- ^ *Everything*, e.g. passing bounces, keep-alive
                         --   signals
               | Debug   -- ^ Various status messages, e.g. gaining and losing
                         --   neighbours
               | Default -- ^ Useful for normal execution, e.g. node deficit,
                         --   chat messages
               | Quiet   -- ^ Only messages intended for display, i.e. chat
               | Mute    -- ^ Nothing, node just serves as a network helper
      deriving (Eq, Ord, Show)
      -- Note: Order matters in order to make `myVerbosity > x` work!



-- | Unique identifier for upstream nodes.
newtype From = From { getFrom :: Integer }
      deriving (Eq, Ord)

instance Show From where
      show (From i) = '#' : show i


-- | Node address clients can send data to. Used to ensure downstream data is
--   sent only to appropriate handles.
newtype To = To { getTo :: Node }
      deriving (Eq, Ord, Generic)

instance Show To where
      show (To node) = "To " ++ show node

instance Binary To


-- | Encodes in what relationship two nodes stand to each other
data NodeRelationship = IsSelf
                      | IsDownstreamNeighbour
                      | IsUnrelated
                   -- | IsUpstreamNeighbour -- Currently unused

-- | Pipe-based concurrent chan. Unifies read/write ends and sealing operation.
--   Used as a better wrapper around them than the default @(,,)@ returned from
--   'P.spawn\''.
data PChan a = PChan { _pOutput :: P.Output a
                     , _pInput  :: P.Input  a
                     , _pSeal   :: STM ()
                     }