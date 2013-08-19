-- TODO: Put types only needed by single modules in their respective locations


{-# LANGUAGE DeriveGeneric #-}


module Types where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Set
import Data.Map
import Network
import GHC.Generics (Generic)
import Data.Functor

import Data.Binary





-- | State of the local node. Consists of a variety of communication channels,
--   the address of the node's server, and an initial program configuration.
data Environment = Environment {

      -- Mutable environment

        _downstream :: TVar (Map To Client)
                                       -- ^ Neighbours the current node knows,
                                       --   and when they have last been sent
                                       --   a signal

      , _upstream    :: TVar (Map From Timestamp)
                                       -- ^ Nodes the current node knows it's
                                       --   a downstream neighbour of, or
                                       --   equivalently the set of upstream
                                       --   neighbours of the current node.
                                       --   Also carries a timestamp to keep
                                       --   track of when the last signal was
                                       --   received.

      , _stc        :: TChan NormalSignal    -- ^ Send messages to all clients

      , _st1c       :: TBQueue NormalSignal  -- ^ Send message to one client

      , _io         :: TBQueue (IO ()) -- ^ Send action to the dedicated local
                                       --   IO thread

      , _handledQueries :: TVar (Set NormalSignal)
                                       -- ^ Timestamped signals that have
                                       --   already been handled by the current
                                       --   node, and can thus be ignored if
                                       --   they come in again.

      , _self       :: Node            -- ^ Own hostname/port

      , _config     :: Config          -- ^ Program start configuration

      }

-- | Unifies everything the list of known nodes has to store
data Client = Client { _clientTimestamp :: Timestamp
                     , _clientAsync     :: Async ()
                     , _clientQueue     :: TBQueue NormalSignal
                     }


-- | Configuration parameters accessible before anything goes online.
data Config = Config {

        _serverPort     :: PortNumber -- ^ Port to open the server socket on

      , _maxNeighbours  :: Word       -- ^ The maximum number of neighbours. No
                                      --   new ones will be accepted once it's
                                      --   full.

      , _minNeighbours  :: Word       -- ^ The minimum number of neighbours. If
                                      --   the current number is smaller issue
                                      --   announce signals.

      , _maxChanSize    :: Int        -- ^ How many entries the bounded
                                      --   communication channels can hold

      , _bounces        :: Word       -- ^ Number of initial bounces
      , _acceptP        :: Double     -- ^ Edge requesti acceptance probability
                                      --   for the second bounce phase.
      , _poolTickRate   :: Int        -- ^ Every couple of milliseconds, the
                                      --   client pool will loop to maintain a
                                      --   proper connection to the network.
      , _keepAliveTickRate :: Int     -- ^ Like the pool tickrate, but for
                                      --   sending KeepAlive signals downstream.
      , _poolTimeout    :: Double     -- ^ Number of seconds before a
                                      --   non-responding node is considered
                                      --   gone
      , _verbosity      :: Verbosity  -- ^ Determines quantity of messages
                                      --   printed
}




-- | Uniquely identifies a node in a network by providing the address of its
--   server.
data Node = Node { _host :: HostName
                 , _port :: PortNumber
                 }
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

      -- | Signals that are handled in a special way. For example bootstrap
      --   servers are able to issue a special signal that is processed
      --   despite them not being upstream neighbours of a node. -- TODO: Implement this behaviour
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
      --   neighbour pool. The 'To' address provided is the server address of
      --   the terminating node.
      | ShuttingDown To

      -- | Text message.
      | TextMessage Timestamp String

      deriving (Eq, Ord, Show, Generic)

instance Binary NormalSignal






-- | Sent back to the client in response to an icoming signal
data ServerResponse =

        -- | Server response if the command received will be processed
        OK

        -- | Unspecified error
      | Error

        -- | Sent to node that tries to connect without being a registered
        --   upstream neighbour
      | Ignore

      deriving (Eq, Ord, Show, Generic)

instance Binary ServerResponse





-- | Classifies special signals in order to process them differently. For
--   example, many of them do not need the sending node to be known in order
--   to be processed.
data SpecialSignal =

      -- | Sent from a bootstrap server to the network. Bypasses checks whether
      --   it was issued from a registered upstream nodes, and is therefore
      --   suitable for making an initial connection to the network.
        BootstrapHelper NormalSignal

      -- | Initial request sent from a future client to a bootstrap server.
      --   While the reverse connection is provided by the request, the
      --   hostname will be deduced by the incoming connection by the server.
      | BootstrapRequest PortNumber

      -- | Sent as a response to a Bootstrap to tell the node its hostname, so
      --   it can add it to its Environment.
      | YourHostIs HostName

      -- | Sent to the new downstream neighbour node so it can keep track of
      --   how many times it's referenced.
      | IAddedYou

      -- | Sent to the requesting node: "I have upstream neighbour space free"
      | AddMe Node

      deriving (Eq, Ord, Show, Generic)

instance Binary SpecialSignal





-- | An edge request consists of the direction of the edge to construct, and
--   a bounce parameter to keep track of how far the request travels through the
--   network.
data EdgeData = EdgeData {
        _direction   :: Direction
      , _bounceParam :: Either Word Double
      }
      deriving (Eq, Ord, Show, Generic)

instance Binary EdgeData






-- | Direction of a query that establishes a new connection
data Direction = Outgoing | Incoming
      deriving (Eq, Ord, Show, Generic)

instance Binary Direction



-- | Used in loops that may end. Continue means looping, Terminate hops out.
--   Functions will typically have the return type @IO Proceed@, indicating that
--   they are part of a loop that may terminate under certain conditions.
data Proceed = Continue | Terminate



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



-- | Node address clients can send data to. Used to ensure upstream nodes aren't
--   sent downstream data.
newtype From = From { getFrom :: Node }
      deriving (Eq, Ord, Show)


-- | Node address clients can send data to. Used to ensure downstream data is
--   sent only to appropriate handles.
newtype To = To { getTo :: Node }
      deriving (Eq, Ord, Show, Generic)

instance Binary To