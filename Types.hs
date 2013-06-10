{-# LANGUAGE DeriveGeneric #-}


module Types (
        NodeEnvironment(..)
      , Signal(..)
      , EdgeData(..)
      , Direction(..)
      , Timestamp(..)
      , Node(..)
      , Proceed(..)
) where

import Control.Concurrent.STM
import Data.Set
import Data.Map
import Network
import GHC.Generics (Generic)
import Data.Functor

import Data.Binary





-- | State of the local node
data NodeEnvironment = NodeEnvironment {

      -- Mutable environment

        _knownNodes :: TVar (Set Node) -- ^ Neighbours the current node knows

      , _knownBy    :: TVar (Map Node Timestamp)
                                       -- ^ Nodes the current node knows it's
                                       --   a downstream neighbour of, or
                                       --   equivalently the set of upstream
                                       --   neighbours of the current node.
                                       --   Also carries a timestamp to keep
                                       --   track of when the last signal was
                                       --   received.

      , _stc        :: TChan Signal    -- ^ Send messages to all clients

      , _st1c       :: TBQueue Signal  -- ^ Send message to one client

      , _io         :: TBQueue (IO ()) -- ^ Send action to the dedicated local
                                       --   IO thread

      , _handledQueries :: TVar (Set Signal)
                                       -- ^ Timestamped signals that have
                                       --   already been handled by the current
                                       --   node, and can thus be ignored if
                                       --   they come in again.


      -- Configuration

      , _self       :: Node            -- ^ Own hostname/port

      , _maxNeighbours  :: Word       -- ^ The maximum number of neighbours. No
                                      --   new ones will be accepted once it's
                                      --   full.

      , _minNeighbours  :: Word       -- ^ The minimum number of neighbours. If
                                      --   the current number is smaller issue
                                      --   announce signals.

      , _portRange      :: (Int, Int) -- ^ The node will open a server on a
                                      --   randomly picked node in this range.

      , _maxChanSize    :: Int        -- ^ How many entries the bounded
                                      --   communication channels can hold

      , _maxRandomPorts :: Word       -- ^ Number of retries to find a random
                                      --   open port

      , _bounces        :: Word       -- ^ Number of initial bounces
      , _lambda         :: Double     -- ^ Parameter for exponentially
                                      --   distributed things. Each step, the
                                      --   probability will decrease by a
                                      --   factor lambda. Must be >= 1.
      , _poolTickRate   :: Int        -- ^ Every couple of milliseconds, the
                                      --   client pool will loop to maintain a
                                      --   proper connection to the network.
      , _keepAliveTickRate :: Int     -- ^ Like the pool tickrate, but for
                                      --   sending KeepAlive signals downstream.
      , _poolTimeout    :: Double     -- ^ Number of seconds before a
                                      --   non-responding node is considered
                                      --   gone

      }





-- | Uniquely identifies a node in a network by providing the address of its
--   server.
data Node = Node { _host :: HostName
                 , _port :: PortNumber
                 }
                 deriving (Eq, Ord, Show, Generic)

instance Binary Node





newtype Timestamp = Timestamp Double
      deriving (Eq, Ord, Show, Generic)

instance Binary Timestamp





instance Binary PortNumber where
      get = (fromIntegral :: Int -> PortNumber) <$> get
      put = put . (fromEnum :: PortNumber -> Int)





-- | Stores a signal to be executed by a node, e.g. print a message, search for
--   new neighbours etc.
data Signal =

        -- | Query to add an edge to the network. Direction specifies which way
        --   the new connection should go. The Either part is used to limit how
        --   far the request bounces through the network.
        --
        --   The name has been chosen because when an EdgeRequest is complete,
        --   the graph of nodes will have a new edge.
        EdgeRequest Node EdgeData


        -- | Special version of an EdgeRequest that is accepted by nodes even
        --   when the sender is not an upstream node. Used to introduce a new
        --   node to the network.
      | Bootstrap PortNumber

        -- | Sent as a response to a Bootstrap to tell the node its hostname, so
        --   it can add it to its NodeEnvironment.
      | YourHostIs HostName

        -- | Sent to the new downstream neighbour node so it can keep track of
        --   how many times it's referenced
      | IAddedYou Node

        -- | Sent to the requesting node: "I have upstream neighbour space free"
      | AddMe

        -- | Randomly sent to downstream nodes so the timestamps are refreshed,
        --   and the node is kept in the books as an upstream neighbour
      | KeepAlive

        -- | Current node is shutting down, remove it from your upstream
        --   neighbour pool
      | ShuttingDown Node

        -- | Sent to node that tries to connect without being a registered
        --   upstream neighbour, so it can remove the current node from its
        --   database.
      | NotYourNeighbour

        -- | Text message. Should only be considered when it comes timestamped
        --   in a Signal.
      | Message Timestamp String

      deriving (Eq, Ord, Show, Generic)

instance Binary Signal




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
data Direction = Request  -- ^ Request new neighbours to fill the pool
               | Announce -- ^ Announce a certain node so others add it to their
                          --   pool
               deriving (Eq, Ord, Show, Generic)

instance Binary Direction



-- | Used in loops that may end. Continue means looping, Terminate hops out.
data Proceed = Continue | Terminate
      deriving (Eq, Ord, Show)