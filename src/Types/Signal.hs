-- | Signal types, i.e. the protocol used.

{-# LANGUAGE DeriveGeneric #-}

module Types.Signal where


import GHC.Generics (Generic)
import Control.Concurrent.Async (Async)
import Text.Printf
import Data.Set (Set)

import Data.Binary
import Pipes.Concurrent as P




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

      -- | Signals meant to be considered by every node in the network.
      | Flood Timestamp FloodSignal

      -- | Sent to downstream nodes so the timestamps are refreshed, and the
      --   node is kept in the books as a living upstream neighbour
      | KeepAlive

      -- | Current node is shutting down, remove it from your upstream
      --   neighbour pool.
      | ShuttingDown

      -- | To avoid unnecessarily many connections, the "Prune" signal asks a
      --   DSN whether the connection can be dropped without making it cross
      --   its minimum connection threshold.
      | Prune

      deriving (Eq, Ord, Generic)

instance Show NormalSignal where
      show KeepAlive = "KeepAlive"
      show Prune = "Prune"
      show ShuttingDown = "ShuttingDown"
      show (Flood t s) = printf "Flood %s %s" (show t) (show s)
      show (EdgeRequest to ed) = printf "EdgeRequest { %s, %s }" (show to) (show ed)

instance Binary NormalSignal




-- | These signals will be distributed over the entire network, with every node
--   distributing them to all its downstream neighbours.
data FloodSignal =

      -- | Simple text message
        TextMessage String

      -- | Used to send a drawing server a full list of all neighbours. The
      --   address is of the painting server.
      | SendNeighbourList To

      deriving (Eq, Ord, Show, Generic)

instance Binary FloodSignal






-- | Sent back to the client in response to an icoming signal
data ServerResponse =

        -- | Server response if the command received will be processed
        OK

        -- | Generic error
      | Error String

        -- | Confirmation that dropping the connection is alright
      | PruneOK

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

        -- | The signal was received, but couldn't be decoded to the appropriate
        --   format
      | DecodeError

      | Timeout

      | ConnectionClosed

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

      -- | Own address and list of DSNs sent to the drawing server. Represents
      --   one node in the network graph.
      | NeighbourList To (Set To)

      deriving (Eq, Ord, Show, Generic)

instance Binary SpecialSignal





-- | An edge request consists of the direction of the edge to construct, and
--   a bounce parameter to keep track of how far the request travels through the
--   network.
data EdgeData = EdgeData {
        _direction   :: Direction
      , _bounceParam :: BounceParameter
            -- ^ Left n: Hard bounces left, i.e. how many times more the
            --           request will definitely be relayed
            --   Right (n, p): n: Counter how many times the signal was bounced
            --                    in the soft phase; this can be used to swallow
            --                    requests that bounce indefinitely.
            --                 p: Acceptance probability
      }
      deriving (Eq, Ord, Generic)

instance Show EdgeData where
      show ed = printf "%s edge (%s)"(show $ _direction ed) bp
            where bp :: String
                  bp = case _bounceParam ed of
                        HardBounce n -> printf "%d bounce%s left"
                                        n
                                        (if n /= 1 then "s" else "")
                        SoftBounce n p -> printf "bounces: %d, accept: %.2f" n p

instance Binary EdgeData



-- | Stores how many times an 'EdgeRequest' should be bounced on. A hard bounce
--   means the bounces are guaranteed to happen a certain number of times,
--   a soft bounce happens with a certain probability. The 'Word' parameter of
--   soft bounces is to provide a hard upper bound for bounces that aren't
--   accepted a certain number of times.
--
--   > HardBounce 10    -- Will bounce 10 times before entering soft bounce mode
--   > SoftBounce 3 0.8 -- Will be accepted with probability 0.8. It was bounced
--                         3 times already without being accepted.
data BounceParameter = HardBounce Word
                     | SoftBounce Word Double
                     deriving (Eq, Ord, Generic)

instance Binary BounceParameter






-- | Direction of a query that establishes a new connection
data Direction = Outgoing | Incoming
      deriving (Eq, Ord, Show, Generic)

instance Binary Direction



-- | Node address clients can send data to. Used to ensure downstream data is
--   sent only to appropriate handles.
newtype To = To { getTo :: Node }
      deriving (Eq, Ord, Generic)

instance Show To where
      show (To node) = "->" ++ show node

instance Binary To



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



-- | Unifies everything the list of known nodes has to store
data Client = Client {
        _clientTimestamp :: Timestamp          -- ^ Last downstream contact
      , _clientAsync     :: Async ()           -- ^ Client thread
      , _stsc            :: PChan NormalSignal -- ^ Direct channel, e.g. to send
                                               --   "KeepAlive" signals
      }



-- | Pipe-based concurrent chan. Unifies read/write ends and sealing operation.
--   Used as a better wrapper around them than the default @(,,)@ returned from
--   'P.spawn\''.
data PChan a = PChan { _pOutput :: P.Output a
                     , _pInput  :: P.Input  a
                     , _pSeal   :: STM ()
                     }