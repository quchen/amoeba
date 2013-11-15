-- | Signal types, i.e. the protocol used.

{-# LANGUAGE DeriveGeneric #-}

module Types.Signal where


import GHC.Generics (Generic)
import Control.Concurrent.Async (Async)
import Text.Printf

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

      -- | Randomly sent to downstream nodes so the timestamps are refreshed,
      --   and the node is kept in the books as an upstream neighbour
      | KeepAlive

      -- | Current node is shutting down, remove it from your upstream
      --   neighbour pool.
      | ShuttingDown

      -- | Signals meant to be considered by every node in the network.
      | Flood Timestamp FloodSignal

      deriving (Eq, Ord, Generic)

instance Show NormalSignal where
      show KeepAlive = "KeepAlive"
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
      | NeighbourList To

      deriving (Eq, Ord, Show, Generic)

instance Binary FloodSignal






-- | Sent back to the client in response to an icoming signal
data ServerResponse =

        -- | Server response if the command received will be processed
        OK

        -- | Generic error
      | Error String

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
      deriving (Eq, Ord, Generic)

instance Show EdgeData where
      show ed = printf "%s edge (%s)"(show $ _direction ed) bp
            where bp :: String
                  bp = case _bounceParam ed of
                        Left n -> printf "%d bounce%s left"
                                         n
                                         (if n /= 1 then "s" else "")
                        Right (n,p) -> printf "bounces: %d, accept: %.2f" n p

instance Binary EdgeData






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
data Client = Client { _clientTimestamp :: Timestamp
                     , _clientAsync     :: Async ()
                     , _stsc            :: PChan NormalSignal
                     }



-- | Pipe-based concurrent chan. Unifies read/write ends and sealing operation.
--   Used as a better wrapper around them than the default @(,,)@ returned from
--   'P.spawn\''.
data PChan a = PChan { _pOutput :: P.Output a
                     , _pInput  :: P.Input  a
                     , _pSeal   :: STM ()
                     }