-- | Types that don't fit in any of the specialized modules.

module Types.Misc where

import Control.Concurrent
import Control.Concurrent.STM.TBQueue



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



-- | Encodes in what relationship two nodes stand to each other
data NodeRelationship = IsSelf
                      | IsDownstreamNeighbour
                      | IsUnrelated
                      deriving (Eq, Ord, Show)



-- | Wrapper around the queue to the output thread
newtype IOQueue = IOQueue { _ioQueue :: TBQueue OutMsg }

-- | Used to send a message to the terminal via "IOQueue"s
data OutMsg = STDOUT String
            | STDERR String
            | STDLOG String

-- | Used by the client pool. When the MVar contained is filled, an arbitrary
--   node will be terminated.
newtype TerminationTrigger = TerminationTrigger (MVar ())