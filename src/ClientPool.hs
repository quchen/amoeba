-- | The client pool keeps track of running clients, requests new connections
--   when there's a deficit, and cleans up terminated ones.

-- TODO: Print status periodically like in the pre-pipes version (i.e. current
--       neighbours in both directions)
-- TODO: Refactor the housekeeping part, it's fugly
-- FIXME: If the db sizes don't change and the transaction in 'balanceEdges'
--        is retrying, the function will lock. This could be avoided by having
--        a periodically changed nonsense TVar in the transaction, but that
--        seems a bit hacky. Are there better solutions?
-- TODO: >-> is pull-based. Would push-based composition be more appropriate?

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module ClientPool (
          clientPool
        , isRoomIn
) where

import           Control.Exception
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import qualified Data.Map as Map
import           Control.Monad
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Data.Maybe (isJust)
import           Text.Printf
import qualified Data.Set as Set

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Concurrent as P


import Types
import Housekeeping
import Utilities


-- | Sets up the client pool by forking the housekeeping thread, and then starts
--   the client pool loop.
--
--   For further documentation, see @housekeeping@ and @clientLoop@.
clientPool :: Environment -> IO ()
clientPool env = withAsync hkeep $ \_ -> fillPool env
      where hkeep = clientPoolHousekeeping env


-- | Watches the count of nodes in the database, and issues 'EdgeRequest's
--   to fill the ranks if necessary.
fillPool :: Environment -> IO ()
fillPool env = yellAndRethrow 42 ("ClientPool: " ++) $

      runEffect $ balanceEdges env
              >-> P.map edgeRequest
              >-> dispatch

      where
            -- Send signal to the single worker channel
            dispatch :: Consumer NormalSignal IO ()
            dispatch = P.toOutput ((_pOutput . _st1c) env)

            -- Create an 'EdgeRequest' from a 'Direction'
            edgeRequest :: Direction
                        -> NormalSignal
            edgeRequest dir =
                  EdgeRequest (_self env)
                              (EdgeData dir
                                        (HardBounce ((_bounces._config) env)))



-- | Watch the database of upstream and downstream neighbours. If there is a
--   deficit in one of them, generate the 'Direction' of the new edge to
--   construct.
balanceEdges :: Environment -> Producer Direction IO r
balanceEdges env = forever $ do

      delay ((_mediumTickRate . _config) env)

      (usnDeficit, dsnDeficit) <- liftIO $ atomically $ do

            usnCount <- dbSize env _upstream
            dsnCount <- dbSize env _downstream

            -- Print status message: "Network connections: upstream 7/(5..10),
            -- downstream 5/(5..10)"to indicate there are 7 of a minimum of 5,
            -- and a maximum of 10, upstream connections (and similarly for
            -- downstream).
            toIO env Debug $ printf
                  "Network connections: upstream %d/(%d..%d),\
                                    \ downstream %d/(%d..%d)\n"
                  usnCount minN maxN
                  dsnCount minN maxN

            return ( minN - usnCount
                   , minN - dsnCount
                   )

      each $ mergeLists (replicate dsnDeficit Outgoing)
                        (replicate usnDeficit Incoming)


      where minN = _minNeighbours (_config env)
            maxN = _maxNeighbours (_config env)

            -- mergeLists [a,b] [w,x,y,z]  ==  [a,w,b,x,y,z]
            mergeLists []     ys = ys
            mergeLists (x:xs) ys = x : mergeLists ys xs



-- | Makes sure other nodes know this node is still running and has them as its
--   neighbour, removes timed out upstream nodes and dead clients/downstream
--   nodes.
clientPoolHousekeeping :: Environment -> IO ()
clientPoolHousekeeping env = forever $ do
      t <- makeTimestamp
      removeTimedOutUsn env t
      removeTimedOutDsn env t
      removeDeadClients env
      sendKeepAlive     env t
      delay (_mediumTickRate $ _config env)



-- | Check whether there is room to add another node to the pool. The second
--   argument is supposed to be either
isRoomIn :: Environment
         -> (Environment -> TVar (Map.Map k a))
            -- ^ Projector from 'Environment' to the database, i.e. either
            -- '_upstream' or '_downstream'.
         -> STM Bool
isRoomIn env db = (maxSize >) <$> dbSize env db
      where maxSize = (fromIntegral . _maxNeighbours . _config) env

