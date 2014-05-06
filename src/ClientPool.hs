-- | The client pool keeps track of running clients, requests new connections
--   when there's a deficit, and cleans up terminated ones.

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module ClientPool (
          clientPool
        , isRoomIn
) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List (intercalate)
import           Data.Set as Set (toList)
import           Text.Printf
import qualified Data.Map as Map

import           Pipes
import qualified Pipes.Concurrent as P
import qualified Pipes.Prelude as P

import           Control.Lens.Operators
import qualified Control.Lens as L
import qualified Types.Lens as L

import           Housekeeping
import           Types
import           Utilities


-- | Sets up the client pool by forking the housekeeping thread, and then starts
--   the client pool loop.
--
--   For further documentation, see @housekeeping@ and @clientLoop@.
clientPool :: Environment -> IO ()
clientPool env = withAsync hkeep $ \_ -> fillPool env
      where hkeep = dsnHousekeeping env


-- | Watches the count of nodes in the database, and issues 'EdgeRequest's
--   to fill the ranks if necessary.
fillPool :: Environment -> IO ()
fillPool env = runEffect (balanceEdges env >-> P.map edgeRequest >-> dispatch)

      where

      -- Send signal to the single worker channel
      dispatch :: Consumer NormalSignal IO ()
      dispatch = P.toOutput (env ^. L.st1c . L.pOutput)

      -- Create an EdgeRequest from a Direction
      edgeRequest :: Direction
                  -> NormalSignal
      edgeRequest dir =
            EdgeRequest (env ^. L.self)
                        (EdgeData dir
                                  (HardBounce (env ^. L.config . L.hardBounces)))



-- | Watch the database of upstream and downstream neighbours. If there is a
--   deficit in one of them, generate the 'Direction' of the new edge to
--   construct.
balanceEdges :: Environment -> Producer Direction IO r
balanceEdges env = forever $ do

      delay (env ^. L.config . L.mediumTickRate)

      (usnDeficit, dsnDeficit) <- liftIO . atomically $ do

            usnCount <- dbSize env L.upstream
            dsnCount <- dbSize env L.downstream

            -- Gather all DSN node ports
            dsnPorts <- fmap (map (L.view L.port . getTo) . Set.toList . Map.keysSet)
                             (env ^. L.downstream . L.to readTVar)

            let colouredNumber :: Int -> String
                colouredNumber n = printf "\ESC[3%dm%d\ESC[0m" (n `rem` 8 + 1) n
                -- Like List's Show instance, but won't recursively show
                -- list elements (therefore avoiding "\ESC..." in the output).
                showColoured = (++ "]") . ("[" ++) . intercalate ", "

            -- Print status message: "Network connections: upstream 7/(5..10),
            -- downstream 5/(5..10)"to indicate there are 7 of a minimum of 5,
            -- and a maximum of 10, upstream connections (and similarly for
            -- downstream).
            (toIO env Debug . STDLOG)
                  (printf "[%s] Network connections:\
                                \ upstream %*d/(%d..%d),\
                                \ downstream %*d/(%d..%d)\
                                \ %s"
                          (colouredNumber serverPort)
                          maxNDigits usnCount minN maxN
                          maxNDigits dsnCount minN maxN
                          (showColoured (map colouredNumber dsnPorts)))

            return ( minN - usnCount
                   , minN - dsnCount
                   )

      -- Convert a deficit in nodes into a number of edge requests to be sent
      -- out.
      --
      -- - The number should be at least one, so that a deficit leads to at
      --   least one request.
      -- - In order not to flood the network, the number of requests should not
      --   be too high: it might take multiple ticks of 'balanceEdges' to
      --   establish a new connection, and lots of requests will be sent out in
      --   that time.
      --
      -- For this reason, the current number of requests sent out is bounded
      -- below by 1, and grows like sqrt with the deficit.
      let requests deficit = max 1 (round (sqrt (fromIntegral deficit)))

      each (mergeLists (replicate (requests dsnDeficit) Outgoing)
                       (replicate (requests usnDeficit) Incoming))


      where config     = env    ^. L.config
            minN       = config ^. L.minNeighbours
            maxN       = config ^. L.maxNeighbours
            maxNDigits = round (logBase 10 (fromIntegral maxN)) + 1 :: Int
            serverPort = config ^. L.serverPort
