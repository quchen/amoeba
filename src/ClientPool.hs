-- | The client pool keeps track of running clients, requests new connections
--   when there's a deficit, and cleans up terminated ones.

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module ClientPool (
          clientPool
        , isRoomIn
) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import qualified Data.Map as Map
import           Control.Monad
import           Text.Printf
import Data.Set (toList)
import Data.List (intercalate)

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
clientPool env = do withAsync hkeep $ \_ -> fillPool env
      where hkeep = dsnHousekeeping env


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
            dispatch = P.toOutput (_pOutput (_st1c env))

            -- Create an EdgeRequest from a Direction
            edgeRequest :: Direction
                        -> NormalSignal
            edgeRequest dir =
                  EdgeRequest (_self env)
                              (EdgeData dir
                                        (HardBounce (_bounces (_config env))))



-- | Watch the database of upstream and downstream neighbours. If there is a
--   deficit in one of them, generate the 'Direction' of the new edge to
--   construct.
balanceEdges :: Environment -> Producer Direction IO r
balanceEdges env = forever $ do

      delay ((_mediumTickRate . _config) env)

      (usnDeficit, dsnDeficit) <- liftIO $ atomically $ do

            usnCount <- dbSize env _upstream
            dsnCount <- dbSize env _downstream

            dsn <- map (_port . getTo) . toList . Map.keysSet <$> readTVar (_downstream env)

            let coloredNumber :: Int -> Int -> String
                coloredNumber m x = printf "\ESC[3%dm%d\ESC[0m" (x `rem` m + 1) x

                m = 8 -- modulus for colouring

            -- Print status message: "Network connections: upstream 7/(5..10),
            -- downstream 5/(5..10)"to indicate there are 7 of a minimum of 5,
            -- and a maximum of 10, upstream connections (and similarly for
            -- downstream).
            toIO env Debug . STDLOG $ printf -- DEBUG colours
                  "[%s] Network connections:\
                        \ upstream %*d/(%d..%d),\
                        \ downstream %*d/(%d..%d)\
                        \ %s\n"
                  (coloredNumber m serverPort)
                  maxNDigits usnCount minN maxN
                  maxNDigits dsnCount minN maxN
                  ((++ "]") . ("[" ++) . intercalate ", " $ map (coloredNumber m) dsn)

            return ( minN - usnCount
                   , minN - dsnCount
                   )

      each (mergeLists (replicate dsnDeficit Outgoing)
                       (replicate usnDeficit Incoming))


      where minN       = _minNeighbours (_config env)
            maxN       = _maxNeighbours (_config env)
            maxNDigits = round (logBase 10 (fromIntegral maxN)) + 1 :: Int
            serverPort = _serverPort (_config env)



-- | Check whether there is room to add another node to the pool. The second
--   argument is supposed to be either
isRoomIn :: Environment
         -> (Environment -> TVar (Map.Map k a))
            -- ^ Projector from 'Environment' to the database, i.e. either
            -- "_upstream" or "_downstream".
         -> STM Bool
isRoomIn env db = (maxSize >) <$> dbSize env db
      where maxSize = fromIntegral (_maxNeighbours (_config env))

