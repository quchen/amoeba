-- | Functions to query the upstream/downstream databases

-- TODO: The USN/DSN API could probably be refactored significantly by using
--       optics.

module Utilities.Databases (

      -- * General
        makeTimestamp


      -- * USN DB
      , insertUsn
      , deleteUsn
      , isUsn
      , usnDBSize
      , isRoomForUsn


      -- * DSN DB
      , insertDsn
      , deleteDsn
      , isDsn
      , dsnDBSize
      , isRoomForDsn
      , dumpDsnDB
      , updateDsnTimestamp
      , nodeRelationship


      -- * Flood signal DB
      , knownFlood
      , insertFlood

) where


import           Control.Concurrent.STM
import           Control.Monad.Trans
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           Control.Lens.Operators
import qualified Control.Lens as L

import           Types
import qualified Types.Lens as L





-- #############################################################################
-- ##  General functions  ######################################################
-- #############################################################################



-- | Create a timestamp, which is a Double representation of the Unix time.
makeTimestamp :: (MonadIO m) => m Timestamp
makeTimestamp = liftIO (fmap (Timestamp . realToFrac) getPOSIXTime)
--   Since Haskell's Time library is borderline retarded, this seems to be the
--   cleanest way to get something that is easily an instance of Binary and
--   comparable to seconds.





-- #############################################################################
-- ##  USN DB handling  ########################################################
-- #############################################################################



-- | Insert a USN into the DB.
insertUsn :: Environment -> From -> STM ()
insertUsn env from = modifyUsnDB env (Set.insert from)



-- | Delete a USN from the DB.
deleteUsn :: Environment -> From -> STM ()
deleteUsn env from = modifyUsnDB env (Set.delete from)



-- | Modify the entire USN DB with a function. Used as a general interface for
--   smaller functions.
modifyUsnDB :: Environment -> (Set From -> Set From) -> STM ()
modifyUsnDB env f = modifyTVar' db f
      where db = L.view L.upstream env


-- | Query the entire USN DB. Used as a general interface for smaller functions.
queryUsnDB :: Environment -> (Set From -> a) -> STM a
queryUsnDB env query = fmap query (readTVar db)
      where db = env ^. L.upstream



-- | Retrieve the current USN DB size.
usnDBSize :: Environment -> STM Int
usnDBSize env = queryUsnDB env (Set.size)



-- | Check whether a USN is in the DB.
isUsn :: Environment -> From -> STM Bool
isUsn env from = queryUsnDB env (Set.member from)



-- | Check whether another USN can be added to the DB without exceeding the
--   neighbour limit.
isRoomForUsn :: Environment -> STM Bool
isRoomForUsn env = fmap (maxSize > ) (usnDBSize env)
      where maxSize = env ^. L.config . L.maxNeighbours . L.to fromIntegral





-- #############################################################################
-- ##  DSN DB handling  ########################################################
-- #############################################################################



-- | 'Set' of all known DSN.
dumpDsnDB :: Environment -> STM (Set To)
dumpDsnDB env = fmap Map.keysSet
                     (readTVar (env ^. L.downstream))



-- | Query the entire DSN DB. Used as a general interface for smaller functions.
queryDsnDB :: Environment -> (Map To Client -> a) -> STM a
queryDsnDB env query = fmap query (readTVar db)
      where db = env ^. L.downstream





-- | Determine the current size of a database
dsnDBSize :: Environment -> STM Int
dsnDBSize env = queryDsnDB env (Map.size)



-- | Check whether there is room to add another node to the pool.
isRoomForDsn :: Environment -> STM Bool
isRoomForDsn env = fmap (maxSize >) (dsnDBSize env)
      where maxSize = env ^. L.config . L.maxNeighbours . L.to fromIntegral




-- | Is the USN in the DB?
--
--   (Defined in terms of "nodeRelationship", mainly to provide an analogon for
--   "isUsn".)
isDsn :: Environment -> To -> STM Bool
isDsn env to = fmap (== IsDownstreamNeighbour)
                    (nodeRelationship env to)



-- | Insert/update a DSN.
insertDsn :: Environment
          -> To     -- ^ DSN address
          -> Client -- ^ Local client connecting to this address
          -> STM ()
insertDsn env to client = modifyTVar (env ^. L.downstream)
                                     (Map.insert to client)



-- | Remove a DSN from the DB.
deleteDsn :: Environment -> To -> STM ()
deleteDsn env to = modifyTVar (env ^. L.downstream)
                              (Map.delete to)



-- | Update the "last communicated with" timestmap in the DSN DB.
updateDsnTimestamp :: Environment -> To -> Timestamp -> STM ()
updateDsnTimestamp env to t = modifyTVar (env ^. L.downstream)
                                         (Map.adjust (L.clientTimestamp .~ t)
                                                     to)



-- | What is the relationship between this node and another one? A node must not
--   connect to itself or to known neighbours multiple times.
--
--   Due to the fact that an "EdgeRequest" does not contain the upstream address
--   of the connection to be established, it cannot be checked whether the node
--   is already an upstream neighbour directly; timeouts will have to take care
--   of that.
nodeRelationship :: Environment
                 -> To
                 -> STM NodeRelationship
nodeRelationship env to
      | to == env ^. L.self = return IsSelf
      | otherwise = do isDSN <- fmap (Map.member to)
                                     (readTVar (env ^. L.downstream))
                       return (if isDSN then IsDownstreamNeighbour
                                        else IsUnrelated)





-- #############################################################################
-- ##  Flood signal DB handling  ###############################################
-- #############################################################################



-- | Check whether a flood signal is already in the DB.
knownFlood :: Environment -> (Timestamp, FloodSignal) -> STM Bool
knownFlood env tfSignal = fmap (Set.member tfSignal)
                               (env ^. L.handledFloods . L.to readTVar)



-- | Insert a new flood signal into the DB. Deletes an old one if the DB is
--   full.
insertFlood :: Environment -> (Timestamp, FloodSignal) -> STM ()
insertFlood env tfSignal = modifyTVar (env ^. L.handledFloods)
                                      (prune . Set.insert tfSignal)

      where -- Delete the oldest entry if the DB is full
            prune :: Set a -> Set a
            prune db | Set.size db > dbMaxSize = Set.deleteMin db
                     | otherwise               = db

            dbMaxSize = env ^. L.config . L.floodMessageCache