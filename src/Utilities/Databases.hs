-- | Functions to query the upstream/downstream databases

module Utilities.Databases (


      -- * General
        isRoomIn
      , dbSize
      , makeTimestamp


      -- * USN DB
      , isUsn
      , insertUsn
      , deleteUsn
      , updateUsnTimestamp


      -- * DSN DB
      , isDsn
      , insertDsn
      , deleteDsn
      , dumpDsnDB
      , updateDsnTimestamp
      , nodeRelationship


      -- * Flood signal DB
      , knownFlood
      , insertFlood
) where


import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)

import Types





-- #############################################################################
-- ##  General functions  ######################################################
-- #############################################################################



-- | Projector of the upstream or downstream DB from the "Environment".
--   Unifies with "_upstream" and "_downstream".
type DBProjector k a = Environment -> TVar (Map.Map k a)



-- | Check whether there is room to add another node to the pool.
isRoomIn :: Environment
         -> DBProjector k a -- ^ "_upstream" or "_downstream"
         -> STM Bool
isRoomIn env db = (maxSize >) <$> dbSize env db
      where maxSize = (fromIntegral . _maxNeighbours . _config) env



-- | Determine the current size of a database
dbSize :: Environment
       -> DBProjector k a -- ^ "_upstream" or "_downstream"
       -> STM Int
dbSize env db = Map.size <$> readTVar (db env)



-- | Create a timestamp, which is a Double representation of the Unix time.
makeTimestamp :: (MonadIO m) => m Timestamp
makeTimestamp = liftIO (Timestamp . realToFrac <$> getPOSIXTime)
--   Since Haskell's Time library is borderline retarded, this seems to be the
--   cleanest way to get something that is easily an instance of Binary and
--   comparable to seconds.





-- #############################################################################
-- ##  USN DB handling  ########################################################
-- #############################################################################



-- | Is the USN in the DB?
isUsn :: Environment -> From -> STM Bool
isUsn env from = Map.member from <$> readTVar (_upstream env)



-- | Add a USN to the DB.
insertUsn :: Environment -> From -> Timestamp -> STM ()
insertUsn env from t = modifyTVar (_upstream env)
                                  (Map.insert from t)



-- | Remove a USN from the DB.
deleteUsn :: Environment -> From -> STM ()
deleteUsn env from = modifyTVar (_upstream env)
                                (Map.delete from)



-- | Update the "last heard of" timestmap in the database.
updateUsnTimestamp :: Environment -> From -> Timestamp -> STM ()
updateUsnTimestamp env from t = modifyTVar (_upstream env)
                                           (Map.adjust (const t) from)





-- #############################################################################
-- ##  DSN DB handling  ########################################################
-- #############################################################################



-- | "Set.Set" of all known DSN.
dumpDsnDB :: Environment -> STM (Set.Set To)
dumpDsnDB env = Map.keysSet <$> readTVar (_downstream env)



-- | Is the USN in the DB?
isDsn :: Environment -> To -> STM Bool
isDsn env to = Map.notMember to <$> readTVar (_downstream env)



-- | Insert/update a DSN.
insertDsn :: Environment
          -> To     -- ^ DSN address
          -> Client -- ^ Local client connecting to this address
          -> STM ()
insertDsn env to client = modifyTVar (_downstream env)
                                     (Map.insert to client)




deleteDsn :: Environment -> To -> STM ()
deleteDsn env to = modifyTVar (_downstream env)
                              (Map.delete to)



-- | Update the "last communicated with" timestmap in the DSN database.
updateDsnTimestamp :: Environment -> To -> Timestamp -> STM ()
updateDsnTimestamp env to t = modifyTVar (_downstream env)
                                         (Map.adjust updateTimestamp to)

      where updateTimestamp client = client { _clientTimestamp = t }



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
nodeRelationship env node
      | node == _self env = return IsSelf
      | otherwise = do isDS <- Map.member node <$> readTVar (_downstream env)
                       return (if isDS then IsDownstreamNeighbour
                                       else IsUnrelated)




-- #############################################################################
-- ##  Flood signal DB handling  ###############################################
-- #############################################################################



-- | Check whether a flood signal is already in the DB.
knownFlood :: Environment -> (Timestamp, FloodSignal) -> STM Bool
knownFlood env tfSignal = fmap (Set.member tfSignal)
                               (readTVar (_handledFloods env))



-- | Insert a new flood signal into the DB. Deletes an old one if the DB is
--   full.
insertFlood :: Environment -> (Timestamp, FloodSignal) -> STM ()
insertFlood env tfSignal = modifyTVar (_handledFloods env)
                                      (prune . Set.insert tfSignal)

      where -- Delete the oldest entry if the DB is full
            prune :: Set.Set a -> Set.Set a
            prune db | Set.size db > dbMaxSize = Set.deleteMin db
                     | otherwise               = db

            dbMaxSize = _floodMessageCache (_config env)