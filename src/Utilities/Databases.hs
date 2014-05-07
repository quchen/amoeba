-- | Functions to query the upstream/downstream databases

{-# LANGUAGE RankNTypes #-} -- for DBProjector

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
import Control.Monad
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)

import Control.Lens.Operators
import qualified Control.Lens as L

import Types
import qualified Types.Lens as L





-- #############################################################################
-- ##  General functions  ######################################################
-- #############################################################################



-- | Projector of the upstream or downstream DB from the "Environment".
--   Unifies with 'L.upstream' or 'L.downstream'.
type DBProjector k a = L.Lens' Environment (TVar (Map.Map k a))



-- | Check whether there is room to add another node to the pool.
isRoomIn :: Environment
         -> DBProjector k a -- ^ 'L.upstream' or 'L.downstream'
         -> STM Bool
isRoomIn env db = fmap (maxSize >) (dbSize env db)
      where maxSize = env ^. L.config . L.maxNeighbours . L.to fromIntegral



-- | Determine the current size of a database
dbSize :: Environment
       -> DBProjector k a -- ^ 'L.upstream' or 'L.downstream'
       -> STM Int
dbSize env db = fmap Map.size (env ^. db . L.to readTVar)



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
isUsn env from = Map.member from <$> env ^. L.upstream . L.to readTVar



-- | Add a USN to the DB if there is space and it's not already in there.
insertUsn :: Environment
          -> From -- ^ New USN
          -> Timestamp
          -> STM Bool -- ^ True if an insertion was made, False if the node is
                      --   already present or a connection is not allowed.
insertUsn env from t = do
      isRoom <- isRoomIn env L.upstream

      -- Check for previous membership, just in case this method is called
      -- twice concurrently for some odd reason TODO: can this happen?
      -- This is an STM block after all
      alreadyKnown <- isUsn env from

      let p = isRoom && not alreadyKnown

      -- Reserve slot. Cleanup happens when the worker shuts down because
      -- of a non-OK signal.
      when p (modifyTVar (env ^. L.upstream)
                         (Map.insert from t))

      return p





-- | Remove a USN from the DB.
deleteUsn :: Environment -> From -> STM ()
deleteUsn env from = modifyTVar (env ^. L.upstream)
                                (Map.delete from)



-- | Update the "last heard of" timestmap in the database.
updateUsnTimestamp :: Environment -> From -> Timestamp -> STM ()
updateUsnTimestamp env from t = modifyTVar (env ^. L.upstream)
                                           (Map.adjust (const t) from)





-- #############################################################################
-- ##  DSN DB handling  ########################################################
-- #############################################################################



-- | "Set.Set" of all known DSN.
dumpDsnDB :: Environment -> STM (Set.Set To)
dumpDsnDB env = fmap Map.keysSet
                     (readTVar (env ^. L.downstream))



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
            prune :: Set.Set a -> Set.Set a
            prune db | Set.size db > dbMaxSize = Set.deleteMin db
                     | otherwise               = db

            dbMaxSize = env ^. L.config . L.floodMessageCache