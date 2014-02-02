-- | Functions to query the upstream/downstream databases

module Utilities.Databases (
        isRoomIn
      , nodeRelationship
      , dbSize
      , makeTimestamp
) where


import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Applicative
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)

import Types



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



-- | Check whether there is room to add another node to the pool.
isRoomIn :: Environment
         -> (Environment -> TVar (Map.Map k a))
            -- ^ Projector from 'Environment' to the database, i.e. either
            -- "_upstream" or "_downstream".
         -> STM Bool
isRoomIn env db = (maxSize >) <$> dbSize env db
      where maxSize = fromIntegral (_maxNeighbours (_config env))



-- | Determine the current size of a database
dbSize :: Environment
       -> (Environment -> TVar (Map.Map k a)) -- _upstream or _downstream
       -> STM Int
dbSize env db = Map.size <$> readTVar (db env)



-- | Create a timestamp, which is a Double representation of the Unix time.
makeTimestamp :: (MonadIO m) => m Timestamp
makeTimestamp = liftIO (Timestamp . realToFrac <$> getPOSIXTime)
--   Since Haskell's Time library is borderline retarded, this seems to be the
--   cleanest way to get something that is easily an instance of Binary and
--   comparable to seconds.