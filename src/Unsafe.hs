-- | Unsafe global counter. Useful for example in order to check whether
--   functions terminate properly.
module Unsafe where

import System.IO.Unsafe
import Control.Concurrent.STM

import Utilities

leak = unsafePerformIO $ newTVarIO 0

-- | Increment counter
inc = do x <- atomically $ modifyTVar leak (+1) >> readTVar leak
         yell 45 $ "Leak counter: " ++ show x

-- | Decrement counter
dec = do x <- atomically $ modifyTVar leak (subtract 1) >> readTVar leak
         yell 45 $ "Leak counter: " ++ show x