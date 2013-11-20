-- | Unsafe global counter. Useful for example in order to check whether
--   functions terminate properly.
module Unsafe where

import Control.Monad.Trans
import System.IO.Unsafe
import Control.Concurrent.STM

import Utilities

leak :: TVar Integer
leak = unsafePerformIO $ newTVarIO 0
{-# NOINLINE leak #-}

-- | Increment counter
inc :: MonadIO io => io ()
inc = liftIO $ do x <- atomically $ modifyTVar leak (+1) >> readTVar leak
                  yell 45 $ "Leak counter: " ++ show x

-- | Decrement counter
dec :: MonadIO io => io ()
dec = liftIO $ do x <- atomically $ modifyTVar leak (subtract 1) >> readTVar leak
                  yell 45 $ "Leak counter: " ++ show x

-- | Increments the counter before, and decrements it after, an IO action.
wrap :: MonadIO io => io a -> io a
wrap io = do
      inc
      r <- io
      dec
      return r