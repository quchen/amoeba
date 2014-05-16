-- | Functions for ad-hoc debugging.
--
--   While the exported interface is safe to use, the functionality provided
--   is far from being suitable for production.
module Unsafe (
        inc
      , dec
      , value
      , yellCounter
) where


import System.IO.Unsafe
import Control.Concurrent.STM



import Utilities



counter :: TVar Integer
counter = unsafePerformIO (newTVarIO 0)

-- | Increment counter
inc :: IO ()
inc = atomically (modifyTVar counter (+1))

-- | Decrement counter
dec :: IO ()
dec = atomically (modifyTVar counter (subtract 1))

value :: IO Integer
value = atomically (readTVar counter)

-- | Print counter contents
yellCounter :: IO ()
yellCounter = do x <- value
                 yell 45 ("Unsafe counter value: " ++ show x)
