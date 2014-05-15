-- | Functions needed over a large range of other modules.

module Utilities (

        module Utilities.Debug
      , module Utilities.Concurrency
      , module Utilities.Databases
      , module Utilities.IOQueue
      , module Utilities.Networking

      -- * Various utilities
      , whenM
      , ifM
      , whileM
      , pluralS
      , mergeLists
      , showT
      , iSqrt

) where



import           Control.Monad

import qualified Data.Text as T

import           Utilities.Concurrency
import           Utilities.Databases
import           Utilities.Debug
import           Utilities.IOQueue
import           Utilities.Networking



-- | Monadic version of 'when'.
whenM :: Monad m => m Bool -> m () -> m ()
whenM mp m = mp >>= \p -> when p m



-- | Monadic version of 'IfThenElse'.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mp x y = mp >>= \p -> if p then x else y



-- | Repeatedly executes a monadic action until its contents evaluate to False.
whileM :: Monad m => (a -> Bool) -> m a -> m ()
whileM p m = go
      where go = m >>= \x -> when (p x) go




-- | To add an \"s\" in print statements if the first argument is 1.
--
--   >>> printf "%d minute%s remaining" n (pluralS n)
pluralS :: (Eq a, Num a) => a -> String
pluralS 1 = ""
pluralS _ = "s"



-- | Merges two lists by alternatingly taking one element of each. Overflow is
--   appended as bulk.
--
--   > mergeLists [a,b] [w,x,y,z]  ==  [a,w,b,x,y,z]
mergeLists :: [a] -> [a] -> [a]
mergeLists []     ys = ys
mergeLists (x:xs) ys = x : mergeLists ys xs



-- | 'T.Text' version of 'show'.
showT :: Show a => a -> T.Text
showT = T.pack . show



-- | Reasonably accurate version of an integer square root, used to model
--   diminishing returns for small values (e.g. neighbour counts).
--
--   This function is total, and returns 0 for inputs smaller than zero.
iSqrt :: Integral int => int -> int
iSqrt n = round (sqrt (max 0 (fromIntegral n :: Double)))