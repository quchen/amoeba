-- | Functions that are only used for debugging. This file should be safely
--   removable in stable releases.

module Utilities.Debug where




import           Control.Monad
import           Control.Monad.Trans
import           Control.Applicative
import           Control.Exception
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import           System.IO
import           System.Timeout

import Text.Printf



-- | Easily print colored text for debugging
yell :: MonadIO io => Int -> String -> io ()
yell n text = liftIO (printf "\ESC[%dm%d - %s\ESC[0m\n" n n text)



-- | Catch all exceptions, "yell" their contents, and rethrow them.
yellAndRethrow :: (MonadIO io)
               => Int
               -> (String -> String) -- ^ Modify error message, e.g. (++ "foo")
               -> IO ()
               -> io ()
yellAndRethrow n f = liftIO . handle handler
      where handler :: SomeException -> IO ()
            handler (SomeException e) = yell n (f (show e)) >> throw e



-- | Mandatory silly catchall function. Intended to be used as a safety net
--   only, not as a cpeap getaway :-)
catchAll :: IO a -> IO ()
catchAll x = void x `catch` handler
      where handler :: SomeException -> IO ()
            handler _ = return ()
