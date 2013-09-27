module Main (main) where


import           Control.Monad
import           System.IO
import           Control.Exception

import Utilities
import Types


main :: IO ()
main = do putStrLn "Message sending server."
          putStrLn "Sends a message to localhost:21001."
          getMessage

getMessage = forever $ do
      putStr "> " >> hFlush stdout
      message <- getLine
      let entryNode = To $ Node "localhost" 21001
      bracket (connectToNode entryNode) hClose $ \h -> do
            t <- makeTimestamp
            send' h . Normal . Flood t . TextMessage $ message
            receive' h :: IO ServerResponse