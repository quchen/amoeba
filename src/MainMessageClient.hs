module Main (main) where


import Control.Monad
import qualified Pipes.Network.TCP as P
import Pipes
import Data.Binary (Binary)
import qualified Data.ByteString as BS
import qualified Pipes.Prelude as P
import qualified Pipes.Binary as P

import Utilities (connectToNode, makeTimestamp, encodeMany)
import Types



server = To $ Node "127.0.0.1" 20000



main :: IO ()
main = do
      putStrLn "Message injection client"
      putStrLn "Enter \"quit\" to quit"
      putStrLn ""
      connectToNode server $ \(socket, addr) -> do
            putStrLn $ "Connected to " ++ show server
            runEffect $ P.stdinLn >-> handle >-> encodeMany >-> send socket



handle :: MonadIO io => Pipe String Signal io ()
handle = P.takeWhile (/= "quit") >-> P.mapM dispatch
      where dispatch msg = do
                  liftIO $ putStrLn $ "Sending \"" ++ msg ++ "\""
                  signal msg
            signal msg = do
                  t <- makeTimestamp
                  return $ (Normal . Flood t . TextMessage) msg



send :: (MonadIO io) => P.Socket -> Consumer BS.ByteString io ()
send = P.toSocket
