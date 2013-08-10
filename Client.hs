module Client  (
      forkNewClient
) where




import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception
import Network
import System.IO
import Control.Monad
import qualified Data.Map as Map



import Types
import Utilities (untilTerminate, send, toIO, connectToNode)



-- Starts a new client in a separate thread.
forkNewClient :: Environment -> Node -> IO ()
forkNewClient env node = do
      let -- Shortcut function to add/delete nodes to the database
          updateKnownNodes = atomically . modifyTVar (_knownNodes env)
      withAsync (newClient env node) $ \nodeAsync -> do
            updateKnownNodes $ Map.insert node nodeAsync
            wait nodeAsync



-- Initializes a new client, and then starts the client loop. Does not check
-- whether there is any space in the client pool; that's the job of the function
-- that sends the newClient command (i.e. the server).
newClient :: Environment -> Node -> IO ()
newClient env node =
      bracket (connectToNode node) hClose $ \h -> do
            send h IAddedYou
            stc <- atomically $ dupTChan (_stc env)
            clientLoop env h stc (_st1c env)



-- Listens to a TChan (signals broadcast to all nodes) and a TBQueue (signals
-- meant to be handled by only one client), and executes their orders.
clientLoop :: Environment
           -> Handle
           -> TChan Signal
           -> TBQueue Signal
           -> IO ()
clientLoop env h stc st1c = untilTerminate $ do

      -- Receive orders from whatever channel is first available
      signal <- atomically $ msum [readTChan stc, readTBQueue st1c]

      -- Listen to the order channels and execute them
      case signal of
            TextMessage {} -> send h signal
            EdgeRequest {} -> send h signal
            -- TODO: Implement other signals
            _otherwise  -> atomically $ toIO env $
                  putStrLn $ "Error: The signal " ++ show signal ++ " should"
                                        ++ " never have been sent to a client"
                                        ++ error "Implement other signals"


      -- TODO: Termination
      --   - If the handle is closed, remove the client from the client pool and
      --     terminate.
      --   - If the client is not in the pool, close the connection. Send a
      --     termination signal to the targeted node so it can adjust its
      --     knownBy set accordingly
      return Continue




