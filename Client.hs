module Client where




import Control.Concurrent.STM
import Control.Exception
import Network
import System.IO
import Control.Monad



import Types
import Utilities



-- Initializes a new client, and then spawns the client loop.
newClient :: NodeEnvironment -> Node -> IO ()
newClient ne node = do

      -- Duplicate broadcast chan for the new client
      stc <- atomically $ dupTChan (_stc ne)
      let st1c = _st1c ne

      -- Open connection
      bracket (connectTo (_host node) (PortNumber $ _port node))
              hClose
              (\h -> clientLoop ne h stc st1c)




-- Listens to a TChan (signals broadcast to all nodes) and a TBQueue (signals
-- meant to be handled by only one client), and executes their orders.
clientLoop :: NodeEnvironment
           -> Handle
           -> TChan Signal
           -> TBQueue Signal
           -> IO ()
clientLoop env h stc st1c = untilTerminate $ do

      -- Receive orders from whatever channel is first available
      signal <- atomically $ msum [readTChan stc, readTBQueue st1c]

      -- Listen to the order channels and execute them
      case signal of
            Message {}     -> send h signal
            EdgeRequest {} -> send h signal
            -- TODO: Implement other signals
            _otherwise  -> atomically $ toIO env $
                  putStrLn $ "Error: The signal " ++ show signal ++ " should"
                                        ++ " never have been sent to a client"
                                        ++ debugError "Implement other signals"


      -- TODO: Termination
      --   - If the handle is closed, remove the client from the client pool and
      --     terminate.
      --   - If the client is not in the pool, close the connection. Send a
      --     termination signal to the targeted node so it can adjust its
      --     knownBy set accordingly
      return Continue




