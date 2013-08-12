module Client  (
      forkNewClient
) where




import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception
import System.IO
import Control.Monad
import qualified Data.Map as Map



import Types
import Utilities (untilTerminate, send, toIO, connectToNode, makeTimestamp)



-- TODO: Make sure the client terminates properly in case something fails,
--       i.e. it is removed from the client pool and the handle is closed
--       if necessary



-- Starts a new client in a separate thread.
forkNewClient :: Environment -> Node -> IO ()
forkNewClient env targetNode = do
      let -- Shortcut function to add/delete nodes to the database
          updateKnownNodes = atomically . modifyTVar (_downstream env)

      -- Setup queue for talking directly to the speicif client created here
      stsc <- newTBQueueIO (_maxChanSize $ _config env)


      withAsync (newClient env targetNode stsc) $ \thread -> do
            timestamp <- makeTimestamp
            let client = Client timestamp thread stsc
            updateKnownNodes $ Map.insert targetNode client
            wait thread
                  `finally` updateKnownNodes (Map.delete targetNode)





-- Initializes a new client, and then starts the client loop. Does not check
-- whether there is any space in the client pool; that's the job of the function
-- that sends the newClient command (i.e. the server).
newClient :: Environment -> Node -> TBQueue Signal -> IO ()
newClient env targetNode stsc =
      bracket (connectToNode targetNode) hClose $ \h -> do
            send h IAddedYou
            stc <- atomically $ dupTChan (_stc env)
            clientLoop env h stc (_st1c env) stsc



-- Listens to a TChan (signals broadcast to all nodes) and a TBQueue (signals
-- meant to be handled by only one client), and executes their orders.
clientLoop :: Environment
           -> Handle
           -> TChan Signal   -- ^ Server to all clients
           -> TBQueue Signal -- ^ Server to arbitrary client
           -> TBQueue Signal -- ^ Server to this specific client
           -> IO ()
clientLoop env h stc st1c stsc = untilTerminate $ do

      -- Receive orders from whatever channel is first available
      signal <- atomically $ msum [ readTChan stc
                                  , readTBQueue st1c
                                  , readTBQueue stsc
                                  ]

      -- TODO: terminate if the handle is closed

      -- Execute incoming actions
      case signal of
            TextMessage {} -> send h signal
            EdgeRequest {} -> send h signal
            -- TODO: Implement other signals
            _otherwise  -> atomically $ toIO env Debug $
                  putStrLn $ "Error: The signal " ++ show signal ++ " should"
                                        ++ " never have been sent to a client"
                                        ++ error "TODO: Implement other signals"


      -- TODO: Termination
      --   - If the handle is closed, remove the client from the client pool and
      --     terminate.
      --   - If the client is not in the pool, close the connection. Send a
      --     termination signal to the targeted node so it can adjust its
      --     knownBy set accordingly
      return Continue




