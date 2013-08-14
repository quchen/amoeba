-- | A client represents one connection to a downstream node.
--

{-# LANGUAGE LambdaCase #-}


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
import Utilities



-- TODO: Make sure the client terminates properly in case something fails,
--       i.e. it is removed from the client pool and the handle is closed
--       if necessary



-- Starts a new client in a separate thread.
forkNewClient :: Environment -> Node -> IO ()
forkNewClient env targetNode = do

      -- Setup queue for talking directly to the speicif client created here.
      -- STSC = Server to Single Client
      stsc <- newTBQueueIO (_maxChanSize $ _config env)

      withAsync (newClient env targetNode stsc) $ \thread -> do
            timestamp <- makeTimestamp
            let client = Client timestamp thread stsc
            atomically . modifyTVar (_downstream env) $
                                                    Map.insert targetNode client





-- Initializes a new client, and then starts the client loop. Does not check
-- whether there is any space in the client pool; that's the job of the function
-- that sends the newClient command (i.e. the server).
newClient :: Environment -> Node -> TBQueue NormalSignal -> IO ()
newClient env targetNode stsc =
      let release h = do
               atomically . modifyTVar (_downstream env) $ Map.delete targetNode
               hClose h

      in bracket (connectToNode targetNode) release $ \h -> do
               send h (Normal IAddedYou)
               stc <- atomically $ dupTChan (_stc env)
               clientLoop env h [ readTChan stc
                                , readTBQueue (_st1c env)
                                , readTBQueue stsc
                                ]



-- Listens to a TChan (signals broadcast to all nodes) and a TBQueue (signals
-- meant to be handled by only one client), and executes their orders.
clientLoop :: Environment
           -> Handle
           -> [STM NormalSignal] -- ^ Actions that read incoming channels
           -> IO ()
clientLoop env h chans = untilTerminate $ do

      -- Receive orders from whatever channel is first available
      send h =<< atomically (msum chans)

      receive h >>= \case
            OK     -> return Continue
            Error  -> genericError env
            Ignore -> ignore env




-- | A downstream node has received a signal from this node without having it
--   in its list of upstream neighbours. A a result, it tells the issuing client
--   that it will ignore its requests.
--
-- The purpose of this is twofold:
--
--   - Nodes can only be contacted by other registered nodes. A malicious
--     network of other nodes cannot nuke a node with illegal requests, because
--     it will just ignore all the illegally created ones.
--
--   - If a node doesn't send a signal for too long, it will time out. When it
--     starts sending new signals, it will be told that it was dropped.
ignore :: Environment -> IO Proceed
ignore env = do
      atomically . toIO env Debug $
            putStrLn "Server ignores this node, terminating client"
      return Terminate


-- | Server sent back a generic error
genericError :: Environment -> IO Proceed
genericError env = do
      atomically . toIO env Debug $
            putStrLn "Generic server error, terminating client"
      return Terminate