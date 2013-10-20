-- | A client represents one connection to a downstream node.
--

{-# LANGUAGE LambdaCase #-}


module Client  (
      newClient
) where




import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception
import System.IO
import System.Timeout
import Data.Functor
import Control.Monad
import qualified Data.Map as Map



import Types
import Utilities



-- TODO: Make sure the client terminates properly in case something fails,
--       i.e. it is removed from the client pool and the handle is closed
--       if necessary





-- | Initializes a new client, and then starts the client loop. Does not check
--   whether there is any space in the client pool; that's the job of the
--   function that sends the newClient command (i.e. the server).
newClient :: Environment
          -> Handle               -- ^ Connection to use (setup by the handshake
                                  --   process)
          -> To                   -- ^ Target downstream neighbour
          -> TBQueue NormalSignal -- ^ This client's private signal channel
          -> IO ()
newClient env h node stsc = do

      let cleanup = do
            atomically . modifyTVar (_downstream env) $ Map.delete node
            void . timeout (_longTickRate $ _config env) $
                  request h $ Normal ShuttingDown
            hClose h

      stc <- atomically $ dupTChan (_stc env)

      (`finally` cleanup) $ clientLoop env h node [ readTChan stc
                                                  , readTBQueue (_st1c env)
                                                  , readTBQueue stsc
                                                  ]




-- | Listens to a TChan (signals broadcast to all nodes) and a TBQueue (signals
--   meant to be handled by only one client), and executes their orders.
clientLoop :: Environment
           -> Handle             -- ^ Network connection
           -> To                 -- ^ Target downstream neighbour
           -> [STM NormalSignal] -- ^ Actions that read incoming channels
           -> IO ()
clientLoop env h node chans = whileM isContinue $ do

      -- Receive orders from whatever channel is first available
      signal <- Normal <$> atomically (msum chans)

      request h signal >>= \case
            OK     -> ok           env node
            Error  -> genericError env
            Ignore -> ignore       env
            Denied -> denied       env



-- | Response to sending a signal to a server successfully. (Updates the "last
--   successfully sent signal to" timestamp)
ok :: Environment
   -> To          -- ^ Target downstream neighbour
   -> IO Proceed
ok env node = do
      timestamp <- makeTimestamp
      let updateTimestamp client = client { _clientTimestamp = timestamp }
      atomically $ modifyTVar (_downstream env) $
            Map.adjust updateTimestamp node
      return Continue



-- | A downstream node has received a signal from this node without having it
--   in its list of upstream neighbours. A a result, it tells the issuing client
--   that it will ignore its requests.
--
--   The purpose of this is twofold:
--
--     - Nodes can only be contacted by other registered nodes. A malicious
--       network of other nodes cannot nuke a node with illegal requests,
--       because it will just ignore all the illegally created ones.
--
--     - If a node doesn't send a signal for too long, it will time out. When it
--       starts sending new signals, it will be told that it was dropped.
ignore :: Environment -> IO Proceed
ignore env = do
      atomically . toIO env Debug $
            putStrLn "Server ignores this node, terminating client"
      return Terminate



-- | Server sent back a generic error, see docs for 'Error'
genericError :: Environment -> IO Proceed
genericError env = do
      atomically . toIO env Debug $
            putStrLn "Generic server error, terminating client"
      return Terminate



-- | Server denied a valid request, see docs for 'Denied'
denied :: Environment -> IO Proceed
denied env = do
      atomically . toIO env Debug $
            putStrLn "Server denied the request"
      return Terminate