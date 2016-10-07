{-# language DeriveDataTypeable, DeriveGeneric #-}
{-# language TemplateHaskell #-}
module Lib
    -- (
    --   ourAdd
    -- )
    where

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import Text.Printf
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

-- import System.Environment
       -- import Network.Socket hiding (shutdown)


-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y


data Message = Ping ProcessId
             | Pong ProcessId
  deriving (Typeable, Generic)          -- 1

instance Binary Message                 -- 2

pingServer :: Process ()
pingServer = do
  Ping from <- expect                              -- 1
  say $ printf "ping received from %s" (show from) -- 2
  mypid <- getSelfPid                              -- 3
  send from (Pong mypid)                           -- 4  

remotable ['pingServer]


-- master :: Process ()
-- master = do
--   node <- getSelfNode                               -- 1

--   say $ printf "spawning on %s" (show node)
--   pid <- spawn node $(mkStaticClosure 'pingServer)  -- 2

--   mypid <- getSelfPid                               -- 3
--   say $ printf "sending ping to %s" (show pid)
--   send pid (Ping mypid)                             -- 4

--   Pong _ <- expect                                  -- 5
--   say "pong."

--   terminate





-- main :: IO ()
-- main = distribMain (\_ -> master) Lib.__remoteTable



-- distribMain :: ([NodeId] -> Process ()) -> (RemoteTable -> RemoteTable) -> IO ()
-- distribMain master frtable = do
--   args <- getArgs
--   let rtable = frtable initRemoteTable

--   case args of
--     [] -> do
--       backend <- initializeBackend defaultHost defaultPort rtable
--       startMaster backend master
--     [ "master" ] -> do
--       backend <- initializeBackend defaultHost defaultPort rtable
--       startMaster backend master
--     [ "master", port ] -> do
--       backend <- initializeBackend defaultHost port rtable
--       startMaster backend master
--     [ "slave" ] -> do
--       backend <- initializeBackend defaultHost defaultPort rtable
--       startSlave backend
--     [ "slave", port ] -> do
--       backend <- initializeBackend defaultHost port rtable
--       startSlave backend
--     [ "slave", host, port ] -> do
--       backend <- initializeBackend host port rtable
--       startSlave backend

-- defaultHost = "localhost"
-- defaultPort = "44444"
