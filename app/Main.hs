module Main where


import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

import Lib (ourAdd)

-- import Text.Printf (printf)

-- main :: IO ()
-- main = printf "2 + 3 = %d\n" (ourAdd 2 3)


main :: IO ()
main = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  _ <- runProcess node $ do
    self <- getSelfPid
    send self "hello"
    hello <- expect :: Process String
    liftIO $ putStrLn hello
  return ()
