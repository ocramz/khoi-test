module Main where

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

import Control.Concurrent (threadDelay)
import Control.Monad (forever)


-- import System.Environment (getArgs)
import Options.Applicative

import Lib (ourAdd)

-- import Text.Printf (printf)

-- main :: IO ()
-- main = printf "2 + 3 = %d\n" (ourAdd 2 3)

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg




withLocalNode :: Process () -> IO ()
withLocalNode mf = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  _ <- runProcess node mf
  return ()


-- main :: IO ()
-- main = withLocalNode $ do
--     self <- getSelfPid
--     send self "hello"
--     hello <- expect :: Process String
--     liftIO $ putStrLn hello

task1 :: Process ()
task1 = do
    echoPid <- spawnLocal $ forever $ do
      -- Test our matches in order against each message in the queue
      receiveWait [match logMessage, match replyBack]

    -- The `say` function sends a message to a process registered as "logger".
    -- By default, this process simply loops through its mailbox and sends
    -- any received log message strings it finds to stderr.

    say "send some messages!"
    send echoPid "hello"
    self <- getSelfPid
    send echoPid (self, "hello")

    -- `expectTimeout` waits for a message or times out after "delay"
    m <- expectTimeout 1000000
    case m of
      -- Die immediately - throws a ProcessExitException with the given reason.
      Nothing  -> die "nothing came back!"
      Just s -> say $ "got " ++ s ++ " back!"
      
    -- Without the following delay, the process sometimes exits before the messages are exchanged.
    liftIO $ threadDelay 2000000

-- main :: IO ()
-- main = do
--   Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
--   node <- newLocalNode t initRemoteTable
--   _ <- runProcess node $ do
--     self <- getSelfPid
--     send self "hello"
--     hello <- expect :: Process String
--     liftIO $ putStrLn hello
--   return ()





-- | optparse-applicative options


data Options = Options { k :: Int,  -- [s] send period duration
                         l :: Int,  -- [s] grace period "
                         s :: Int}  -- random seed 
             deriving (Eq, Show)

printOptions :: Options -> IO ()
printOptions (Options k' l' s') = do
  putStrLn $ "send period [s] : " ++ show k'
  putStrLn $ "grace period [s] : " ++ show l'
  putStrLn $ "random seed : " ++ show s'


options :: Parser Options
options = Options
     <$> argument auto ( metavar "SEND_PERIOD" <> help "Send period [s] :: Int")
     <*> argument auto ( metavar "GRACE_PERIOD" <> help "Grace period [s] :: Int" )
     <*> argument auto (metavar "RANDOM_SEED" <> help "random seed :: Int")


main :: IO ()
main = execParser opts >>= printOptions
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "khoi-test (c) Marco Zocca, Oct 2016"
     <> header "khoi-test" )
