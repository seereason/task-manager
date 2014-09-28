{-# LANGUAGE CPP, FlexibleContexts, Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Concurrent (forkIO, MVar, newEmptyMVar)
#ifdef DEBUG
import System.Tasks.Pretty (putMVar, takeMVar)
#else
import Control.Concurrent (putMVar, takeMVar)
#endif
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Monoid
import Data.Text.Lazy as Text (empty)
import Debug.Console (ePutStrLn)
import System.Process (CreateProcess, shell, proc)
import System.Process.Text.Lazy ()
import System.Tasks (manager)
import System.Tasks.Types (TopTakes(..), ManagerToTop(..), ManagerTakes(..),
                           TopToManager(..), ManagerToTask(..))

type TaskId = Integer

firstTaskId :: TaskId
firstTaskId = 1

main :: IO ()
main = do
  ePutStrLn "top\t\tmanager\t\ttask\t\tprocess"
  topTakes <- newEmptyMVar
  managerTakes <- newEmptyMVar
  forkIO $ manager firstTaskId topTakes managerTakes
  forkIO $ keyboard managerTakes
  top topTakes

top :: Show TaskId => MVar (TopTakes TaskId) -> IO ()
top topTakes = loop
    where
      loop = do
        msg <- takeMVar topTakes
        ePutStrLn (show msg)
        case msg of
          TopTakes ManagerFinished -> return ()
          TopTakes (ManagerStatus _tids _mode) -> loop
          _ -> loop

keyboard :: (Show taskid, Read taskid) =>
            MVar (ManagerTakes taskid)
         -> IO ()
keyboard managerTakes =
    loop (countToFive : nekos)
    where
      loop cmds@(cmd : next) =
          do input <- getLine
             case input of
               -- Start a new task
               "t" -> putMVar managerTakes (TopToManager (StartTask cmd empty)) >> loop next
               -- Get the status of a task
               ['s',d] | isDigit d -> putMVar managerTakes (TopToManager (SendTaskStatus (read [d]))) >> loop cmds
               -- Get process manager status
               "s" -> putMVar managerTakes  (TopToManager SendManagerStatus) >> loop cmds
               -- Kill a task
               ['k',d] | isDigit d -> putMVar managerTakes  (TopToManager $ TopToTask $ CancelTask $ read [d]) >> loop cmds
               -- Initiate shutdown and exit keyboard loop
               "x" -> putMVar managerTakes  (TopToManager ShutDown)
               -- error
               x -> ePutStrLn (show x ++ " - expected: t, s, s<digit>, k<digit>, or x") >> loop cmds

countToFive :: CreateProcess
countToFive = shell $ "bash -c 'echo hello from task 1>&2; for i in " <> intercalate " " (map show ([1..5] :: [Int])) <> "; do echo $i; sleep 1; done'"
million :: CreateProcess
million = shell $ "yes | head -1000000"
neko :: String -> Int -> CreateProcess
neko color speed = proc "oneko" ["-fg", color, "-speed", show speed]
nekos :: [CreateProcess]
nekos = map (uncurry neko) (zip (concat (repeat ["red", "green", "blue", "black", "yellow"])) (concat (repeat [12..18])))
