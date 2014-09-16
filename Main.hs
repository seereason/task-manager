{-# LANGUAGE FlexibleContexts #-}
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (unless)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Map as Map (Map, insert, toList, delete, lookup, null, keys)
import Data.Monoid
import Data.Set (Set, fromList)
import Data.Text.Lazy as Text (Text, empty, pack)
import System.Exit (ExitCode)
import System.IO
import System.Process (CreateProcess, ProcessHandle, shell, interruptProcessGroupOf, terminateProcess)
import System.Process.ListLike (Chunk(..), readProcessChunks)
import System.Process.Text.Lazy ()
import System.Tasks (TopTakes(ManagerStatus, ManagerFinished),
                     ManagerTakes(TopToManager),
                     TopToManager(SendManagerStatus, ShutDown, StartTask, TopToTask),
                     ManagerToTask(SendTaskStatus, CancelTask),
                     TaskTakes,
                     manager)

ePutStr = hPutStr stderr
ePutStrLn = hPutStrLn stderr

type TaskId = Integer

firstTaskId :: TaskId
firstTaskId = 1

main :: IO ()
main = do
  topTakes <- newEmptyMVar
  managerTakes <- newEmptyMVar
  forkIO $ manager firstTaskId topTakes managerTakes
  forkIO $ keyboard managerTakes
  loop topTakes
    where
      loop :: Show TaskId => MVar (TopTakes TaskId) -> IO ()
      loop topTakes = do
         msg <- takeMVar topTakes
         -- ePutStrLn ("top: " ++ show msg)
         case msg of
           ManagerFinished -> return ()
           msg@(ManagerStatus tids mode) ->
               ePutStrLn ("top: " ++ show msg) >> loop topTakes
           _ -> loop topTakes

keyboard :: (Show taskid, Read taskid) => MVar (ManagerTakes taskid) -> IO ()
keyboard managerTakes = do
  input <- getLine
  case input of
    -- Start a new task
    "t" -> putMVar managerTakes (TopToManager (StartTask cmd1 empty)) >> keyboard managerTakes
    -- Get the status of a task
    ['s',d] | isDigit d -> putMVar managerTakes  (TopToManager (TopToTask (read [d]) SendTaskStatus)) >> keyboard managerTakes
    -- Get process manager status
    "s" -> putMVar managerTakes  (TopToManager SendManagerStatus) >> keyboard managerTakes
    -- Kill a task
    ['k',d] | isDigit d -> putMVar managerTakes  (TopToManager (TopToTask (read [d]) CancelTask)) >> keyboard managerTakes
    -- Initiate shutdown and exit keyboard loop
    "x" -> putMVar managerTakes  (TopToManager ShutDown)
    -- error
    x -> ePutStrLn "runKeyboard - expected: t, s, s<digit>, k<digit>, or x" >> keyboard managerTakes

cmd1 = shell $ "bash -c 'echo hello from task 1>&2; for i in " <> intercalate " " (map show ([1..5] :: [Int])) <> "; do echo $i; sleep 1; done'"
cmd2 = shell $ "yes | head -100000"
