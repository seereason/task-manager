{-# LANGUAGE CPP, FlexibleContexts, GADTs, Rank2Types, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Trans (lift)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Monoid
import Data.Text.Lazy as Text (empty)
import System.Process (CreateProcess, shell, proc)
import System.Process.Text.Lazy ()
import System.Tasks (manager)
import System.Tasks.Types (TaskId, ManagerTakes(..), TopToManager(..), ManagerToTask(..), TopTakes(..), ManagerToTop(..), TaskToManager(..))
import System.Tasks.Pretty ()

#if DEBUG
import Debug.Console (ePutStrLn)
#else
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO (hPutStrLn, stderr)
ePutStrLn :: MonadIO m => String -> m ()
ePutStrLn = liftIO . hPutStrLn stderr
#endif

type TID = Integer

instance TaskId TID

main :: IO ()
main = manager (`evalStateT` (cmds, 1)) keyboard output

-- | The output device
output :: TopTakes TID -> IO ()
output (TopTakes ManagerFinished) = ePutStrLn "ManagerFinished"
output (TopTakes (ManagerStatus tasks status)) = ePutStrLn $ "ManagerStatus " ++ show tasks ++ " " ++ show status
output (TopTakes (NoSuchTask taskid)) = ePutStrLn $ "NoSuchTask " ++ show taskid
output (TopTakes (TaskStatus taskid status)) = ePutStrLn $ "TaskStatus " ++ show taskid ++ " " ++ show status
output (TopTakes (TaskToTop (TaskCancelled taskid))) = ePutStrLn $ "TaskCancelled " ++ show taskid
output (TopTakes (TaskToTop (TaskFinished taskid))) = ePutStrLn $ "TaskFinished " ++ show taskid
output (TopTakes (TaskToTop (ProcessToManager taskid chunk))) = ePutStrLn $ "ProcessOutput " ++ show taskid ++ " " ++ show chunk

-- | The input device
keyboard :: StateT ([CreateProcess], TID) IO (ManagerTakes TID)
keyboard = do
  input <- lift $ getLine
  case input of
    -- Start a new task
    "t" -> get >>= \ ((cmd : more), nextId) -> put (more, succ nextId) >> return (TopToManager (StartTask nextId cmd empty))
    -- Get the status of a task
    ['s',d] | isDigit d -> return (TopToManager (SendTaskStatus (read [d])))
    -- Get process manager status
    "s" -> return (TopToManager SendManagerStatus)
    -- Kill a task
    ['k',d] | isDigit d -> return (TopToManager $ TopToTask $ CancelTask $ read [d])
    -- Initiate shutdown and exit keyboard loop
    "x" -> return (TopToManager ShutDown)
    -- error
    x -> ePutStrLn (show x ++ " - expected: t, s, s<digit>, k<digit>, or x") >> keyboard

cmds :: [CreateProcess]
cmds = countToFive : nekos

countToFive :: CreateProcess
countToFive = shell $ "bash -c 'echo hello from task 1>&2; for i in " <> intercalate " " (map show ([1..5] :: [Int])) <> "; do echo $i; sleep 1; done'"
million :: CreateProcess
million = shell $ "yes | head -1000000"
neko :: String -> Int -> CreateProcess
neko color speed = proc "oneko" ["-fg", color, "-speed", show speed]
nekos :: [CreateProcess]
nekos = map (uncurry neko) (zip (concat (repeat ["red", "green", "blue", "black", "yellow"])) (concat (repeat [12..18])))
