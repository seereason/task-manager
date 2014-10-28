{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GADTs, Rank2Types, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Concurrent (MVar, putMVar)
import Control.Exception (throw, ArithException(LossOfPrecision))
import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Trans (lift)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Monoid
import Data.Text.Lazy as Text (Text)
import System.Process (CreateProcess, shell, proc, terminateProcess)
import System.Process.Chunks (Chunk(Exception, ProcessHandle))
import System.Process.ListLike (ListLikeLazyIO, readCreateProcess)
import System.Process.Text.Lazy ()
import System.Tasks (manager)
import System.Tasks.Types (TaskId, Progress, Result, ManagerTakes(..), TopToManager(..), ManagerToTask(..), TopTakes(..), ManagerToTop(..), TaskToManager(..), TaskTakes(ProcessToTask))
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
instance Progress (Chunk Text)
type ResultType = Int
instance Result ResultType

main :: IO ()
main = manager (`evalStateT` (cmds, 1)) keyboard output

-- | The output device
output :: TopTakes TID (Chunk Text) ResultType -> IO ()
output (TopTakes ManagerFinished) = ePutStrLn "ManagerFinished"
output (TopTakes (ManagerStatus tasks status)) = ePutStrLn $ "ManagerStatus " ++ show tasks ++ " " ++ show status
output (TopTakes (NoSuchTask taskid)) = ePutStrLn $ "NoSuchTask " ++ show taskid
output (TopTakes (TaskStatus taskid status)) = ePutStrLn $ "TaskStatus " ++ show taskid ++ " " ++ show status
output (TopTakes (TaskToTop (TaskCancelled taskid))) = ePutStrLn $ "TaskCancelled " ++ show taskid
output (TopTakes (TaskToTop (TaskFinished taskid result))) = ePutStrLn $ "TaskFinished " ++ show taskid ++ " -> " ++ show result
output (TopTakes (TaskToTop (ProcessToManager taskid chunk))) = ePutStrLn $ "ProcessOutput " ++ show taskid ++ " " ++ show chunk

-- | The input device
keyboard :: StateT ([MVar (TaskTakes TID (Chunk Text) ) -> IO ResultType], TID) IO (ManagerTakes TID (Chunk Text) ResultType)
keyboard = do
  input <- lift $ getLine
  case input of
    -- Start a new task
    "t" -> get >>= \ ((cmd : more), nextId) -> put (more, succ nextId) >> return (TopToManager (StartTask nextId cmd))
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

-- | The sequence of tasks that t will run.
cmds :: [MVar (TaskTakes TID (Chunk Text)) -> IO ResultType]
cmds = throwExn : run' countToFive : map run' nekos

throwExn :: MVar (TaskTakes taskid progress) -> IO Int
throwExn _ = ePutStrLn "About to throw an exception" >> throw LossOfPrecision
countToFive :: CreateProcess
countToFive = shell $ "bash -c 'echo hello from task 1>&2; for i in " <> intercalate " " (map show ([1..5] :: [Int])) <> "; do echo $i; sleep 1; done'"
million :: CreateProcess
million = shell $ "yes | head -1000000"
neko :: String -> Int -> CreateProcess
neko color speed = proc "oneko" ["-fg", color, "-speed", show speed]
nekos :: [CreateProcess]
nekos = map (uncurry neko) (zip (concat (repeat ["red", "green", "blue", "black", "yellow"])) (concat (repeat [12..18])))

run :: (ListLikeLazyIO a c, a ~ Text, progress ~ Chunk Text, TaskId taskid) =>
       CreateProcess -> a -> MVar (TaskTakes taskid progress) -> IO ResultType
run cmd input taskTakes =
  do (ProcessHandle _pid : chunks) <- readCreateProcess cmd input
     mapM_ (putMVar taskTakes . ProcessToTask) chunks
     mapM_ (\ chunk -> case chunk of
                         Exception e -> do
#if DEBUG
                           ePutStrLn "throw chunk!"
#endif
                           throw e
                         _x -> return ()) chunks
     return 123

run' :: (TaskId taskid, progress ~ Chunk Text) => CreateProcess -> MVar (TaskTakes taskid progress) -> IO ResultType
run' cmd taskTakes = run cmd mempty taskTakes

