{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, Rank2Types, ScopedTypeVariables, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

import Control.Concurrent (MVar)
import Control.Exception (fromException, throw, ArithException(LossOfPrecision), AsyncException(ThreadKilled))
import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Trans (lift, liftIO)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Monoid
import Data.Text.Lazy as Text (Text)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (shell, proc, ProcessHandle)
-- In order for the IO task to be cancellable we need to be able to
-- intercept the ThreadKilled exception and turn it into an
-- IOCancelled task message (in the ProgressAndResult instance.)
-- Therefore, we use the ChunkE module, which delivers exceptions in
-- the output stream, rather than Chunks which re-throws them.
import qualified System.Process.ChunkE as C (Chunk(..))
-- import qualified System.Process.Chunks as C (Chunk(..))
import System.Process.ListLike (readCreateProcess)
import System.Process.Text.Lazy ()
import System.Tasks (runIO, {-runProgressIO,-} runCancelIO, manager,
                     TaskId, ProgressAndResult(taskMessage), ManagerTakes(..), TopToManager(..), ManagerToTask(..), TopTakes(..), ManagerToTop(..),
                     TaskToManager(..), TaskTakes, IOPuts(..))

#if DEBUG
import Debug.Console (ePutStrLn)
import Debug.Show (V(V))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
#else
-- Use hPutStrLn instead of the ePutStrLn in Debug.Console.
import Control.Monad.Trans (MonadIO)
import System.IO (hPutStrLn, stderr)
import System.Tasks (ManagerStatus(..))
ePutStrLn :: MonadIO m => String -> m ()
ePutStrLn = liftIO . hPutStrLn stderr
deriving instance Show ManagerStatus
#endif

-- | The type used to identify tasks in this example
type TID = Integer
instance TaskId TID
-- | The type returned by tasks in this example
type ResultType = Int
-- | The type of progress messages output by tasks in this example
type ProgressType = C.Chunk Text

-- The message types passed to (taken by) the three main components
-- using MVars:
--   1. Top (the client of the task manager)
--   2. Manager (the thread that coordinates all the task)
--   3. Task (the thread that coordinates a single IO task.)
type ToTop = TopTakes TID ProgressType ResultType
type ToTask = TaskTakes TID ProgressType ResultType
type ToManager = ManagerTakes TID ProgressType ResultType

-- | We must define an instance of ProgressAndResult to be able to
-- receive progress messages from IO tasks.  The taskMessage method
-- returns a value of type @IOPuts progress result@, which can easily
-- be turned into a message to the task wrapper.
instance ProgressAndResult ProgressType ResultType where
    taskMessage (C.Exception e) | fromException e == Just ThreadKilled = IOCancelled
    taskMessage (C.Exception e) = IOException e
    taskMessage (C.Result ExitSuccess) = IOFinished 0
    taskMessage (C.Result (ExitFailure n)) = IOFinished n
    taskMessage x@(C.ProcessHandle _h) = IOProgress x
    taskMessage x@(C.Stdout _) = IOProgress x
    taskMessage x@(C.Stderr _) = IOProgress x
    -- taskMessage x = ProcessToManager x

instance Show ProcessHandle where
    show _ = "<ProcessHandle>"

deriving instance Show ProgressType

#if DEBUG
instance Pretty (V ProgressType) where
    -- pPrint (V (Exception e)) = pPrint (V e)
    pPrint (V x) = text (show x)
#endif

-- | The output device in this example just sends the messages it
-- receives to the console.
output :: ToTop -> IO ()
output (TopTakes ManagerFinished) = ePutStrLn "ManagerFinished"
output (TopTakes (ManagerStatus tasks status)) = ePutStrLn $ "ManagerStatus " ++ show tasks ++ " " ++ show status
output (TopTakes (NoSuchTask taskid)) = ePutStrLn $ show taskid ++ ": NoSuchTask"
output (TopTakes (TaskStatus taskid status)) = ePutStrLn $ show taskid ++ ": TaskStatus " ++ show status
output (TopTakes (TaskToTop (TaskPuts taskid IOCancelled))) = ePutStrLn $ show taskid ++ ": IOCancelled"
output (TopTakes (TaskToTop (TaskPuts taskid (IOException e)))) = ePutStrLn $ show taskid ++ ": IOException " ++ show e
output (TopTakes (TaskToTop (TaskPuts taskid (IOFinished result)))) = ePutStrLn $ show taskid ++ ": IOFinished " ++ show result
output (TopTakes (TaskToTop (TaskPuts taskid (IOProgress chunk)))) = ePutStrLn $ show taskid ++ ": IOProgress " ++ show chunk

-- | The input device maps several keyboard inputs to messages that
-- will be sent to the task manager.
keyboard :: StateT ([MVar ToTask -> IO ()], TID) IO ToManager
keyboard = do
  input <- lift $ getLine
  case input of
    -- Start a new task
    "t" -> get >>= \ ((cmd : more), nextId) -> put (more, succ nextId) >> return (TopToManager (StartTask nextId cmd))
    -- Start an IO 123
    "a" -> get >>= \ (cmds, nextId) -> put (cmds, succ nextId) >> return (TopToManager (StartTask nextId (runIO (return 123))))
    -- Throw an exception
    "e" -> get >>= \ (cmds, nextId) -> put (cmds, succ nextId) >> return (TopToManager (StartTask nextId (runIO (throw LossOfPrecision))))
    -- Get the status of a task
    ['s',d] | isDigit d -> return (TopToManager (SendTaskStatus (read [d])))
    -- Get process manager status
    "s" -> return (TopToManager SendManagerStatus)
    -- Kill a task
    ['k',d] | isDigit d -> return (TopToManager $ TopToTask $ CancelTask $ read [d])
    -- Initiate shutdown and exit keyboard loop
    "x" -> return (TopToManager ShutDown)
    -- error
    x -> ePutStrLn (show x ++ " - expected: t, a, e, s, s<digit>, k<digit>, or x") >> keyboard

-- | The sequence of IO operations that the "t" command will run,
-- wrapped up to communicate with a task thread.
ioTasks :: (taskid ~ TID, progress ~ C.Chunk Text, result ~ ResultType) =>
           [MVar (TaskTakes taskid progress result) -> IO ()]
ioTasks = runIO throwExn : map (runCancelIO (flip evalStateT Nothing)) (map ioWithHandle (countToFive : nekos))

ioWithHandle :: IO [C.Chunk Text] -> StateT (Maybe ProcessHandle) IO [C.Chunk Text]
ioWithHandle p = do
  (C.ProcessHandle h : chunks) <- liftIO p
  put (Just h)
  return chunks

-- | Set up a state monad to manage the allocation of task ids, and to
-- allow the "t" command to cycle through the list of tasks to be run.
main :: IO ()
main = manager (`evalStateT` (ioTasks, 1)) keyboard output

-- The remaining definitions are IO tasks the example runs.
throwExn :: IO result
throwExn = ePutStrLn "About to throw an exception" >> throw LossOfPrecision

countToFive :: IO [C.Chunk Text]
countToFive = readCreateProcess (shell $ "bash -c 'echo hello from task 1>&2; for i in " <> intercalate " " (map show ([1..5] :: [Int])) <> "; do echo $i; sleep 1; done'") mempty

million :: IO [C.Chunk Text]
million = readCreateProcess (shell $ "yes | head -1000000") mempty
neko :: String -> Int -> IO [C.Chunk Text]
neko color speed = readCreateProcess (proc "oneko" ["-fg", color, "-speed", show speed]) mempty
nekos :: [IO [C.Chunk Text]]
nekos = map (uncurry neko) (zip (concat (repeat ["red", "green", "blue", "black", "yellow"])) (concat (repeat [12..18])))
