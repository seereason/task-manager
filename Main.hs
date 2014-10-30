{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, Rank2Types, ScopedTypeVariables, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

import Control.Concurrent (MVar)
import Control.Exception (fromException, AsyncException(ThreadKilled), throw, ArithException(LossOfPrecision))
import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Trans (lift)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Monoid
import Data.Text.Lazy as Text (Text)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (shell, proc, ProcessHandle, terminateProcess)
import System.Process.Chunks (Chunk(..))
import System.Process.ListLike (readCreateProcess)
import System.Process.Text.Lazy ()
import System.Tasks (runIO, runProgressIO, manager,
                     TaskId, ProgressAndResult(taskMessage), ManagerTakes(..), TopToManager(..), ManagerToTask(..), TopTakes(..), ManagerToTop(..),
                     TaskToManager(..), TaskTakes, IOPuts(..))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

#if DEBUG
import Debug.Console (ePutStrLn)
import Debug.Show (V(V))
#else
-- Use hPutStrLn instead of the ePutStrLn in Debug.Console.
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO (hPutStrLn, stderr)
ePutStrLn :: MonadIO m => String -> m ()
ePutStrLn = liftIO . hPutStrLn stderr
#endif

-- | The type used to identify tasks in this example
type TID = Integer
instance TaskId TID
-- | The type returned by tasks in this example
type ResultType = Int
-- | The type of progress messages output by tasks in this example
type ProgressType = Chunk Text

-- The message types passed to (taken by) the three main components
-- using MVars:
--   1. Top (the client of the task manager)
--   2. Manager (the thread that coordinates all the task)
--   3. Task (the thread that coordinates a single IO task.)
type ToTop = TopTakes TID (Chunk Text) ResultType
type ToTask = TaskTakes TID (Chunk Text) ResultType
type ToManager = ManagerTakes TID (Chunk Text) ResultType

-- | We must define an instance of ProgressAndResult to be able to
-- receive progress messages from IO tasks.  The taskMessage method
-- returns a value of type @IOPuts progress result@, which can easily
-- be turned into a message to the task wrapper.
instance ProgressAndResult (Chunk Text) ResultType where
    taskMessage (Exception e) | fromException e == Just ThreadKilled = IOCancelled
    taskMessage (Exception e) = IOException e
    taskMessage (Result ExitSuccess) = IOFinished 0
    taskMessage (Result (ExitFailure n)) = IOFinished n
    taskMessage x@(ProcessHandle _h) = IOProgress x
    taskMessage x@(Stdout _) = IOProgress x
    taskMessage x@(Stderr _) = IOProgress x
    -- taskMessage x = ProcessToManager x

instance Show ProcessHandle where
    show _ = "<ProcessHandle>"

deriving instance Show (Chunk Text)

#if DEBUG
instance Pretty (V (Chunk Text)) where
    pPrint (V (Exception e)) = pPrint (V e)
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
ioTasks :: (taskid ~ TID, progress ~ Chunk Text, result ~ ResultType) =>
           [MVar (TaskTakes taskid progress result) -> IO ()]
ioTasks = runIO throwExn : map runProgressIO (countToFive : countToFive' : nekos)

-- | Set up a state monad to manage the allocation of task ids, and to
-- allow the "t" command to cycle through the list of tasks to be run.
main :: IO ()
main = manager (`evalStateT` (ioTasks, 1)) keyboard output

-- The remaining definitions are IO tasks the example runs.
throwExn :: IO result
throwExn = ePutStrLn "About to throw an exception" >> throw LossOfPrecision

-- 'readCreateProcess' catches all exceptions and returns them as
-- Chunk values in the output list.  This scans through the chunks
-- throws any exceptions it finds.  (This needs to send a task cancelled
-- message.)  (This seems to be strict, why?)
throwChunks :: [Chunk Text] -> IO [Chunk Text]
throwChunks xs =
    evalStateT (mapM_ throwChunk xs) Nothing >> return xs
    where throwChunk :: Chunk Text -> StateT (Maybe ProcessHandle) IO (Chunk Text)
          throwChunk x@(ProcessHandle h) = put (Just h) >> return x
          throwChunk (Exception e) = do
            -- lift (ePutStrLn ("throwChunk " ++ show e))
            -- Should the ThreadKilled exception just below kill the
            -- process?  It doesn't seem to, hence this terminateProcess.
            get >>= maybe (return ()) (lift . terminateProcess)
            throw e
          throwChunk x = return x

countToFive :: IO [Chunk Text]
countToFive = readCreateProcess (shell $ "bash -c 'echo hello from task 1>&2; for i in " <> intercalate " " (map show ([1..5] :: [Int])) <> "; do echo $i; sleep 1; done'") mempty

-- Re-throw any exceptions that were caught.  If this is killed it
-- gets left in the task map.  After that, manager shutdown blocks.
-- Also, all of the output from this comes out at once.
countToFive' :: IO [Chunk Text]
countToFive' = readCreateProcess (shell $ "bash -c 'echo hello from task 1>&2; for i in " <> intercalate " " (map show ([1..5] :: [Int])) <> "; do echo $i; sleep 1; done'") mempty >>=
               throwChunks

million :: IO [Chunk Text]
million = readCreateProcess (shell $ "yes | head -1000000") mempty
neko :: String -> Int -> IO [Chunk Text]
neko color speed = readCreateProcess (proc "oneko" ["-fg", color, "-speed", show speed]) mempty >>= throwChunks
nekos :: [IO [Chunk Text]]
nekos = map (uncurry neko) (zip (concat (repeat ["red", "green", "blue", "black", "yellow"])) (concat (repeat [12..18])))
