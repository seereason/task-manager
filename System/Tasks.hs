{-# LANGUAGE FlexibleContexts, CPP, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeSynonymInstances #-}
-- | Create a manager in a thread which will handle a set of tasks.  Tasks can be
-- started, their status can be queried, and they can be cancelled.
--
-- The system has four components:
--
--    1. Top is the external system that wants processes managed
--    2. Manager controls the set of concurrent tasks
--    3. Task controls a single process
--    4. Process is one of the the things that Top wanted managed
--
-- The Manager and each task has is a loop running in an IO thread
-- which receives and handles incoming messages.  The values passed in
-- each thread are named after the component: 'TaskTakes',
-- 'ManagerTakes', and so on.  Inside these types are types describing
-- the specific path the message took: 'ManagerToTask',
-- 'ProcessToTask'.  Within these are specific message types, such as
-- 'ShutDown' or 'SendTaskStatus'.

module System.Tasks
    ( TopTakes(ManagerStatus, ManagerFinished)
    , ManagerTakes(TopToManager)
    , TopToManager(SendManagerStatus, ShutDown, StartTask, TopToTask)
    , ManagerToTask(SendTaskStatus, CancelTask)
    , TaskTakes
    , manager
    ) where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import qualified Data.ByteString.Lazy as L
import Data.List (intercalate)
import Data.Map as Map (Map, insert, toList, delete, lookup, null, keys)
import Data.Monoid
import Data.Set (Set, fromList)
import Data.Text.Lazy as Text (Text, pack)
import System.Exit (ExitCode)
import System.IO
import System.IO.Unsafe
import System.Process (ProcessHandle, shell, proc, terminateProcess, CreateProcess(..), CmdSpec(..), createProcess, StdStream(CreatePipe))
import System.Process.ListLike (ListLikePlus, Chunk(..), showCmdSpecForUser)
import System.Process.ListLike.Ready (readProcessInterleaved)
--import System.Process.Text.Lazy ()
--import System.Process.ListLike.Ready (readProcessChunks)

console :: String -> IO ()
console = unsafePerformIO $ do
  v <- newEmptyMVar
  forkIO (loop v)
  return (putMVar v)
    where loop v = takeMVar v >>= hPutStr stderr >> loop v

ePutStr s = liftIO (console s)
ePutStrLn s = ePutStr (s <> "\n")

-- Types describing messages between two components

type ProcessToTask = Chunk Text

instance Show CmdSpec where
    show x = "(CmdSpec " ++ showCmdSpecForUser x ++ ")"

instance Show CreateProcess where
    show x = "(CreateProcess " ++ show (cmdspec x) ++ ")"

data ManagerToTask
    = SendTaskStatus
    | CancelTask
    deriving (Show, Eq)

data TopToManager taskid
    = StartTask CreateProcess Text
    | SendManagerStatus
    | ShutDown
    | TopToTask taskid ManagerToTask
    deriving Show

data TaskToManager taskid
    = ProcessToManager taskid ProcessToTask
    | ProcessStatus taskid TaskState
    | ProcessFinished taskid
    deriving Show

data TopTakes taskid
    = ManagerFinished
    | ManagerStatus (Set taskid) Mode
    | NoSuchTask taskid
    | TaskToTop (TaskToManager taskid)
    deriving Show

-- | A type describing the messages a task might receive - either from
-- the task manager or from the process it is managing.
data TaskTakes
    = ManagerToTask ManagerToTask
    | ProcessToTask ProcessToTask
    deriving Show

-- | A type describing the messages the task manager might receive -
-- either from the top or from one of the tasks being managed.
data ManagerTakes taskid
    = TopToManager (TopToManager taskid)
    | TaskToManager (TaskToManager taskid)
    deriving Show

data ManagerState taskid
    = ManagerState
      { managerMode :: Mode
      , nextTaskId :: taskid
      , mvarMap :: Map taskid (MVar TaskTakes)
      }

-- | The manager has two modes, currently the same as the task status.
-- Normally it keeps running whether there are any running tasks or not,
-- in exiting mode it exits as soon as the last task finishes.
data Mode
    = Running'
    | Exiting'
    deriving (Show, Eq)

manager :: forall taskid. (Show taskid, Read taskid, Enum taskid, Ord taskid) => taskid -> MVar (TopTakes taskid) -> MVar (ManagerTakes taskid) -> IO ()
manager firstTaskId topTakes managerTakes = do
  evalStateT loop (ManagerState {managerMode = Running', nextTaskId = firstTaskId, mvarMap = mempty})
    where
      loop :: StateT (ManagerState taskid) IO ()
      loop = do
        st <- get
        case (managerMode st, Map.null (mvarMap st)) of
          (Exiting', True) -> lift $ putMVar topTakes ManagerFinished
          _ -> do
               msg <- lift $ takeMVar managerTakes
               ePutStrLn ("managerTakes: " ++ show msg)
               case msg of
                 TopToManager (StartTask cmd input) -> do -- Start a new task
                   taskTakes <- lift newEmptyMVar
                   _tid <- lift $ forkIO $ task (nextTaskId st) managerTakes taskTakes cmd input
                   put (st {nextTaskId = succ (nextTaskId st), mvarMap = Map.insert (nextTaskId st) taskTakes (mvarMap st)})
                 TopToManager SendManagerStatus -> do -- Send top the manager status (Running or Exiting)
                   lift $ putMVar topTakes (ManagerStatus (fromList (keys (mvarMap st))) (managerMode st))
                 TopToManager ShutDown -> do -- Tell all the tasks to shut down
                   lift $ mapM_ (\ (_, taskTakes) -> putMVar taskTakes (ManagerToTask CancelTask)) (Map.toList (mvarMap st))
                   put (st {managerMode = Exiting'})

                 TopToManager (TopToTask taskId msg') -> do -- Forward some other message to a task
                   case Map.lookup taskId (mvarMap st) of
                     Just taskTakes -> lift $ putMVar taskTakes (ManagerToTask msg')
                     Nothing -> lift $ putMVar topTakes (NoSuchTask taskId)

                 TaskToManager (ProcessToManager taskId (Result code)) -> do
                   -- A process finished - remove it from the process map
                   lift $ putMVar topTakes (TaskToTop (ProcessToManager taskId (Result code)))
                   put (st { mvarMap = Map.delete taskId (mvarMap st) })

                 TaskToManager (ProcessStatus taskId status) -> do
                   -- Insert the process handle into the task status
                   lift $ putMVar topTakes (TaskToTop (ProcessStatus taskId status))

                 TaskToManager msg' -> do
                   -- Forward messages to top
                   lift $ putMVar topTakes (TaskToTop msg')

               loop

data TaskState
    = TaskState
      { processStatus :: Status
      , processHandle :: Maybe ProcessHandle
      -- ^ This is not available until the process sends it
      -- back to the task manager after being started.
      } deriving Show

-- | A task can have two statuses - either it is running, or it has
-- been asked to terminate (and is still running.)
data Status
    = Running
    | Exiting
    deriving (Show, Eq)

-- | Manage a single task.  This is a wrapper around a process that
-- can do status inquiries, tell the process to terminate, notice the
-- task has finished and return a message, A task receives TaskInput
-- from the manager and the process, and sends TaskOutput messages
-- back to the manager.  It forks the process into the background so
-- it can receive messages from it and the task coordinator.
task :: (Show taskid, Read taskid, ListLikePlus a c, a ~ Text) =>
        taskid -> MVar (ManagerTakes taskid) -> MVar TaskTakes -> CreateProcess -> a -> IO ()
task taskId managerTakes taskTakes cmd input = do
  ePutStrLn $ "Starting task " ++ show cmd
  -- hs@(_, _, _, pid) <- createProcess (cmd {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
  forkIO $ process taskId taskTakes cmd input
  evalStateT loop (TaskState {processStatus = Running, processHandle = Nothing})
    where
      -- Read and send messages until the process sends a result
      loop :: StateT TaskState IO ()
      loop = do
        st <- get
        ePutStrLn $ "task " ++ show taskId ++ " waiting"
        msg <- lift $ takeMVar taskTakes
        ePutStrLn $ "task " ++ show taskId ++ " received: " ++ show msg
        case msg of
          ManagerToTask SendTaskStatus -> putManager (ProcessStatus taskId st) >> loop
          ManagerToTask CancelTask ->
              do lift $ maybe (return ()) terminateProcess (processHandle st)
                 putManager (ProcessFinished taskId)
          ProcessToTask x@(Result _) -> putManager (ProcessToManager taskId x) -- Process is finished, so stop looping
          ProcessToTask x@(ProcessHandle pid) -> put (st {processHandle = Just pid}) >> putManager (ProcessToManager taskId x) >> loop
          ProcessToTask x -> putManager (ProcessToManager taskId x) >> loop

      putManager msg = do
        ePutStrLn ("task->manager: " ++ show msg)
        lift $ putMVar managerTakes (TaskToManager msg)

taskTest = newEmptyMVar >>= \ v1 -> newEmptyMVar >>= \ v2 -> task 1 v1 v2 (proc "oneko" []) (Text.pack "")

-- | A process only sends process output.
process :: Show taskid => taskid -> MVar TaskTakes -> CreateProcess -> Text -> IO ()
process taskId taskTakes p input = do
  ePutStrLn $ "process " ++ show taskId ++ " starting"
  readProcessInterleaved (\ pid -> putMVar taskTakes (ProcessToTask (ProcessHandle pid))) p input >>=
    mapM_ (\ x -> ePutStrLn ("process " ++ show taskId ++ " chunk: " ++ show x) >> putMVar taskTakes (ProcessToTask x))
--process taskId taskTakes cmd input = do
--  ePutStrLn $ "process starting: " ++ show cmd
--  p <- createProcess (cmd {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe})
--  chunks <- readProcessChunks (mempty :: L.ByteString) p
--  mapM_ putTask chunks
--    where
--      putTask :: Chunk L.ByteString -> IO ()
--      putTask chunk = do
--        ePutStrLn ("sending to task: " ++ show chunk)
--        putMVar taskTakes (ProcessToTask chunk)
