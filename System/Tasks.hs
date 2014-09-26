{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeSynonymInstances #-}
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
    ( manager
    , module System.Tasks.Types
    ) where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import qualified Data.ByteString.Lazy as L
import Data.List (intercalate)
import Data.Map as Map (Map, insert, toList, delete, lookup, null, keys, member)
import Data.Monoid
import Data.Set (Set, fromList)
import Data.Text.Lazy as Text (Text, pack)
import Debug.Console (ePutStrLn)
import System.Exit (ExitCode)
import System.IO
import System.IO.Unsafe
import System.Process (ProcessHandle, shell, proc, terminateProcess, CreateProcess(..), CmdSpec(..), createProcess, StdStream(CreatePipe))
import System.Process.ListLike (ListLikePlus, Chunk(..), showCmdSpecForUser)
import System.Process.ListLike.Thread (readProcessInterleaved)
import System.Tasks.Types
import System.Tasks.Pretty (putMVar', takeMVar')
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), Doc, text)

data ManagerState taskid
    = ManagerState
      { managerStatus :: ManagerStatus
      , nextTaskId :: taskid
      , mvarMap :: Map taskid (MVar (TaskTakes taskid))
      }

manager :: forall taskid. (Show taskid, Read taskid, Enum taskid, Ord taskid) => taskid -> MVar (TopTakes taskid) -> MVar (ManagerTakes taskid) -> IO ()
manager firstTaskId topTakes managerTakes = do
  evalStateT loop (ManagerState {managerStatus = Running', nextTaskId = firstTaskId, mvarMap = mempty})
    where
      loop :: StateT (ManagerState taskid) IO ()
      loop = do
        st <- get
        case (managerStatus st, Map.null (mvarMap st)) of
          (Exiting', True) -> lift $ putMVar' topTakes (TopTakes ManagerFinished)
          _ -> do
               msg <- lift $ takeMVar' managerTakes
               case msg of
                 TopToManager (StartTask cmd input) -> do -- Start a new task
                   taskTakes <- lift newEmptyMVar
                   _tid <- lift $ forkIO $ task (nextTaskId st) managerTakes taskTakes cmd input
                   put (st {nextTaskId = succ (nextTaskId st), mvarMap = Map.insert (nextTaskId st) taskTakes (mvarMap st)})
                   -- We should probably send back a message here saying the task was started
                 TopToManager SendManagerStatus -> do -- Send top the manager status (Running or Exiting)
                   lift $ putMVar' topTakes (TopTakes (ManagerStatus (fromList (keys (mvarMap st))) (managerStatus st)))
                 TopToManager ShutDown -> do -- Tell all the tasks to shut down
                   lift $ mapM_ (\ (taskId, taskTakes) -> putMVar' taskTakes (ManagerToTask (CancelTask taskId))) (Map.toList (mvarMap st))
                   put (st {managerStatus = Exiting'})

                 TopToManager (SendTaskStatus taskId) -> do
                   lift $ putMVar' topTakes (TopTakes (TaskStatus taskId (Map.member taskId (mvarMap st))))
                 TopToManager (TopToTask (CancelTask taskId)) -> do -- Forward some other message to a task
                   case Map.lookup taskId (mvarMap st) of
                     Just taskTakes -> lift $ putMVar' taskTakes (ManagerToTask (CancelTask taskId))
                     Nothing -> lift $ putMVar' topTakes (TopTakes (NoSuchTask taskId))

                 TaskToManager (ProcessToManager taskId (Result code)) -> do
                   -- A process finished - remove it from the process map
                   lift $ putMVar' topTakes (TopTakes (TaskToTop (ProcessToManager taskId (Result code))))
                   put (st { mvarMap = Map.delete taskId (mvarMap st) })

                 TaskToManager msg' -> do
                   -- Forward messages to top
                   lift $ putMVar' topTakes (TopTakes (TaskToTop msg'))

               loop


data TaskState
    = TaskState
      { processHandle :: Maybe ProcessHandle
      -- ^ This is not available until the process sends it
      -- back to the task manager after being started.
      } deriving Show

-- | Manage a single task.  This is a wrapper around a process that
-- can do status inquiries, tell the process to terminate, notice the
-- task has finished and return a message, A task receives TaskInput
-- from the manager and the process, and sends TaskOutput messages
-- back to the manager.  It forks the process into the background so
-- it can receive messages from it and the task coordinator.
task :: (Show taskid, Read taskid, ListLikePlus a c, a ~ Text) =>
        taskid -> MVar (ManagerTakes taskid) -> MVar (TaskTakes taskid) -> CreateProcess -> a -> IO ()
task taskId managerTakes taskTakes cmd input = do
  forkIO $ process taskId taskTakes cmd input
  evalStateT loop (TaskState {processHandle = Nothing})
    where
      -- Read and send messages until the process sends a result
      loop :: StateT TaskState IO ()
      loop = do
        st <- get
        msg <- lift $ takeMVar' taskTakes
        case msg of
          ManagerToTask (CancelTask _) ->
              do lift $ maybe (return ()) terminateProcess (processHandle st)
                 loop
          ProcessToTask x@(Result _) -> putManager (ProcessToManager taskId x) -- Process is finished, so stop looping
          ProcessToTask x@(ProcessHandle pid) -> put (st {processHandle = Just pid}) >> putManager (ProcessToManager taskId x) >> loop
          ProcessToTask x -> putManager (ProcessToManager taskId x) >> loop

      putManager msg = lift $ putMVar' managerTakes (TaskToManager msg)

taskTest = newEmptyMVar >>= \ v1 -> newEmptyMVar >>= \ v2 -> task 1 v1 v2 (proc "oneko" []) (Text.pack "")

-- | A process only sends process output.
process :: Show taskid => taskid -> MVar (TaskTakes taskid) -> CreateProcess -> Text -> IO ()
process taskId taskTakes p input = do
  readProcessInterleaved (\ pid -> putMVar' taskTakes (ProcessToTask (ProcessHandle pid))) p input >>=
    mapM_ (putMVar' taskTakes . ProcessToTask)
