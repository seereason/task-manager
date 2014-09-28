{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, Rank2Types, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
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

import Control.Concurrent (forkIO, MVar, newEmptyMVar)
#if DEBUG
import System.Tasks.Pretty (putMVar, takeMVar)
#else
import Control.Concurrent (putMVar, takeMVar)
#endif
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (liftIO)
import Data.Map as Map (Map, insert, toList, delete, lookup, null, keys, member)
import Data.Monoid
import Data.Set (fromList)
import Data.Text.Lazy as Text (Text)
import System.Process (ProcessHandle, terminateProcess, CreateProcess(..))
import System.Process.ListLike (ListLikePlus, Chunk(..))
-- import System.Process.ListLike.Ready (readProcessInterleaved)
import System.Process.ListLike.Thread (readProcessInterleaved)
import System.Tasks.Types

data ManagerState taskid
    = ManagerState
      { managerStatus :: ManagerStatus
      , nextTaskId :: taskid
      , mvarMap :: Map taskid (MVar (TaskTakes taskid))
      }

manager :: forall taskid. (Show taskid, Read taskid, Enum taskid, Ord taskid) =>
           taskid
        -> MVar (TopTakes taskid)
        -> MVar (ManagerTakes taskid)
        -> IO ()
manager firstTaskId topTakes managerTakes = do
#if DEBUG
  ePutStrLn "top\t\tmanager\t\ttask\t\tprocess"
#endif
  evalStateT loop (ManagerState {managerStatus = Running', nextTaskId = firstTaskId, mvarMap = mempty})
    where
      loop :: StateT (ManagerState taskid) IO ()
      loop = do
        st <- get
        case (managerStatus st, Map.null (mvarMap st)) of
          (Exiting', True) -> liftIO $ putMVar topTakes (TopTakes ManagerFinished)
          _ -> do
               msg <- liftIO $ takeMVar managerTakes
               case msg of
                 TopToManager (StartTask cmd input) -> do -- Start a new task
                   taskTakes <- liftIO newEmptyMVar
                   _tid <- liftIO $ forkIO $ task (nextTaskId st) managerTakes taskTakes cmd input
                   put (st {nextTaskId = succ (nextTaskId st), mvarMap = Map.insert (nextTaskId st) taskTakes (mvarMap st)})
                   -- We should probably send back a message here saying the task was started
                 TopToManager SendManagerStatus -> do -- Send top the manager status (Running or Exiting)
                   liftIO $ putMVar topTakes (TopTakes (ManagerStatus (fromList (keys (mvarMap st))) (managerStatus st)))
                 TopToManager ShutDown -> do -- Tell all the tasks to shut down
                   liftIO $ mapM_ (\ (taskId, taskTakes) -> putMVar taskTakes (ManagerToTask (CancelTask taskId))) (Map.toList (mvarMap st))
                   put (st {managerStatus = Exiting'})

                 TopToManager (SendTaskStatus taskId) -> do
                   liftIO $ putMVar topTakes (TopTakes (TaskStatus taskId (Map.member taskId (mvarMap st))))
                 TopToManager (TopToTask (CancelTask taskId)) -> do -- Forward some other message to a task
                   case Map.lookup taskId (mvarMap st) of
                     Just taskTakes -> liftIO $ putMVar taskTakes (ManagerToTask (CancelTask taskId))
                     Nothing -> liftIO $ putMVar topTakes (TopTakes (NoSuchTask taskId))

                 TaskToManager (ProcessToManager taskId (Result code)) -> do
                   -- A process finished - remove it from the process map
                   liftIO $ putMVar topTakes (TopTakes (TaskToTop (ProcessToManager taskId (Result code))))
                   put (st { mvarMap = Map.delete taskId (mvarMap st) })

                 TaskToManager msg' -> do
                   -- Forward messages to top
                   liftIO $ putMVar topTakes (TopTakes (TaskToTop msg'))

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
        taskid
     -> MVar (ManagerTakes taskid)
     -> MVar (TaskTakes taskid)
     -> CreateProcess
     -> a
     -> IO ()
task taskId managerTakes taskTakes cmd input = do
  -- Should we do something with the ThreadId returned here?
  forkIO $ process taskTakes cmd input
  evalStateT loop (TaskState {processHandle = Nothing})
    where
      -- Read and send messages until the process sends a result
      loop :: StateT TaskState IO ()
      loop = do
        st <- get
        msg <- liftIO $ takeMVar taskTakes
        case msg of
          ManagerToTask (CancelTask _) ->
              do liftIO $ maybe (return ()) terminateProcess (processHandle st)
                 loop
          ProcessToTask x@(Result _) -> putManager (ProcessToManager taskId x) -- Process is finished, so stop looping
          ProcessToTask x@(ProcessHandle pid) -> put (st {processHandle = Just pid}) >> putManager (ProcessToManager taskId x) >> loop
          ProcessToTask x -> putManager (ProcessToManager taskId x) >> loop

      putManager msg = liftIO $ putMVar managerTakes (TaskToManager msg)

--instance Monoid ProcessHandle where
--    mempty = undefined
--    mappend p _ = p
--
--instance ListLikePlus a c => ProcessOutput a (Maybe ProcessHandle, [Chunk a]) where
--    pidf pid = (Just pid, mempty)
--    outf x = (Nothing, [Stdout x])
--    errf x = (Nothing, [Stderr x])
--    intf x = (Nothing, [Exception x])
--    codef x = (Nothing, [Result x])

-- | A process only sends process output.
process :: Show taskid =>
           MVar (TaskTakes taskid)
        -> CreateProcess
        -> Text
        -> IO ()
process taskTakes p input = do
  chunks <- readProcessInterleaved p input
  mapM_ (putMVar taskTakes . ProcessToTask) chunks
