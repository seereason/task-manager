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

import Control.Concurrent (forkIO, MVar, newEmptyMVar, throwTo)
import Control.Concurrent.Async (Async, async, asyncThreadId, cancel, waitCatch)
import Control.Exception (fromException, AsyncException(ThreadKilled))
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map as Map (Map, insert, toList, delete, lookup, null, keys, member)
import Data.Monoid
import Data.Set (fromList)
import Data.Text.Lazy (Text)
import System.Exit (ExitCode(..))
import System.Process (ProcessHandle)
import System.Process.Chunks (Chunk(..))
import System.Tasks.Types

-- When DEBUG is set a fairly nice display of all the message passing
-- is produced.
#if DEBUG
import Debug.Show (V(V))
import System.Tasks.Pretty (putMVar, takeMVar)
import Debug.Console (ePutStrLn) -- atomic debug output
#else
import Control.Concurrent (putMVar, takeMVar)
#endif

manager :: forall m taskid progress result. (MonadIO m, TaskId taskid, ProgressAndResult progress result, progress ~ Chunk Text, result ~ Int) =>
           (forall a. m a -> IO a)    -- ^ Run the monad transformer required by the putter
        -> (m (ManagerTakes taskid progress result))  -- ^ return the next message to send to the task manager
        -> (TopTakes taskid progress result -> IO ()) -- ^ handle a message delivered by the task manager
        -> IO ()
manager runner putter taker = do
  topTakes <- newEmptyMVar
  managerTakes <- newEmptyMVar
  -- Run messages between the task manager and the tasks.  The reason
  -- I use succ def instead of def is so that the first task will be 1
  -- rather than 0 if taskid is an Integral.
  _ <- async $ managerLoop topTakes managerTakes
  -- Messages coming from the input device.  This will run forever, it
  -- needs to be killed when the task manager is finished.
  let putLoop = do
        msg <- putter
        liftIO $ putMVar managerTakes msg
        case msg of
          -- TopToManager ShutDown -> return ()
          _ -> putLoop
  async (runner putLoop) >>= \ inputThread ->
      -- Messages from the task manager going to the client via the taker
      -- argument.  This runs in the main thread and exits when the
      -- ManagerFinished message is received.
      let takeLoop = do
            msg <- takeMVar topTakes
            taker msg
            case msg of
              TopTakes ManagerFinished -> cancel inputThread
              _ -> takeLoop in
      takeLoop

data ManagerState taskid progress
    = ManagerState
      { managerStatus :: ManagerStatus
      , mvarMap :: Map taskid (MVar (TaskTakes taskid progress))
      }

managerLoop :: forall taskid progress result. (TaskId taskid, ProgressAndResult progress result, progress ~ Chunk Text, result ~ Int) =>
               MVar (TopTakes taskid progress result)
            -> MVar (ManagerTakes taskid progress result)
            -> IO ()
managerLoop topTakes managerTakes = do
#if DEBUG
  ePutStrLn "top\t\tmanager\t\ttask\t\tprocess"
#endif
  evalStateT loop (ManagerState {managerStatus = Running, mvarMap = mempty})
    where
      loop :: StateT (ManagerState taskid progress) IO ()
      loop = do
        st <- get
        case (managerStatus st, Map.null (mvarMap st)) of
          (Exiting, True) -> liftIO $ putMVar topTakes (TopTakes ManagerFinished)
          _ -> do
               msg <- liftIO $ takeMVar managerTakes
               case msg of
                 TopToManager (StartTask taskId run) -> do
                   taskTakes <- liftIO newEmptyMVar
                   _a <- liftIO $ async $ forkIO $ task taskId managerTakes taskTakes (run taskTakes)
                   put (st {mvarMap = Map.insert taskId taskTakes (mvarMap st)})
                   -- We should probably send back a message here saying the task was started
                 TopToManager SendManagerStatus -> do -- Send top the manager status (Running or Exiting)
                   liftIO $ putMVar topTakes (TopTakes (ManagerStatus (fromList (keys (mvarMap st))) (managerStatus st)))
                 TopToManager ShutDown -> do -- Tell all the tasks to shut down
                   liftIO $ mapM_ (\ (taskId, taskTakes) -> putMVar taskTakes (ManagerToTask (CancelTask taskId))) (Map.toList (mvarMap st))
                   put (st {managerStatus = Exiting})

                 TopToManager (SendTaskStatus taskId) -> do
                   liftIO $ putMVar topTakes (TopTakes (TaskStatus taskId (Map.member taskId (mvarMap st))))
                 TopToManager (TopToTask (CancelTask taskId)) -> do -- Forward some other message to a task
                   case Map.lookup taskId (mvarMap st) of
                     Just taskTakes -> liftIO $ putMVar taskTakes (ManagerToTask (CancelTask taskId))
                     Nothing -> liftIO $ putMVar topTakes (TopTakes (NoSuchTask taskId))

                 TaskToManager x@(TaskPuts taskId (TaskFinished _result)) -> do
                   -- The task completed and delivered its result value.
                   liftIO $ putMVar topTakes (TopTakes (TaskToTop x))
                   put (st { mvarMap = Map.delete taskId (mvarMap st) })

                 TaskToManager x@(TaskPuts taskId TaskCancelled) -> do
                   -- Process was sent a cancel (thread killed exception)
                   liftIO $ putMVar topTakes (TopTakes (TaskToTop x))
                   put (st { mvarMap = Map.delete taskId (mvarMap st) })

                 TaskToManager (TaskPuts _taskId (ProcessToManager (Result _code))) ->
                   -- A process finished - remove it from the process map
                   -- (It is no longer necessary to send this message, the
                   -- TaskFinished message below is used instead.)
                   -- liftIO $ putMVar topTakes (TopTakes (TaskToTop (TaskFinished taskId)))
                   -- put (st { mvarMap = Map.delete taskId (mvarMap st) })
                   return ()

                 TaskToManager x@(TaskPuts taskId (ProcessToManager (Exception _e))) -> do
                   -- A process was terminated with an exception - remove it from the process map
                   liftIO $ putMVar topTakes (TopTakes (TaskToTop x))
                   put (st { mvarMap = Map.delete taskId (mvarMap st) })

                 TaskToManager msg' -> do
                   -- Forward messages to top
                   liftIO $ putMVar topTakes (TopTakes (TaskToTop msg'))

               loop

data TaskState result
    = TaskState
      { processHandle :: Maybe ProcessHandle
      , processAsync :: Async result
      -- ^ This is not available until the process sends it
      -- back to the task manager after being started.
      }

-- | Manage a single task.  This is a wrapper around a process that
-- can do status inquiries, tell the process to terminate, notice the
-- task has finished and return a message, A task receives TaskInput
-- from the manager and the process, and sends TaskOutput messages
-- back to the manager.  It forks the process into the background so
-- it can receive messages from it and the task coordinator.
task :: forall taskid progress result. (TaskId taskid, ProgressAndResult progress result, progress ~ Chunk Text, result ~ Int) =>
        taskid
     -> MVar (ManagerTakes taskid progress result)
     -> MVar (TaskTakes taskid progress)
     -> IO result
     -> IO ()
task taskId managerTakes taskTakes p = do
  a <- async p
  evalStateT loop (TaskState {processAsync = a, processHandle = Nothing})
  r <- waitCatch a
  case r of
    Left e -> do
        -- This is not ever being executed - why?
#if DEBUG
        ePutStrLn ("wait -> " ++ show (V e))
#endif
        throwTo (asyncThreadId a) e
        putMVar managerTakes (TaskToManager (TaskPuts taskId (TaskException e)))
    Right result -> do
        putMVar managerTakes (TaskToManager (TaskPuts taskId (TaskFinished result)))
    where
      -- Read and send messages from the process until we see the final result
      loop :: StateT (TaskState result) IO ()
      loop = do
        st <- get
        msg <- liftIO $ takeMVar taskTakes
        case msg of
          ProcessToTask x@(ProcessHandle pid) ->
              do put (st {processHandle = Just pid})
                 liftIO $ putMVar managerTakes (TaskToManager (TaskPuts taskId (ProcessToManager x)))
                 loop
          ManagerToTask (CancelTask _) ->
              do liftIO $ cancel (processAsync st)
                 -- liftIO $ maybe (return ()) terminateProcess (processHandle st)
                 loop
          ProcessToTask (Result ExitSuccess) ->  -- Process is finished, so stop looping
              liftIO $ putMVar managerTakes (TaskToManager (TaskPuts taskId (TaskFinished 0)))
          ProcessToTask (Result (ExitFailure n)) ->  -- Process is finished, so stop looping
              liftIO $ putMVar managerTakes (TaskToManager (TaskPuts taskId (TaskFinished n)))
          ProcessToTask (Exception e) | fromException e == Just ThreadKilled -> -- Process was cancelled
              liftIO $ putMVar managerTakes (TaskToManager (TaskPuts taskId TaskCancelled))
          ProcessToTask (Exception e) -> -- Some other exception
              liftIO $ putMVar managerTakes (TaskToManager (TaskPuts taskId (TaskException e)))
          ProcessToTask x -> -- Stdout, Stderr
              do liftIO $ putMVar managerTakes (TaskToManager (TaskPuts taskId (ProcessToManager x)))
                 loop
