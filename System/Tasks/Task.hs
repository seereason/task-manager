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

module System.Tasks.Task
    ( task
    ) where

import Control.Concurrent (MVar)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Monad.State (StateT, evalStateT, get)
import Control.Monad.Trans (liftIO)
import System.Process (ProcessHandle)
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

data TaskState result
    = TaskState
      { processHandle :: Maybe ProcessHandle
      , processAsync :: Async ()
      -- ^ This is not available until the process sends it
      -- back to the task manager after being started.
      }

-- | Manage a single task.  This is a wrapper around a process that
-- can do status inquiries, tell the process to terminate, notice the
-- task has finished and return a message, A task receives TaskInput
-- from the manager and the process, and sends TaskOutput messages
-- back to the manager.  It forks the process into the background so
-- it can receive messages from it and the task coordinator.
task :: forall taskid progress result. (TaskId taskid, ProgressAndResult progress result) =>
        taskid
     -> MVar (ManagerTakes taskid progress result)
     -> MVar (TaskTakes taskid progress result)
     -> IO ()
     -> IO ()
task taskId managerTakes taskTakes io = do
  a <- async io
  -- Here we are counting on the task sending at least one message
  -- that results in the loop exiting.
  evalStateT loop (TaskState {processAsync = a, processHandle = Nothing})
  r <- waitCatch a
  case r of
    Left e -> do
#if DEBUG
        -- This happens after P2 is cancelled, with BlockedIndefinitelyOnMVar,
        -- presumably because P2 already received a cancel and isn't taking.
        -- The process is still putting messages.
        ePutStrLn ("wait -> " ++ show (V e))
#endif
        -- Uh, if we already did a wait isn't this thread gone?
        -- throwTo (asyncThreadId a) e
        putMVar managerTakes (TaskToManager (TaskPuts taskId (IOException e)))
    Right () -> do
      -- Some sort of completion message should already have been sent
      return ()
      -- putMVar managerTakes (TaskToManager (TaskPuts taskId (IOFinished result)))
    where
      -- Read and send messages from the process until we see the final result
      loop :: StateT (TaskState result) IO ()
      loop = do
        st <- get
        -- At least one terminating message needs to come from the
        -- process - TaskFinish, TaskCancelled, TaskException.
        msg <- liftIO $ takeMVar taskTakes
        case msg of
          ManagerToTask (CancelTask _) ->
              do -- This throws a ThreadKilled exception in the io thread.
                 liftIO $ cancel (processAsync st)
                 -- liftIO $ maybe (return ()) terminateProcess (processHandle st)
                 loop
          IOToTask m@(IOProgress _) ->
              do liftIO (putMVar managerTakes (TaskToManager (TaskPuts taskId m)))
                 loop
          IOToTask m ->
                    liftIO (putMVar managerTakes (TaskToManager (TaskPuts taskId m)))
