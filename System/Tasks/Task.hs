{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, Rank2Types, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
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

-- | Helper function called by 'System.Tasks.Manager.manager' to run a
-- single task.  This wraps up the IO operation so that we can do
-- status inquiries, tell the process to terminate, notice the task
-- has finished and return a message.  A task receives TaskTakes
-- messages from the manager and the process, and sends ManagerTakes
-- messages back to the manager.  It forks the IO operation into the
-- background so it can receive TaskTakes messages from it.
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
        -- This happens after P2 is cancelled, but this message was already sent
        -- from inside the loop.
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
        -- process - IOFinished, IOCancelled, IOException.
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
          IOToTask m@(IOFinished _) ->
              liftIO (putMVar managerTakes (TaskToManager (TaskPuts taskId m)))
          IOToTask IOCancelled ->
              return ()
          IOToTask m@(IOException _) ->
              -- This exception should have caused the async to finish
              -- and arrived at the waitCatch above.  It seems to be
              -- coming here too, I'm not sure why.
              liftIO (putMVar managerTakes (TaskToManager (TaskPuts taskId m)))
