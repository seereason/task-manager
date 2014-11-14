{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, Rank2Types, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module System.Tasks.Manager
    ( manager
    ) where

import Control.Concurrent (forkIO, MVar, newEmptyMVar)
import Control.Concurrent.Async (async, cancel)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map as Map (Map, insert, toList, delete, lookup, null, keys, member)
import Data.Monoid
import Data.Set (fromList)
import System.Tasks.Task (task)
import System.Tasks.Types

-- When DEBUG is set a fairly nice display of all the message passing
-- is produced.
#if DEBUG
import System.Tasks.Pretty (putMVar, takeMVar)
import Debug.Console (ePutStrLn) -- atomic debug output
#else
import Control.Concurrent (putMVar, takeMVar)
#endif

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

manager :: forall m taskid progress result. (MonadIO m, TaskId taskid, ProgressAndResult progress result) =>
           (m () -> IO ())    -- ^ Run the monad transformer required by the putter
        -> (m (ManagerTakes taskid progress result))  -- ^ return the next message to send to the task manager
        -> (TopTakes taskid progress result -> IO ()) -- ^ handle a message delivered by the task manager
        -> IO ()
manager runner putter taker = do
  topTakes <- newEmptyMVar
  managerTakes <- newEmptyMVar
  -- Run messages between the task manager and the tasks.
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
              ManagerFinished -> cancel inputThread
              _ -> takeLoop in
      takeLoop

data ManagerState taskid progress result
    = ManagerState
      { managerStatus :: ManagerStatus
      , mvarMap :: Map taskid (MVar (TaskTakes taskid progress result))
      }

managerLoop :: forall taskid progress result. (TaskId taskid, ProgressAndResult progress result) =>
               MVar (TopTakes taskid progress result)
            -> MVar (ManagerTakes taskid progress result)
            -> IO ()
managerLoop topTakes managerTakes = do
#if DEBUG
  ePutStrLn "\ttop\t\tmanager\t\t\ttask\t\t\tprocess"
#endif
  evalStateT loop (ManagerState {managerStatus = Running, mvarMap = mempty})
    where
      loop :: StateT (ManagerState taskid progress result) IO ()
      loop = do
        st <- get
        case (managerStatus st, Map.null (mvarMap st)) of
          (Exiting, True) -> liftIO $ putMVar topTakes ManagerFinished
          _ -> do
               msg <- liftIO $ takeMVar managerTakes
               case msg of
                 TopToManager (StartTask taskId run) -> do
                   taskTakes <- liftIO newEmptyMVar
                   _a <- liftIO $ async $ forkIO $ task taskId managerTakes taskTakes (run taskTakes)
                   put (st {mvarMap = Map.insert taskId taskTakes (mvarMap st)})
                   -- We should probably send back a message here saying the task was started
                 TopToManager SendManagerStatus -> do -- Send top the manager status (Running or Exiting)
                   liftIO $ putMVar topTakes (ManagerStatus (fromList (keys (mvarMap st))) (managerStatus st))
                 TopToManager ShutDown -> do -- Tell all the tasks to shut down
                   liftIO $ mapM_ (\ (taskId, taskTakes) -> putMVar taskTakes (ManagerToTask (CancelTask taskId))) (Map.toList (mvarMap st))
                   put (st {managerStatus = Exiting})

                 TopToManager (SendTaskStatus taskId) -> do
                   liftIO $ putMVar topTakes (TaskStatus taskId (Map.member taskId (mvarMap st)))
                 TopToManager (TopToTask (CancelTask taskId)) -> do -- Forward some other message to a task
                   case Map.lookup taskId (mvarMap st) of
                     Just taskTakes -> liftIO $ putMVar taskTakes (ManagerToTask (CancelTask taskId))
                     Nothing -> liftIO $ putMVar topTakes (NoSuchTask taskId)

                 TaskToManager x@(TaskPuts taskId (IOFinished _result)) -> do
                   -- The task completed and delivered its result value.
                   liftIO $ putMVar topTakes (TaskToTop x)
                   put (st { mvarMap = Map.delete taskId (mvarMap st) })

                 TaskToManager x@(TaskPuts taskId IOCancelled) -> do
                   -- Process was sent a cancel (thread killed exception)
                   liftIO $ putMVar topTakes (TaskToTop x)
                   put (st { mvarMap = Map.delete taskId (mvarMap st) })

                 TaskToManager x@(TaskPuts taskId (IOException _)) -> do
                   -- Process was sent a cancel (thread killed exception)
                   liftIO $ putMVar topTakes (TaskToTop x)
                   put (st { mvarMap = Map.delete taskId (mvarMap st) })

                 TaskToManager x@(TaskPuts _ (IOProgress _)) -> do
                   -- Forward messages to top
                   liftIO $ putMVar topTakes (TaskToTop x)

               loop
