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

import Control.Concurrent (forkIO, MVar, newEmptyMVar, killThread, throwTo)
import Control.Concurrent.Async (Async, async, asyncThreadId, cancel, wait)
import Control.Exception (fromException, SomeException, AsyncException(ThreadKilled), throw, try)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map as Map (Map, insert, toList, delete, lookup, null, keys, member)
import Data.Monoid
import Data.Set (fromList)
import Data.Text.Lazy as Text (Text)
import System.Process (ProcessHandle, terminateProcess, CreateProcess(..))
import System.Process.Chunks (Chunk(..))
import System.Process.ListLike (ListLikeLazyIO)
import System.Process.ListLike (readCreateProcess)
import System.Tasks.Types

#if DEBUG
import Debug.Show (V(V))
-- When DEBUG is set a fairly nice display of all the message passing
-- is produced.
import System.Tasks.Pretty (putMVar, takeMVar)
import Debug.Console (ePutStrLn)
#else
import Control.Concurrent (putMVar, takeMVar)
#endif

manager :: forall m taskid. (MonadIO m, TaskId taskid) =>
           (forall a. m a -> IO a)    -- ^ Run the monad transformer required by the putter
        -> (m (ManagerTakes taskid))  -- ^ return the next message to send to the task manager
        -> (TopTakes taskid -> IO ()) -- ^ handle a message delivered by the task manager
        -> IO ()
manager runner putter taker = do
  topTakes <- newEmptyMVar
  managerTakes <- newEmptyMVar
  -- Run messages between the task manager and the tasks.  The reason
  -- I use succ def instead of def is so that the first task will be 1
  -- rather than 0 if taskid is an Integral.
  _ <- forkIO $ managerLoop topTakes managerTakes
  -- Messages coming from the input device.  This will run forever, it
  -- needs to be killed when the task manager is finished.
  inputThread <- forkIO $ runner (putLoop managerTakes)
  -- Messages from the task manager going to the client via the taker
  -- argument.  This runs in the main thread and exits when the
  -- ManagerFinished message is received.
  takeLoop topTakes inputThread
    where
      takeLoop topTakes tid =
          do msg <- takeMVar topTakes
             taker msg
             case msg of
               TopTakes ManagerFinished -> killThread tid
               _ -> takeLoop topTakes tid

      -- Keep sending messages to the client until...?
      putLoop :: MVar (ManagerTakes taskid) -> m ()
      putLoop managerTakes =
          do msg <- putter
             liftIO $ putMVar managerTakes msg
             case msg of
               -- TopToManager ShutDown -> return ()
               _ -> putLoop managerTakes

data ManagerState taskid
    = ManagerState
      { managerStatus :: ManagerStatus
      , mvarMap :: Map taskid (MVar (TaskTakes taskid))
      }

managerLoop :: forall taskid. TaskId taskid =>
               MVar (TopTakes taskid)
            -> MVar (ManagerTakes taskid)
            -> IO ()
managerLoop topTakes managerTakes = do
#if DEBUG
  ePutStrLn "top\t\tmanager\t\ttask\t\tprocess"
#endif
  evalStateT loop (ManagerState {managerStatus = Running, mvarMap = mempty})
    where
      loop :: StateT (ManagerState taskid) IO ()
      loop = do
        st <- get
        case (managerStatus st, Map.null (mvarMap st)) of
          (Exiting, True) -> liftIO $ putMVar topTakes (TopTakes ManagerFinished)
          _ -> do
               msg <- liftIO $ takeMVar managerTakes
               case msg of
                 TopToManager (StartTask taskId cmd input) -> do
                   taskTakes <- liftIO newEmptyMVar
                   _tid <- liftIO $ forkIO $ task taskId managerTakes taskTakes (run cmd input taskTakes)
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

                 TaskToManager (ProcessToManager taskId (Result _code)) -> do
                   -- A process finished - remove it from the process map
                   liftIO $ putMVar topTakes (TopTakes (TaskToTop (TaskFinished taskId)))
                   put (st { mvarMap = Map.delete taskId (mvarMap st) })

                 TaskToManager x@(ProcessToManager taskId (Exception _e)) -> do
                   -- A process was terminated with an exception - remove it from the process map
                   liftIO $ putMVar topTakes (TopTakes (TaskToTop x))
                   put (st { mvarMap = Map.delete taskId (mvarMap st) })

                 TaskToManager x@(TaskCancelled taskId) -> do
                   -- Process was sent a cancel (thread killed exception)
                   liftIO $ putMVar topTakes (TopTakes (TaskToTop x))
                   put (st { mvarMap = Map.delete taskId (mvarMap st) })

                 TaskToManager msg' -> do
                   -- Forward messages to top
                   liftIO $ putMVar topTakes (TopTakes (TaskToTop msg'))

               loop


data TaskState
    = TaskState
      { processHandle :: Maybe ProcessHandle
      , processAsync :: Async ()
      -- ^ This is not available until the process sends it
      -- back to the task manager after being started.
      }

run :: (ListLikeLazyIO a c, a ~ Text, TaskId taskid) =>
       CreateProcess -> a -> MVar (TaskTakes taskid) -> IO ()
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

-- | Manage a single task.  This is a wrapper around a process that
-- can do status inquiries, tell the process to terminate, notice the
-- task has finished and return a message, A task receives TaskInput
-- from the manager and the process, and sends TaskOutput messages
-- back to the manager.  It forks the process into the background so
-- it can receive messages from it and the task coordinator.
task :: forall taskid. TaskId taskid =>
        taskid
     -> MVar (ManagerTakes taskid)
     -> MVar (TaskTakes taskid)
     -> IO ()
     -> IO ()
task taskId managerTakes taskTakes p = do
  a <- async p
  evalStateT loop (TaskState {processAsync = a, processHandle = Nothing})
  r <- try (wait a)
  case r of
    Left (e :: SomeException) -> do
        -- This is not ever being executed.  Each exception is being
        -- caught in the process and returned as an Exception chunk.
#if DEBUG
        ePutStrLn ("wait -> " ++ show (V e))
#endif
        throwTo (asyncThreadId a) e
        putMVar managerTakes (TaskToManager (ProcessToManager taskId (Exception e)))
    Right () -> return ()
    where
      -- Read and send messages from the process until we see the final result
      loop :: StateT TaskState IO ()
      loop = do
        st <- get
        msg <- liftIO $ takeMVar taskTakes
        case msg of
          ProcessToTask x@(ProcessHandle pid) ->
              do put (st {processHandle = Just pid})
                 liftIO $ putMVar managerTakes (TaskToManager (ProcessToManager taskId x))
                 loop
          ManagerToTask (CancelTask _) ->
              do liftIO $ cancel (processAsync st)
                 -- liftIO $ maybe (return ()) terminateProcess (processHandle st)
                 loop
          ProcessToTask x@(Result _) ->  -- Process is finished, so stop looping
              liftIO $ putMVar managerTakes (TaskToManager (ProcessToManager taskId x))
          ProcessToTask (Exception e) | fromException e == Just ThreadKilled -> -- Process was cancelled
              liftIO $ putMVar managerTakes (TaskToManager (TaskCancelled taskId))
          ProcessToTask x@(Exception _e) -> -- Some other exception
              liftIO $ putMVar managerTakes (TaskToManager (ProcessToManager taskId x))
          ProcessToTask x -> -- Stdout, Stderr
              do liftIO $ putMVar managerTakes (TaskToManager (ProcessToManager taskId x))
                 loop
