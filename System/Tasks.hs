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

import Control.Concurrent (forkIO, MVar, newEmptyMVar, killThread)
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
-- When DEBUG is set a fairly nice display of all the message passing
-- is produced.
import System.Tasks.Pretty (putMVar, takeMVar)
import Debug.Console (ePutStrLn)
#else
import Control.Concurrent (putMVar, takeMVar)
#endif

manager :: forall m taskid. (MonadIO m, Eq taskid, Ord taskid, Enum taskid
#if DEBUG
                             , Show taskid
#endif
                            ) =>

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

managerLoop :: forall taskid. (Enum taskid, Ord taskid
#if DEBUG
                              , Show taskid
#endif
                              ) =>
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
                   _tid <- liftIO $ forkIO $ task taskId managerTakes taskTakes cmd input
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
task :: (ListLikeLazyIO a c, a ~ Text
#if DEBUG
        , Show taskid
#endif
        ) =>
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
process ::
#if DEBUG
           Show taskid =>
#endif
           MVar (TaskTakes taskid)
        -> CreateProcess
        -> Text
        -> IO ()
process taskTakes p input = do
  chunks <- readCreateProcess p input
  mapM_ (putMVar taskTakes . ProcessToTask) chunks
