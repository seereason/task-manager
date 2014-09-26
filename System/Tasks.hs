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
    ( ManagerToTop(ManagerStatus, ManagerFinished)
    , ManagerTakes(TopToManager)
    , TopToManager(SendManagerStatus, ShutDown, StartTask, TopToTask)
    , ManagerToTask(CancelTask)
    , TaskTakes
    , manager
    , takeMVar'
    , putMVar'
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
import System.Process.ListLike.Ready (readProcessInterleaved)
import System.Tasks.Types
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), Doc, text)

takeMVar' v = takeMVar v >>= \ x -> ePutStrLn (ppDisplay (Take, x)) >> return x
putMVar' v x = ePutStrLn (ppDisplay (Put, x)) >> putMVar v x

data ManagerState taskid
    = ManagerState
      { managerStatus :: ManagerStatus
      , nextTaskId :: taskid
      , mvarMap :: Map taskid (MVar TaskTakes)
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
               -- ePutStrLn (ppDisplay msg)
               case msg of
                 TopToManager (StartTask cmd input) -> do -- Start a new task
                   taskTakes <- lift newEmptyMVar
                   _tid <- lift $ forkIO $ task (nextTaskId st) managerTakes taskTakes cmd input
                   put (st {nextTaskId = succ (nextTaskId st), mvarMap = Map.insert (nextTaskId st) taskTakes (mvarMap st)})
                   -- We should probably send back a message here saying the task was started
                 TopToManager SendManagerStatus -> do -- Send top the manager status (Running or Exiting)
                   lift $ putMVar' topTakes (TopTakes (ManagerStatus (fromList (keys (mvarMap st))) (managerStatus st)))
                 TopToManager ShutDown -> do -- Tell all the tasks to shut down
                   lift $ mapM_ (\ (_, taskTakes) -> putMVar' taskTakes (ManagerToTask CancelTask)) (Map.toList (mvarMap st))
                   put (st {managerStatus = Exiting'})

                 TopToManager (SendTaskStatus taskId) -> do
                   lift $ putMVar' topTakes (TopTakes (TaskStatus taskId (Map.member taskId (mvarMap st))))
                 TopToManager (TopToTask taskId msg') -> do -- Forward some other message to a task
                   case Map.lookup taskId (mvarMap st) of
                     Just taskTakes -> lift $ putMVar' taskTakes (ManagerToTask msg')
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
      { processStatus :: ProcessStatus
      , processHandle :: Maybe ProcessHandle
      -- ^ This is not available until the process sends it
      -- back to the task manager after being started.
      } deriving Show

-- | A task can have two statuses - either it is running, or it has
-- been asked to terminate (and is still running.)
data ProcessStatus
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
  -- ePutStrLn $ "\t\t\t\t" ++ show cmd
  -- hs@(_, _, _, pid) <- createProcess (cmd {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
  forkIO $ process taskId taskTakes cmd input
  evalStateT loop (TaskState {processStatus = Running, processHandle = Nothing})
    where
      -- Read and send messages until the process sends a result
      loop :: StateT TaskState IO ()
      loop = do
        st <- get
        ePutStrLn $ "\t\t\t" ++ show taskId ++ "\twaiting"
        msg <- lift $ takeMVar' taskTakes
        -- ePutStrLn $ ppDisplay msg
        case msg of
          -- ManagerToTask SendTaskStatus -> putManager (ProcessStatus taskId st) >> loop
          ManagerToTask CancelTask ->
              do lift $ maybe (return ()) terminateProcess (processHandle st)
                 -- putManager (ProcessFinished taskId)
                 loop
          ProcessToTask x@(Result _) -> putManager (ProcessToManager taskId x) -- Process is finished, so stop looping
          ProcessToTask x@(ProcessHandle pid) -> put (st {processHandle = Just pid}) >> putManager (ProcessToManager taskId x) >> loop
          ProcessToTask x -> putManager (ProcessToManager taskId x) >> loop

      putManager msg = do
        -- ePutStrLn $ ppDisplay msg
        lift $ putMVar' managerTakes (TaskToManager msg)

taskTest = newEmptyMVar >>= \ v1 -> newEmptyMVar >>= \ v2 -> task 1 v1 v2 (proc "oneko" []) (Text.pack "")

-- | A process only sends process output.
process :: Show taskid => taskid -> MVar TaskTakes -> CreateProcess -> Text -> IO ()
process taskId taskTakes p input = do
  -- ePutStrLn $ "\t\t\t\t\t\t" ++ show taskId ++ " starting"
  readProcessInterleaved (\ pid -> putMVar' taskTakes (ProcessToTask (ProcessHandle pid))) p input >>=
    mapM_ (\ x -> -- ePutStrLn (ppDisplay (ProcessToTask x)) >>
                  putMVar' taskTakes (ProcessToTask x))

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
