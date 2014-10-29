{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, Rank2Types, ScopedTypeVariables, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

import Control.Concurrent (MVar, putMVar)
import Control.Exception (fromException, AsyncException(ThreadKilled), throw, ArithException(LossOfPrecision), try)
import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Trans (lift)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Monoid
import Data.Text.Lazy as Text (Text)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (CreateProcess, shell, proc, ProcessHandle, terminateProcess)
import System.Process.Chunks (Chunk(..))
import System.Process.ListLike (ListLikeLazyIO, readCreateProcess)
import System.Process.Text.Lazy ()
import System.Tasks (manager)
import System.Tasks.Types (TaskId, ProgressAndResult(taskMessage), ManagerTakes(..), TopToManager(..), ManagerToTask(..), TopTakes(..), ManagerToTop(..), TaskToManager(..), TaskTakes(IOToTask), IOPuts(..))
import System.Tasks.Pretty ()
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

#if DEBUG
import Debug.Console (ePutStrLn)
import Debug.Show (V(V))
#else
-- Use hPutStrLn instead of the ePutStrLn in Debug.Console.
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO (hPutStrLn, stderr)
ePutStrLn :: MonadIO m => String -> m ()
ePutStrLn = liftIO . hPutStrLn stderr
#endif

type TID = Integer
instance TaskId TID
instance ProgressAndResult (Chunk Text) ResultType where
    taskMessage (Exception e) | fromException e == Just ThreadKilled = IOCancelled
    taskMessage (Exception e) = IOException e
    taskMessage (Result ExitSuccess) = IOFinished 0
    taskMessage (Result (ExitFailure n)) = IOFinished n
    taskMessage x@(ProcessHandle h) = IOProgress x
    taskMessage x@(Stdout _) = IOProgress x
    taskMessage x@(Stderr _) = IOProgress x
    -- taskMessage x = ProcessToManager x
type ResultType = Int

main :: IO ()
main = manager (`evalStateT` (cmds, 1)) keyboard output

instance Show ProcessHandle where
    show _ = "<ProcessHandle>"

deriving instance Show (Chunk Text)

#if DEBUG
instance Pretty (V (Chunk Text)) where
    pPrint (V (Exception e)) = pPrint (V e)
    pPrint (V x) = text (show x)
#endif

-- | The output device
output :: TopTakes TID (Chunk Text) ResultType -> IO ()
output (TopTakes ManagerFinished) = ePutStrLn "ManagerFinished"
output (TopTakes (ManagerStatus tasks status)) = ePutStrLn $ "ManagerStatus " ++ show tasks ++ " " ++ show status
output (TopTakes (NoSuchTask taskid)) = ePutStrLn $ show taskid ++ ": NoSuchTask"
output (TopTakes (TaskStatus taskid status)) = ePutStrLn $ show taskid ++ ": TaskStatus " ++ show status
output (TopTakes (TaskToTop (TaskPuts taskid IOCancelled))) = ePutStrLn $ show taskid ++ ": IOCancelled"
output (TopTakes (TaskToTop (TaskPuts taskid (IOException e)))) = ePutStrLn $ show taskid ++ ": IOException " ++ show e
output (TopTakes (TaskToTop (TaskPuts taskid (IOFinished result)))) = ePutStrLn $ show taskid ++ ": IOFinished " ++ show result
output (TopTakes (TaskToTop (TaskPuts taskid (IOProgress chunk)))) = ePutStrLn $ show taskid ++ ": IOProgress " ++ show chunk

-- | The input device
keyboard :: StateT ([MVar (TaskTakes TID (Chunk Text) ResultType) -> IO ResultType], TID) IO (ManagerTakes TID (Chunk Text) ResultType)
keyboard = do
  input <- lift $ getLine
  case input of
    -- Start a new task
    "t" -> get >>= \ ((cmd : more), nextId) -> put (more, succ nextId) >> return (TopToManager (StartTask nextId cmd))
    -- Get the status of a task
    ['s',d] | isDigit d -> return (TopToManager (SendTaskStatus (read [d])))
    -- Get process manager status
    "s" -> return (TopToManager SendManagerStatus)
    -- Kill a task
    ['k',d] | isDigit d -> return (TopToManager $ TopToTask $ CancelTask $ read [d])
    -- Initiate shutdown and exit keyboard loop
    "x" -> return (TopToManager ShutDown)
    -- error
    x -> ePutStrLn (show x ++ " - expected: t, s, s<digit>, k<digit>, or x") >> keyboard

-- | The sequence of tasks that t will run.
cmds :: [MVar (TaskTakes TID (Chunk Text) ResultType) -> IO ResultType]
cmds = throwExn : run' countToFive : map run' nekos

throwExn :: MVar (TaskTakes taskid (Chunk Text) ResultType) -> IO Int
throwExn taskTakes = try (ePutStrLn "About to throw an exception" >> throw LossOfPrecision) >>=
                     either (\ e -> putMVar taskTakes (IOToTask (taskMessage (Exception e))) >> throw e)
                            (\ v -> putMVar taskTakes (IOToTask (taskMessage (Result v))) >> return 0)
countToFive :: CreateProcess
countToFive = shell $ "bash -c 'echo hello from task 1>&2; for i in " <> intercalate " " (map show ([1..5] :: [Int])) <> "; do echo $i; sleep 1; done'"
million :: CreateProcess
million = shell $ "yes | head -1000000"
neko :: String -> Int -> CreateProcess
neko color speed = proc "oneko" ["-fg", color, "-speed", show speed]
nekos :: [CreateProcess]
nekos = map (uncurry neko) (zip (concat (repeat ["red", "green", "blue", "black", "yellow"])) (concat (repeat [12..18])))

run :: (ListLikeLazyIO a c, a ~ Text, TaskId taskid, progress ~ Chunk Text, result ~ ResultType) =>
       CreateProcess -> a -> MVar (TaskTakes taskid progress result) -> IO result
run cmd input taskTakes =
  do (ProcessHandle _pid : chunks) <- readCreateProcess cmd input
     mapM_ (putMVar taskTakes . IOToTask . taskMessage) chunks
     mapM_ (\ chunk -> case chunk of
                         Exception e -> do
#if DEBUG
                           ePutStrLn "throw chunk!"
#endif
                           throw e
                         _x -> return ()) chunks
     return 123

run' :: (TaskId taskid, progress ~ Chunk Text, result ~ ResultType) => CreateProcess -> MVar (TaskTakes taskid progress result) -> IO ResultType
run' cmd taskTakes = run cmd mempty taskTakes

