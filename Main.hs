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
import System.Tasks.Pretty (ppDisplay)
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
    taskMessage x@(ProcessHandle _h) = IOProgress x
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
keyboard :: StateT ([MVar (TaskTakes TID (Chunk Text) ResultType) -> IO ()], TID) IO (ManagerTakes TID (Chunk Text) ResultType)
keyboard = do
  input <- lift $ getLine
  case input of
    -- Start a new task
    "t" -> get >>= \ ((cmd : more), nextId) -> put (more, succ nextId) >> return (TopToManager (StartTask nextId cmd))
    -- Start an IO 123
    "a" -> get >>= \ (cmds, nextId) -> put (cmds, succ nextId) >> return (TopToManager (StartTask nextId (runIO (return 123))))
    -- Throw an exception
    "e" -> get >>= \ (cmds, nextId) -> put (cmds, succ nextId) >> return (TopToManager (StartTask nextId (runIO (throw LossOfPrecision))))
    -- Get the status of a task
    ['s',d] | isDigit d -> return (TopToManager (SendTaskStatus (read [d])))
    -- Get process manager status
    "s" -> return (TopToManager SendManagerStatus)
    -- Kill a task
    ['k',d] | isDigit d -> return (TopToManager $ TopToTask $ CancelTask $ read [d])
    -- Initiate shutdown and exit keyboard loop
    "x" -> return (TopToManager ShutDown)
    -- error
    x -> ePutStrLn (show x ++ " - expected: t, a, e, s, s<digit>, k<digit>, or x") >> keyboard

-- | The sequence of tasks that t will run.
cmds :: (taskid ~ TID, progress ~ Chunk Text, result ~ ResultType) =>
        [MVar (TaskTakes taskid progress result) -> IO ()]
cmds = runIO throwExn : map runProgressIO (countToFive : countToFive' : nekos)

throwExn :: IO result
throwExn = ePutStrLn "About to throw an exception" >> throw LossOfPrecision

runIO :: TaskId taskid => IO result -> MVar (TaskTakes taskid progress result) -> IO ()
runIO io taskTakes =
    try io >>= either (\ e -> putMVar taskTakes (IOToTask (IOException e)))
                      (\ r -> putMVar taskTakes (IOToTask (IOFinished r)))

runProgressIO :: forall taskid progress result. ProgressAndResult progress result =>
                 IO [progress] -> MVar (TaskTakes taskid progress result) -> IO ()
runProgressIO io taskTakes =
    io >>= mapM_ doChunk
    where
      doChunk :: progress -> IO ()
      doChunk x =
          case (taskMessage x :: IOPuts progress result) of
            m@IOCancelled ->
                do -- Notify the manager that the io was cancelled
                   putMVar taskTakes $ IOToTask m
                   -- Raise the ThreadKilled exception that was
                   -- delivered as a chunk.
                   throw ThreadKilled
            m@(IOException e) ->
                do -- Notify the manager that the io was cancelled
                   putMVar taskTakes $ IOToTask m
                   throw e
            m ->
                do ePutStrLn ("Chunk: " ++ ppDisplay x)
                   putMVar taskTakes $ IOToTask $ m

throwChunks :: [Chunk Text] -> IO [Chunk Text]
throwChunks xs =
    evalStateT (mapM throwChunk xs) Nothing
    where throwChunk :: Chunk Text -> StateT (Maybe ProcessHandle) IO (Chunk Text)
          throwChunk x@(ProcessHandle h) = put (Just h) >> return x
          throwChunk (Exception e) = do
            lift (ePutStrLn ("throwChunk " ++ show e))
            get >>= maybe (return ()) (lift . terminateProcess)
            throw e
          throwChunk x = return x

countToFive :: IO [Chunk Text]
countToFive = readCreateProcess (shell $ "bash -c 'echo hello from task 1>&2; for i in " <> intercalate " " (map show ([1..5] :: [Int])) <> "; do echo $i; sleep 1; done'") mempty

-- Re-throw any exceptions that were caught
countToFive' :: IO [Chunk Text]
countToFive' = readCreateProcess (shell $ "bash -c 'echo hello from task 1>&2; for i in " <> intercalate " " (map show ([1..5] :: [Int])) <> "; do echo $i; sleep 1; done'") mempty >>=
               throwChunks

million :: IO [Chunk Text]
million = readCreateProcess (shell $ "yes | head -1000000") mempty
neko :: String -> Int -> IO [Chunk Text]
neko color speed = readCreateProcess (proc "oneko" ["-fg", color, "-speed", show speed]) mempty >>= throwChunks
nekos :: [IO [Chunk Text]]
nekos = map (uncurry neko) (zip (concat (repeat ["red", "green", "blue", "black", "yellow"])) (concat (repeat [12..18])))
