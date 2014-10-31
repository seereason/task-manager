{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, Rank2Types, ScopedTypeVariables, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module System.Tasks.IO
    ( runIO
    , runProgressIO
    ) where

import Control.Concurrent (MVar, putMVar)
import Control.Exception (AsyncException(ThreadKilled), throw, try)
import System.Process.Text.Lazy ()
import System.Tasks.Types (TaskId, ProgressAndResult(taskMessage), TaskTakes(IOToTask), IOPuts(..))

#if DEBUG
import Debug.Console (ePutStrLn)
import System.Tasks.Pretty (ppDisplay)
#else
-- Use hPutStrLn instead of the ePutStrLn in Debug.Console.
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO (hPutStrLn, stderr)
ePutStrLn :: MonadIO m => String -> m ()
ePutStrLn = liftIO . hPutStrLn stderr
#endif

runIO :: TaskId taskid => IO result -> MVar (TaskTakes taskid progress result) -> IO ()
runIO io taskTakes =
    try io >>= either (\ e -> putMVar taskTakes (IOToTask (IOException e)))
                      (\ r -> putMVar taskTakes (IOToTask (IOFinished r)))

runProgressIO :: forall taskid progress result. ProgressAndResult progress result =>
                 IO [progress] -> MVar (TaskTakes taskid progress result) -> IO ()
runProgressIO io taskTakes =
    -- add state Maybe ProcessHandle and terminate task on exception
    try (io >>= mapM_ doProgress) >>= either (doTaskMessage . IOException) return
    where
      doProgress :: progress -> IO ()
      doProgress x = doTaskMessage (taskMessage x :: IOPuts progress result)
      doTaskMessage m@IOCancelled =
          do -- Notify the manager that the io was cancelled
             putMVar taskTakes $ IOToTask m
             -- Raise the ThreadKilled exception that was
             -- delivered as a chunk.
             throw ThreadKilled
      doTaskMessage m@(IOException e) =
          do -- Notify the manager that the io was cancelled
             putMVar taskTakes $ IOToTask m
             throw e
      doTaskMessage m =
          do ePutStrLn ("Chunk: " ++ ppDisplay m)
             putMVar taskTakes $ IOToTask $ m
