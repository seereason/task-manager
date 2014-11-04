{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, Rank2Types, ScopedTypeVariables, StandaloneDeriving, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module System.Tasks.IO
    ( MonadCancel(cancelIO, evalCancelIO)
    , runIO
    , runProgressIO
    , runCancelIO
    ) where

import Control.Concurrent (MVar, putMVar)
import Control.Exception (AsyncException(ThreadKilled), fromException, SomeException, throw)
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.Trans (MonadIO, liftIO)
import System.Tasks.Types (TaskId, ProgressAndResult(taskMessage), TaskTakes(IOToTask), IOPuts(..))

#if DEBUG
import Debug.Console (ePutStrLn)
import System.Tasks.Pretty (ppDisplay)
#endif

exceptionMessage :: SomeException -> IOPuts progress result
exceptionMessage e = case fromException e of
                  Just ThreadKilled -> IOCancelled
                  _ -> IOException e

runIO :: TaskId taskid => IO result -> MVar (TaskTakes taskid progress result) -> IO ()
runIO io taskTakes =
    try io >>= either (\ e -> putMVar taskTakes (IOToTask (exceptionMessage e)))
                      (\ r -> putMVar taskTakes (IOToTask (IOFinished r)))

-- | Monad transformer that runs an environment where we can (try to)
-- cancel the computation of a monad.
class Monad m => MonadCancel t m where
    cancelIO :: t m ()
    evalCancelIO :: (forall a. t m a -> m a)

-- | Run an IO task with progress output.
runProgressIO :: forall taskid progress result. (ProgressAndResult progress result) =>
                  IO [progress] -> MVar (TaskTakes taskid progress result) -> IO ()
runProgressIO io taskTakes =
    -- Process all the progress messages and then process the
    -- terminating exception (if any.)
    try (io >>= mapM_ doProgress) >>= either (doTaskMessage . IOException) return
    where
      -- Turn progress values into messages to the task manager
      doProgress :: progress -> IO ()
      doProgress x = doTaskMessage (taskMessage x :: IOPuts progress result)
      doTaskMessage :: IOPuts progress result -> IO ()
      doTaskMessage m@IOCancelled =
          do -- Notify the manager that the io was cancelled
             putMVar taskTakes $ IOToTask m
             -- Raise the ThreadKilled exception that was
             -- delivered as a chunk.
             throw ThreadKilled
      doTaskMessage m@(IOException e) =
          do -- Notify the manager that an exception was thrown
             putMVar taskTakes $ IOToTask m
             throw e
      doTaskMessage m =
#if DEBUG
          do ePutStrLn ("Chunk: " ++ ppDisplay m)
#endif
             putMVar taskTakes $ IOToTask $ m

-- | Run an IO task with progress output *and* the ability to cancel.
runCancelIO :: forall m taskid progress result. (MonadIO (m IO), MonadCatch (m IO), ProgressAndResult progress result, MonadCancel m IO) =>
               m IO [progress] -> MVar (TaskTakes taskid progress result) -> IO ()
runCancelIO io taskTakes =
    evalCancelIO $ try (io >>= mapM_ doProgress) >>= either (doTaskMessage . IOException) return
    where
      doProgress :: progress -> m IO ()
      doProgress x = doTaskMessage (taskMessage x :: IOPuts progress result)
      doTaskMessage :: IOPuts progress result -> m IO ()
      doTaskMessage m@IOCancelled =
          do liftIO $ putMVar taskTakes $ IOToTask m
             cancelIO     -- Invoke the task's cancel operation
             throw ThreadKilled
      doTaskMessage m@(IOException e) =
          do liftIO $ putMVar taskTakes $ IOToTask m
             throw e
      doTaskMessage m =
#if DEBUG
          do ePutStrLn ("Chunk: " ++ ppDisplay m)
#endif
             liftIO $ putMVar taskTakes $ IOToTask $ m
