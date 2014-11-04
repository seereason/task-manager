{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, Rank2Types, ScopedTypeVariables, StandaloneDeriving, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module System.Tasks.IO
    ( MonadCancel(cancelIO, evalCancelIO, cancellable)
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
-- cancel the computation of a monad.  The cancellable method takes a
-- stream of progress messages and sets up the environment in which then
-- cancelIO method can cancel the IO task.
class Monad taskm => MonadCancel progress cancelt taskm | cancelt -> progress where
    evalCancelIO :: cancelt taskm a -> taskm a
    -- ^ Run a cancellable IO task
    cancellable :: taskm [progress] -> cancelt taskm [progress]
    -- ^ Turn an IO task into a cancellable IO task
    cancelIO :: cancelt taskm ()
    -- ^ Cancel this IO task

-- | Run an IO task with progress output.
runProgressIO :: forall taskid progress result. ProgressAndResult progress result =>
                 IO [progress] -> MVar (TaskTakes taskid progress result) -> IO ()
runProgressIO io taskTakes =
    -- Process all the progress messages and then process the
    -- terminating exception (if any.)
    try (io >>= mapM_ doProgress) >>= either (doTaskMessage . IOException) return
    where
      -- Turn progress values into messages to the task manager
      -- doProgress :: progress -> IO ()
      doProgress x = doTaskMessage (taskMessage x {- :: IOPuts progress result -})
      -- doTaskMessage :: IOPuts progress result -> IO ()
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
runCancelIO :: forall taskid progress result cancelt taskm.
               (MonadCancel progress cancelt taskm,
                MonadCatch (cancelt taskm),
                MonadIO (cancelt taskm),
                ProgressAndResult progress result) =>
               cancelt taskm [progress]
            -> MVar (TaskTakes taskid progress result) -> taskm ()
runCancelIO io taskTakes =
    evalCancelIO $ try (io >>= mapM_ doProgress) >>= either (doTaskMessage . IOException) return
    where
      -- doProgress :: progress -> m IO ()
      doProgress x = doTaskMessage (taskMessage x {- :: IOPuts progress result -})
      -- doTaskMessage :: IOPuts progress result -> m IO ()
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
