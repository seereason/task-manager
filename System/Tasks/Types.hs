{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module System.Tasks.Types
    ( -- * Classes
      TaskId
    , ProgressAndResult(taskMessage)
    , IOPuts(..)
      -- * Message types by sender and recipient
    , TaskToManager(..)
    , ManagerToTask(..)
    , TopToManager(..)
    , ManagerToTop(..)
    , ManagerStatus(..)
    -- * Message types by recipient
    , TaskTakes(..)
    , ManagerTakes(..)
    , TopTakes(..)
    ) where

import Control.Concurrent (MVar)
import Control.Exception (SomeException)
import Data.Set (Set)

#if DEBUG
import Debug.Show (V)
import Text.PrettyPrint.HughesPJClass (Pretty)
class (Eq taskid, Ord taskid, Enum taskid, Show taskid) => TaskId taskid
class (Show progress, Pretty (V progress), Show result) => ProgressAndResult progress result where
    taskMessage :: progress -> IOPuts progress result
    -- ^ Turn a progress value into a message to the task manager
#else
class (Eq taskid, Ord taskid, Enum taskid) => TaskId taskid
class ProgressAndResult progress result where
    taskMessage :: progress -> IOPuts progress result
    -- ^ Turn a progress value into a message
#endif

-- | Type of messages the task manager receives - either messages
-- from the client or messages from one of the individual tasks.
data ManagerTakes taskid progress result
    = TopToManager (TopToManager taskid progress result)
    | TaskToManager (TaskToManager taskid progress result)

-- | Type of messages the thread controlling an individual task receives.
data TaskTakes taskid progress result
    = ManagerToTask (ManagerToTask taskid)
    | IOToTask (IOPuts progress result)

-- The message types, in order: top <-> manager <-> task <-> process.
-- There is a type for each direction between each of these four.

-- | Type of messages the client can send to the manager.
data TopToManager taskid progress result
    = StartTask taskid (MVar (TaskTakes taskid progress result) -> IO ())
    -- ^ Start a new task.  The client is responsible for generating a
    -- unique taskid for the manager to use.  This simplifies that
    -- task startup protocol - otherwise the client would send the
    -- manager a start task message, the manager would generate an id
    -- and send it back to the client, and the client would have to
    -- figure out which task the taskid applied to.
    | SendManagerStatus
    | ShutDown
    | SendTaskStatus taskid
    | TopToTask (ManagerToTask taskid)

-- | Type of messages the client receives.
newtype TopTakes taskid progress result = TopTakes (ManagerToTop taskid progress result)

-- | Type of messages the manager can send to the client.
data ManagerToTop taskid progress result
    = ManagerFinished -- ^ (Eventual) response to ShutDown
    | ManagerStatus (Set taskid) ManagerStatus -- ^ Response to SendManagerStatus
    | NoSuchTask taskid -- ^ Possible response to TopToTask (which currently can only be CancelTask)
    | TaskStatus taskid Bool -- ^ Response to SendTaskStatus
    | TaskToTop (TaskToManager taskid progress result)

-- | Type of messages the manager can send to a task.
data ManagerToTask taskid
    = CancelTask taskid
    deriving Eq

-- | Type of messages the task can send to the manager.
data TaskToManager taskid progress result = TaskPuts taskid (IOPuts progress result)

-- | The messages that are returned by the 'taskMessage' method of
-- class 'ProgressAndResult'.  This represents how the task manager
-- interprets the task's output.
data IOPuts progress result
    = IOProgress progress
    | IOFinished result
    | IOException SomeException
    | IOCancelled

-- | The manager has two modes, currently the same as the task status.
-- Normally it keeps running whether there are any running tasks or not,
-- in exiting mode it exits as soon as the last task finishes.
data ManagerStatus
    = Running
    | Exiting
    deriving Eq
