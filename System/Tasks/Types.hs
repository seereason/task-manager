{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
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

module System.Tasks.Types
    ( TaskId
    , Progress
    , Result
    , TopTakes(..)
    , ManagerTakes(..)
    , TaskTakes(..)
    , TopToManager(..)
    , ManagerToTop(..)
    , ManagerToTask(..)
    , TaskToManager(..)
    , ManagerStatus(..)
    ) where

import Control.Concurrent (MVar)
import Control.Exception (SomeException)
import Data.Set (Set)
import Debug.Show (V)
import Text.PrettyPrint.HughesPJClass (Pretty)

#if DEBUG
-- The Result class only exists to add Show as a superclass when debugging.
class Show result => Result result
class (Show progress, Pretty (V progress)) => Progress progress
class (Eq taskid, Ord taskid, Enum taskid, Show taskid) => TaskId taskid
#else
class Result result
class Progress progress
class (Eq taskid, Ord taskid, Enum taskid) => TaskId taskid
#endif

data ManagerTakes taskid progress result
    = TopToManager (TopToManager taskid progress result)
    | TaskToManager (TaskToManager taskid progress result)

data TaskTakes taskid progress
    = ManagerToTask (ManagerToTask taskid)
    | ProcessToTask progress

-- The message types, in order: top <-> manager <-> task <-> process.
-- There is a type for each direction between each of these four.

data TopToManager taskid progress result
    = StartTask taskid (MVar (TaskTakes taskid progress) -> IO result)
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

newtype TopTakes taskid progress result = TopTakes (ManagerToTop taskid progress result)

data ManagerToTop taskid progress result
    = ManagerFinished -- ^ (Eventual) response to ShutDown
    | ManagerStatus (Set taskid) ManagerStatus -- ^ Response to SendManagerStatus
    | NoSuchTask taskid -- ^ Possible response to TopToTask (which currently can only be CancelTask)
    | TaskStatus taskid Bool -- ^ Response to SendTaskStatus
    | TaskToTop (TaskToManager taskid progress result)

data ManagerToTask taskid
    = CancelTask taskid
    deriving Eq

data TaskToManager taskid progress result
    = ProcessToManager taskid progress
    | TaskFinished taskid result
    | TaskException taskid SomeException
    | TaskCancelled taskid

-- | The manager has two modes, currently the same as the task status.
-- Normally it keeps running whether there are any running tasks or not,
-- in exiting mode it exits as soon as the last task finishes.
data ManagerStatus
    = Running
    | Exiting
    deriving Eq
