{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeSynonymInstances #-}
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
    ( TopTakes(..)
    , ManagerTakes(..)
    , TaskTakes(..)
    , TopToManager(..)
    , ManagerToTop(..)
    , ManagerToTask(..)
    , TaskToManager(..)
    , ProcessToTask
    , ManagerStatus(..)
    ) where

import Data.Set (Set)
import Data.Text.Lazy as Text (Text)
import System.Process (CreateProcess(..){-, ProcessHandle, StdStream(..), CmdSpec(..)-})
import System.Process.Chunks (Chunk(..))

data ManagerTakes taskid
    = TopToManager (TopToManager taskid)
    | TaskToManager (TaskToManager taskid)

data TaskTakes taskid
    = ManagerToTask (ManagerToTask taskid)
    | ProcessToTask ProcessToTask

-- The message types, in order: top <-> manager <-> task <-> process.
-- There is a type for each direction between each of these four.

data TopToManager taskid
    = StartTask taskid CreateProcess Text
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

newtype TopTakes taskid = TopTakes (ManagerToTop taskid)

data ManagerToTop taskid
    = ManagerFinished -- ^ (Eventual) response to ShutDown
    | ManagerStatus (Set taskid) ManagerStatus -- ^ Response to SendManagerStatus
    | NoSuchTask taskid -- ^ Possible response to TopToTask (which currently can only be CancelTask)
    | TaskStatus taskid Bool -- ^ Response to SendTaskStatus
    | TaskToTop (TaskToManager taskid)

data ManagerToTask taskid
    = CancelTask taskid
    deriving Eq

data TaskToManager taskid
    = ProcessToManager taskid ProcessToTask

type ProcessToTask = Chunk Text

-- | The manager has two modes, currently the same as the task status.
-- Normally it keeps running whether there are any running tasks or not,
-- in exiting mode it exits as soon as the last task finishes.
data ManagerStatus
    = Running
    | Exiting
    deriving Eq
