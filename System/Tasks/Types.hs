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
    ( -- * Types
      TopTakes(..)
    , ManagerTakes(..)
    , TaskTakes(..)
    , TopToManager(..)
    , ManagerToTop(..)
    , ManagerToTask(..)
    , TaskToManager(..)
    , ProcessToTask
    , ManagerStatus(..)
    -- * Pretty Printing
    , PP(PP)
    , ppPrint
    , ppDisplay
    , MVarAction(Put, Take)
    ) where

import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text.Lazy as Text (Text)
import System.Process (CmdSpec, CreateProcess(cmdspec))
import System.Process.ListLike (Chunk, showCmdSpecForUser)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), Doc, text)

data ManagerTakes taskid
    = TopToManager (TopToManager taskid)
    | TaskToManager (TaskToManager taskid)
    deriving Show

data TaskTakes
    = ManagerToTask ManagerToTask
    | ProcessToTask ProcessToTask
    deriving Show

-- The message types, in order: top <-> manager <-> task <-> process.
-- There is a type for each direction between each of these four.

data TopToManager taskid
    = StartTask CreateProcess Text
    | SendManagerStatus
    | ShutDown
    | SendTaskStatus taskid
    | TopToTask taskid ManagerToTask
    deriving Show

newtype TopTakes taskid = TopTakes (ManagerToTop taskid) deriving Show

data ManagerToTop taskid
    = ManagerFinished -- ^ (Eventual) response to ShutDown
    | ManagerStatus (Set taskid) ManagerStatus -- ^ Response to SendManagerStatus
    | NoSuchTask taskid -- ^ Possible response to TopToTask (which currently can only be CancelTask)
    | TaskStatus taskid Bool -- ^ Response to SendTaskStatus
    | TaskToTop (TaskToManager taskid)
    deriving Show

data ManagerToTask
    = CancelTask
    deriving (Show, Eq)

data TaskToManager taskid
    = ProcessToManager taskid ProcessToTask
    deriving Show

type ProcessToTask = Chunk Text

-- | The manager has two modes, currently the same as the task status.
-- Normally it keeps running whether there are any running tasks or not,
-- in exiting mode it exits as soon as the last task finishes.
data ManagerStatus
    = Running'
    | Exiting'
    deriving (Show, Eq)

instance Show CmdSpec where
    show x = "(CmdSpec " ++ showCmdSpecForUser x ++ ")"

instance Show CreateProcess where
    show x = "(CreateProcess " ++ show (cmdspec x) ++ ")"

-- Pretty printing

newtype PP a = PP a

ppPrint :: Pretty (PP a) => a -> Doc
ppPrint = pPrint . PP

ppDisplay :: Pretty (PP a) => a -> String
ppDisplay = show . ppPrint

topPrefix :: Doc
topPrefix = text ""
managerPrefix :: Doc
managerPrefix = text "\t\t\t"
taskPrefix :: Doc
taskPrefix = text "\t\t\t\t\t\t"
processPrefix :: Doc
processPrefix = text "\t\t\t\t\t\t\t\t\t"

data MVarAction = Put | Take deriving Show

instance Show taskid => Pretty (PP (MVarAction, TopTakes taskid)) where
    pPrint (PP (Take, x)) = topPrefix <> text "   " <> ppPrint x <> text " <-"
    pPrint (PP (Put, x)) = managerPrefix <> text "<- " <> ppPrint x

instance Show taskid => Pretty (PP (MVarAction, ManagerTakes taskid)) where
    pPrint (PP (Take, x@(TopToManager _))) = managerPrefix <> text "-> " <> ppPrint x
    pPrint (PP (Take, x@(TaskToManager _))) = managerPrefix <> text "   " <> ppPrint x <> text " <-"
    pPrint (PP (Put, x@(TopToManager _))) = topPrefix <> text "   " <> ppPrint x <> text " ->"
    pPrint (PP (Put, x@(TaskToManager _))) = taskPrefix <> text "<- " <> ppPrint x

instance Pretty (PP (MVarAction, TaskTakes)) where
    pPrint (PP (Take, x@(ManagerToTask _))) = taskPrefix <> text "-> " <> ppPrint x
    pPrint (PP (Take, x@(ProcessToTask _))) = taskPrefix <> text "   " <> ppPrint x <> text " <-"
    pPrint (PP (Put, x@(ManagerToTask _))) = managerPrefix <> text "   " <> ppPrint x <> text " ->"
    pPrint (PP (Put, x@(ProcessToTask _))) = processPrefix <> text "<- " <> ppPrint x

instance Show taskid => Pretty (PP (TopTakes taskid)) where
    pPrint (PP (TopTakes x)) = ppPrint x

instance Show taskid => Pretty (PP (TopToManager taskid)) where
    pPrint (PP x) = text (show x)

instance Show taskid => Pretty (PP (ManagerToTop taskid)) where
    pPrint (PP (TaskToTop x)) = ppPrint x
    pPrint (PP x) = text (show x)

instance Show taskid => Pretty (PP (ManagerTakes taskid)) where
    pPrint (PP (TopToManager x)) = ppPrint x
    pPrint (PP (TaskToManager x)) = ppPrint x

instance Pretty (PP ManagerToTask) where
    pPrint (PP x) = text (show x)

instance Show taskid => Pretty (PP (TaskToManager taskid)) where
    pPrint (PP (ProcessToManager i x)) = text ("P" <> show i <> ": ") <> text (show x)

instance Pretty (PP TaskTakes) where
    pPrint (PP (ManagerToTask x)) = ppPrint x
    pPrint (PP (ProcessToTask x)) = ppPrint x

instance Pretty (PP ProcessToTask) where
    pPrint (PP x) = text (show x)
