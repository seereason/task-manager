{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module System.Tasks.Pretty
    (
#if DEBUG
      MVarAction
    , System.Tasks.Pretty.takeMVar
    , System.Tasks.Pretty.putMVar
#endif
    ) where

import System.Process (ProcessHandle)
import System.Process.Chunks (Chunk(..))
import System.Tasks.Types

#if DEBUG
import Control.Concurrent as C (MVar, putMVar, takeMVar)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Debug.Console (ePutStrLn)
import Debug.Show (V(V))
import System.Process (CreateProcess(..), CmdSpec(..), StdStream(..))
import System.Process.Chunks (showCmdSpecForUser)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), Doc, text)
#endif

instance Show ProcessHandle where
    show _ = "<ProcessHandle>"

deriving instance Show ManagerStatus

#if DEBUG
deriving instance Show CreateProcess
deriving instance Show CmdSpec
deriving instance Show StdStream
deriving instance (Show taskid, Show progress, Show result) => Show (ManagerToTop taskid progress result)
deriving instance Show taskid => Show (ManagerToTask taskid)
deriving instance (Show taskid, Show progress, Show result) => Show (TaskToManager taskid progress result)
deriving instance (Show taskid, Show progress, Show result) => Show (TopTakes taskid progress result)
deriving instance Show (Chunk Text)

instance Show taskid => Show (TopToManager taskid progress result) where
    show (StartTask i _) = "StartTask " ++ show i
    show ShutDown = "ShutDown"
    show SendManagerStatus = "SendManagerStatus"
    show (SendTaskStatus i) = "SendTaskStatus " ++ show i
    show (TopToTask x) = show x

ppPrint :: Pretty (V a) => a -> Doc
ppPrint = pPrint . V

ppDisplay :: Pretty (V a) => a -> String
ppDisplay = show . ppPrint

topPrefix :: Doc
topPrefix = text ""
managerPrefix :: Doc
managerPrefix = text "\t\t\t"
taskPrefix :: Doc
taskPrefix = text "\t\t\t\t\t\t"
processPrefix :: Doc
processPrefix = text "\t\t\t\t\t\t\t\t\t"

-- Used to format the progress messages - Take has an incoming arrow,
-- put has an outgoing arrow.
data MVarAction = Put | Take deriving Show

-- Versions of takeMVar and putMVar that pretty print debugging output.
takeMVar :: Pretty (V (MVarAction, a)) => MVar a -> IO a
takeMVar v = C.takeMVar v >>= \ x -> ePutStrLn (ppDisplay (Take, x)) >> return x

putMVar :: Pretty (V (MVarAction, a)) => MVar a -> a -> IO ()
putMVar v x = ePutStrLn (ppDisplay (Put, x)) >> C.putMVar v x

instance (Show taskid, Pretty (V progress), Show progress, Show result) => Pretty (V (MVarAction, TopTakes taskid progress result)) where
    pPrint (V (Take, x)) = topPrefix <> text "   " <> ppPrint x <> text " <-"
    pPrint (V (Put, x)) = managerPrefix <> text "<- " <> ppPrint x

instance (Show taskid, Pretty (V progress), Show result) => Pretty (V (MVarAction, ManagerTakes taskid progress result)) where
    pPrint (V (Take, x@(TopToManager _))) = managerPrefix <> text "-> " <> ppPrint x
    pPrint (V (Take, x@(TaskToManager _))) = managerPrefix <> text "   " <> ppPrint x <> text " <-"
    pPrint (V (Put, x@(TopToManager _))) = topPrefix <> text "   " <> ppPrint x <> text " ->"
    pPrint (V (Put, x@(TaskToManager _))) = taskPrefix <> text "<- " <> ppPrint x

instance (Show taskid, Pretty (V progress)) => Pretty (V (MVarAction, TaskTakes taskid progress)) where
    pPrint (V (Take, x@(ManagerToTask _))) = taskPrefix <> text "-> " <> ppPrint x
    pPrint (V (Take, x@(ProcessToTask _))) = taskPrefix <> text "   " <> ppPrint x <> text " <-"
    pPrint (V (Put, x@(ManagerToTask _))) = managerPrefix <> text "   " <> ppPrint x <> text " ->"
    pPrint (V (Put, x@(ProcessToTask _))) = processPrefix <> text "<- " <> ppPrint x

instance (Show taskid, Pretty (V progress), Show progress, Show result) => Pretty (V (TopTakes taskid progress result)) where
    pPrint (V (TopTakes x)) = ppPrint x

instance Show taskid => Pretty (V (TopToManager taskid progress result)) where
    pPrint (V (StartTask taskid _run)) = text "sh: " <> text (show taskid)
    pPrint (V (TopToTask x)) = ppPrint x
    pPrint (V x) = text (show x)

instance (Show taskid, Pretty (V progress), Show progress, Show result) => Pretty (V (ManagerToTop taskid progress result)) where
    pPrint (V (TaskToTop x)) = ppPrint x
    pPrint (V x) = text (show x)

instance (Show taskid, Pretty (V progress), Show result) => Pretty (V (ManagerTakes taskid progress result)) where
    pPrint (V (TopToManager x)) = ppPrint x
    pPrint (V (TaskToManager x)) = ppPrint x

instance Show taskid => Pretty (V (ManagerToTask taskid)) where
    pPrint (V x) = text (show x)

instance (Show taskid, Pretty (V progress), Show result) => Pretty (V (TaskToManager taskid progress result)) where
    pPrint (V (ProcessToManager i x)) = text ("P" <> show i <> ": ") <> pPrint (V x)
    pPrint (V (TaskCancelled i)) = text ("P" <> show i <> ": cancelled")
    pPrint (V (TaskFinished i result)) = text ("P" <> show i <> " result: " ++ show result)

instance (Show taskid, Pretty (V progress)) => Pretty (V (TaskTakes taskid progress)) where
    pPrint (V (ManagerToTask x)) = ppPrint (x)
    pPrint (V (ProcessToTask x)) = ppPrint (x)

instance Pretty (V CreateProcess) where
    pPrint (V x) = text (showCmdSpecForUser (cmdspec x))

instance Pretty (V (Chunk Text)) where
    pPrint (V (Exception e)) = text (show (V e))
    pPrint (V x) = text (show x)
#endif
