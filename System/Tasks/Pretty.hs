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
import Data.Text (Text)
import Debug.Console (ePutStrLn)
import Debug.Show (V(V))
import System.Process (CreateProcess(..), CmdSpec(..), StdStream(..))
import System.Process.Chunks (showCmdSpecForUser)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), Doc, text)
#endif

instance Show ProcessHandle where
    show _ = "<ProcessHandle>"

deriving instance Show ManagerStatus
deriving instance Show ProcessToTask

#if DEBUG
deriving instance Show CreateProcess
deriving instance Show CmdSpec
deriving instance Show StdStream
deriving instance Show taskid => Show (TopToManager taskid)
deriving instance Show taskid => Show (ManagerToTop taskid)
deriving instance Show taskid => Show (ManagerToTask taskid)
deriving instance Show taskid => Show (TaskToManager taskid)
deriving instance Show taskid => Show (TopTakes taskid)
deriving instance Show (Chunk Text)

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

instance Show taskid => Pretty (V (MVarAction, TopTakes taskid)) where
    pPrint (V (Take, x)) = topPrefix <> text "   " <> ppPrint x <> text " <-"
    pPrint (V (Put, x)) = managerPrefix <> text "<- " <> ppPrint x

instance Show taskid => Pretty (V (MVarAction, ManagerTakes taskid)) where
    pPrint (V (Take, x@(TopToManager _))) = managerPrefix <> text "-> " <> ppPrint x
    pPrint (V (Take, x@(TaskToManager _))) = managerPrefix <> text "   " <> ppPrint x <> text " <-"
    pPrint (V (Put, x@(TopToManager _))) = topPrefix <> text "   " <> ppPrint x <> text " ->"
    pPrint (V (Put, x@(TaskToManager _))) = taskPrefix <> text "<- " <> ppPrint x

instance Show taskid => Pretty (V (MVarAction, TaskTakes taskid)) where
    pPrint (V (Take, x@(ManagerToTask _))) = taskPrefix <> text "-> " <> ppPrint x
    pPrint (V (Take, x@(ProcessToTask _))) = taskPrefix <> text "   " <> ppPrint x <> text " <-"
    pPrint (V (Put, x@(ManagerToTask _))) = managerPrefix <> text "   " <> ppPrint x <> text " ->"
    pPrint (V (Put, x@(ProcessToTask _))) = processPrefix <> text "<- " <> ppPrint x

instance Show taskid => Pretty (V (TopTakes taskid)) where
    pPrint (V (TopTakes x)) = ppPrint x

instance Show taskid => Pretty (V (TopToManager taskid)) where
    pPrint (V (StartTask _taskid p _input)) = text "sh: " <> ppPrint p
    pPrint (V (TopToTask x)) = ppPrint x
    pPrint (V x) = text (show x)

instance Show taskid => Pretty (V (ManagerToTop taskid)) where
    pPrint (V (TaskToTop x)) = ppPrint x
    pPrint (V x) = text (show x)

instance Show taskid => Pretty (V (ManagerTakes taskid)) where
    pPrint (V (TopToManager x)) = ppPrint x
    pPrint (V (TaskToManager x)) = ppPrint x

instance Show taskid => Pretty (V (ManagerToTask taskid)) where
    pPrint (V x) = text (show x)

instance Show taskid => Pretty (V (TaskToManager taskid)) where
    pPrint (V (ProcessToManager i x)) = text ("P" <> show i <> ": ") <> pPrint (V x)
    pPrint (V (TaskCancelled i)) = text ("P" <> show i <> ": cancelled")
    pPrint (V (TaskFinished i)) = text ("P" <> show i <> ": finished")

instance Show taskid => Pretty (V (TaskTakes taskid)) where
    pPrint (V (ManagerToTask x)) = ppPrint (x)
    pPrint (V (ProcessToTask x)) = ppPrint (x)

instance Pretty (V CreateProcess) where
    pPrint (V x) = text (showCmdSpecForUser (cmdspec x))

instance Pretty (V ProcessToTask) where
    pPrint (V (Exception e)) = text (show (V e))
    pPrint (V x) = text (show x)

instance Pretty (V (Chunk Text)) where
    pPrint (V (Exception e)) = text (show (V e))
    pPrint (V x) = text (show x)
#endif
