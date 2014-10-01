{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module System.Tasks.Pretty
    ( PP(PP)
    , MVarAction
    , System.Tasks.Pretty.takeMVar
    , System.Tasks.Pretty.putMVar
    ) where

import Control.Concurrent as C (MVar, putMVar, takeMVar)
import Data.Monoid ((<>))
import Debug.Console (ePutStrLn)
import System.Process (CreateProcess(cmdspec))
import System.Process.Chunks (showCmdSpecForUser)
import System.Tasks.Types
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), Doc, text)

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

-- Used to format the progress messages - Take has an incoming arrow,
-- put has an outgoing arrow.
data MVarAction = Put | Take deriving Show

-- Versions of takeMVar and putMVar that pretty print debugging output.
takeMVar :: Pretty (PP (MVarAction, a)) => MVar a -> IO a
takeMVar v = C.takeMVar v >>= \ x -> ePutStrLn (ppDisplay (Take, x)) >> return x

putMVar :: Pretty (PP (MVarAction, a)) => MVar a -> a -> IO ()
putMVar v x = ePutStrLn (ppDisplay (Put, x)) >> C.putMVar v x

instance Show taskid => Pretty (PP (MVarAction, TopTakes taskid)) where
    pPrint (PP (Take, x)) = topPrefix <> text "   " <> ppPrint x <> text " <-"
    pPrint (PP (Put, x)) = managerPrefix <> text "<- " <> ppPrint x

instance Show taskid => Pretty (PP (MVarAction, ManagerTakes taskid)) where
    pPrint (PP (Take, x@(TopToManager _))) = managerPrefix <> text "-> " <> ppPrint x
    pPrint (PP (Take, x@(TaskToManager _))) = managerPrefix <> text "   " <> ppPrint x <> text " <-"
    pPrint (PP (Put, x@(TopToManager _))) = topPrefix <> text "   " <> ppPrint x <> text " ->"
    pPrint (PP (Put, x@(TaskToManager _))) = taskPrefix <> text "<- " <> ppPrint x

instance Show taskid => Pretty (PP (MVarAction, TaskTakes taskid)) where
    pPrint (PP (Take, x@(ManagerToTask _))) = taskPrefix <> text "-> " <> ppPrint x
    pPrint (PP (Take, x@(ProcessToTask _))) = taskPrefix <> text "   " <> ppPrint x <> text " <-"
    pPrint (PP (Put, x@(ManagerToTask _))) = managerPrefix <> text "   " <> ppPrint x <> text " ->"
    pPrint (PP (Put, x@(ProcessToTask _))) = processPrefix <> text "<- " <> ppPrint x

instance Show taskid => Pretty (PP (TopTakes taskid)) where
    pPrint (PP (TopTakes x)) = ppPrint x

instance Show taskid => Pretty (PP (TopToManager taskid)) where
    pPrint (PP (StartTask p _input)) = text "sh: " <> ppPrint p
    pPrint (PP (TopToTask x)) = ppPrint x
    pPrint (PP x) = text (show x)

instance Show taskid => Pretty (PP (ManagerToTop taskid)) where
    pPrint (PP (TaskToTop x)) = ppPrint x
    pPrint (PP x) = text (show x)

instance Show taskid => Pretty (PP (ManagerTakes taskid)) where
    pPrint (PP (TopToManager x)) = ppPrint x
    pPrint (PP (TaskToManager x)) = ppPrint x

instance Show taskid => Pretty (PP (ManagerToTask taskid)) where
    pPrint (PP x) = text (show x)

instance Show taskid => Pretty (PP (TaskToManager taskid)) where
    pPrint (PP (ProcessToManager i x)) = text ("P" <> show i <> ": ") <> text (show x)

instance Show taskid => Pretty (PP (TaskTakes taskid)) where
    pPrint (PP (ManagerToTask x)) = ppPrint x
    pPrint (PP (ProcessToTask x)) = ppPrint x

instance Pretty (PP ProcessToTask) where
    pPrint (PP x) = text (show x)

instance Pretty (PP CreateProcess) where
    pPrint (PP x) = text (showCmdSpecForUser (cmdspec x))
