{-# LANGUAGE CPP, FlexibleContexts, GADTs, Rank2Types, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Trans (lift)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Monoid
import Data.Text.Lazy as Text (empty)
import Debug.Console (ePutStrLn)
import System.Process (CreateProcess, shell, proc)
import System.Process.Text.Lazy ()
import System.Tasks (manager)
import System.Tasks.Types (ManagerTakes(..), TopToManager(..), ManagerToTask(..), TopTakes)

type TaskId = Integer

main :: IO ()
main = manager (`evalStateT` (cmds, 1)) keyboard output

-- | The output device
output :: TopTakes TaskId -> IO ()
output = ePutStrLn . show

-- | The input device
keyboard :: StateT ([CreateProcess], TaskId) IO (ManagerTakes TaskId)
keyboard = do
  input <- lift $ getLine
  case input of
    -- Start a new task
    "t" -> get >>= \ ((cmd : more), nextId) -> put (more, succ nextId) >> return (TopToManager (StartTask nextId cmd empty))
    -- Get the status of a task
    ['s',d] | isDigit d -> return (TopToManager (SendTaskStatus (read [d])))
    -- Get process manager status
    "s" -> return (TopToManager SendManagerStatus)
    -- Kill a task
    ['k',d] | isDigit d -> return (TopToManager $ TopToTask $ CancelTask $ read [d])
    -- Initiate shutdown and exit keyboard loop
    "x" -> return (TopToManager ShutDown)
    -- error
    x -> ePutStrLn (show x ++ " - expected: t, s, s<digit>, k<digit>, or x") >> keyboard

cmds :: [CreateProcess]
cmds = countToFive : nekos

countToFive :: CreateProcess
countToFive = shell $ "bash -c 'echo hello from task 1>&2; for i in " <> intercalate " " (map show ([1..5] :: [Int])) <> "; do echo $i; sleep 1; done'"
million :: CreateProcess
million = shell $ "yes | head -1000000"
neko :: String -> Int -> CreateProcess
neko color speed = proc "oneko" ["-fg", color, "-speed", show speed]
nekos :: [CreateProcess]
nekos = map (uncurry neko) (zip (concat (repeat ["red", "green", "blue", "black", "yellow"])) (concat (repeat [12..18])))
