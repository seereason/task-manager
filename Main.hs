{-# LANGUAGE CPP, FlexibleContexts, GADTs, Rank2Types, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Concurrent (forkIO, MVar, newEmptyMVar)
#ifdef DEBUG
import System.Tasks.Pretty (putMVar, takeMVar)
#else
import Control.Concurrent (putMVar, takeMVar)
#endif
import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Monoid
import Data.Text.Lazy as Text (empty)
import Debug.Console (ePutStrLn)
import System.Process (CreateProcess, shell, proc)
import System.Process.Text.Lazy ()
import System.Tasks (manager)
import System.Tasks.Types (TopTakes(..), ManagerToTop(..), ManagerTakes(..),
                           TopToManager(..), ManagerToTask(..))

type TaskId = Integer

firstTaskId :: TaskId
firstTaskId = 1

main :: IO ()
main = runManager keyboard (ePutStrLn . show)

runManager :: forall m. (m ~ StateT [CreateProcess] IO) =>
              (m (ManagerTakes TaskId))
           -> (TopTakes TaskId -> IO ())
           -> IO ()
runManager putter taker = do
  topTakes <- newEmptyMVar
  managerTakes <- newEmptyMVar
  forkIO $ manager firstTaskId topTakes managerTakes
  forkIO $ takeLoop topTakes
  evalStateT (putLoop managerTakes) cmds
    where
      takeLoop topTakes =
          do msg <- takeMVar topTakes
             taker msg
             case msg of
               TopTakes ManagerFinished -> return ()
               _ -> takeLoop topTakes

      putLoop :: MVar (ManagerTakes TaskId) -> m ()
      putLoop managerTakes =
          do msg <- putter
             liftIO $ putMVar managerTakes msg
             case msg of
               TopToManager ShutDown -> return ()
               _ -> putLoop managerTakes

keyboard :: StateT [CreateProcess] IO (ManagerTakes TaskId)
keyboard = do
  input <- lift $ getLine
  case input of
    -- Start a new task
    "t" -> get >>= \ (cmd : more) -> put more >> return (TopToManager (StartTask cmd empty))
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

{-
keyboard :: (Show taskid, Read taskid) =>
            MVar (ManagerTakes taskid)
         -> IO ()
keyboard managerTakes =
    loop (countToFive : nekos)
    where
      loop cmds@(cmd : next) =
          do input <- getLine
             case input of
               -- Start a new task
               "t" -> putMVar managerTakes (TopToManager (StartTask cmd empty)) >> loop next
               -- Get the status of a task
               ['s',d] | isDigit d -> putMVar managerTakes (TopToManager (SendTaskStatus (read [d]))) >> loop cmds
               -- Get process manager status
               "s" -> putMVar managerTakes  (TopToManager SendManagerStatus) >> loop cmds
               -- Kill a task
               ['k',d] | isDigit d -> putMVar managerTakes  (TopToManager $ TopToTask $ CancelTask $ read [d]) >> loop cmds
               -- Initiate shutdown and exit keyboard loop
               "x" -> putMVar managerTakes  (TopToManager ShutDown)
               -- error
               x -> ePutStrLn (show x ++ " - expected: t, s, s<digit>, k<digit>, or x") >> loop cmds
-}


cmds = countToFive : nekos

countToFive :: CreateProcess
countToFive = shell $ "bash -c 'echo hello from task 1>&2; for i in " <> intercalate " " (map show ([1..5] :: [Int])) <> "; do echo $i; sleep 1; done'"
million :: CreateProcess
million = shell $ "yes | head -1000000"
neko :: String -> Int -> CreateProcess
neko color speed = proc "oneko" ["-fg", color, "-speed", show speed]
nekos :: [CreateProcess]
nekos = map (uncurry neko) (zip (concat (repeat ["red", "green", "blue", "black", "yellow"])) (concat (repeat [12..18])))

{-
class ManagerClient taskid where
    putter :: ManagerTakes taskid -> IO ()
    taker :: TopTakes taskid -> IO ()

    startTask :: CreateProcess -> Text -> IO ()
    taskStatus :: taskid -> IO ()
    managerStatus :: IO ()
    killTask :: taskid -> IO ()
    managerShutDown :: IO ()

class Taker taskid a where
    managerFinished :: IO ()
    managerStatus :: Set taskid -> ManagerStatus - IO ()
    noSuchTask :: taskid -> IO ()
    taskStatus :: taskid -> Bool-> IO ()
    processOutput :: Chunk a -> IO ()

top :: Show TaskId => MVar (TopTakes TaskId) -> IO ()
top topTakes = loop
    where
      loop = do
        msg <- takeMVar topTakes
        ePutStrLn (show msg)
        case msg of
          TopTakes ManagerFinished -> return ()
          TopTakes (ManagerStatus _tids _mode) -> loop
          _ -> loop
-}
