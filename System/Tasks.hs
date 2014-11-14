module System.Tasks
    ( -- * Functions to run the Manager and IO tasks
      manager
    , runIO
    , runProgressIO
    , MonadCancel(cancelIO, evalCancelIO, cancellable)
    , runCancelIO
    -- * Task identifier class
    , TaskId
    -- * Task output class
    , ProgressAndResult(taskMessage)
    , IOPuts(..)
    -- * Message Types by Recipient
    , TaskTakes(..)
    , ManagerTakes(..)
    , TopTakes(..)
    -- * Message Types by Sender and Recipient
    , TaskToManager(..)
    , ManagerToTask(..)
    , TopToManager(..)
    , ManagerToTop(..)
    , ManagerStatus(..)
    ) where

import System.Tasks.IO (runIO, runProgressIO, MonadCancel(cancelIO, evalCancelIO, cancellable), runCancelIO)
import System.Tasks.Manager (manager)
import System.Tasks.Types

