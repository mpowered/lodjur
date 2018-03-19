{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module Lodjur.Output.OutputLogger
  ( Output (..)
  , OutputLogs
  , OutputLogger
  , OutputLogMessage (..)
  , initialize
  , logCreateProcessWithExitCode
  ) where

import           Control.Concurrent
import           Control.Exception      (tryJust)
import           Control.Monad          (void)
import           Data.Time.Clock
import           System.Exit
import           System.IO
import           System.IO.Error        (isEOFError)
import           System.Process

import           Lodjur.Database        (DbPool)
import           Lodjur.Deployment
import           Lodjur.Output
import qualified Lodjur.Output.Database as Database
import           Lodjur.Process

newtype OutputLogger = OutputLogger DbPool

initialize :: DbPool -> IO OutputLogger
initialize pool = do
  Database.initialize pool
  return (OutputLogger pool)

data OutputLogMessage r where
  -- Public messages:
  AppendOutput :: JobId -> [String] -> OutputLogMessage Async
  GetOutputLogs :: OutputLogMessage (Sync OutputLogs)

instance Process OutputLogger where
  type Message OutputLogger = OutputLogMessage

  receive _self (a@(OutputLogger pool), AppendOutput jobid lines') = do
    now <- getCurrentTime
    Database.appendOutput pool jobid (Output now lines')
    return a

  receive _self (a@(OutputLogger pool), GetOutputLogs) = do
    logs <- Database.getAllOutputLogs pool
    return (a, logs)

  terminate _ = return ()

logCreateProcessWithExitCode
  :: Ref OutputLogger -> JobId -> CreateProcess -> IO ExitCode
logCreateProcessWithExitCode outputLogger jobid cp = do
  let cp_opts =
        cp { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }

  (_, Just hout, Just herr, ph) <- createProcess cp_opts
  void $ logStream outputLogger jobid hout
  void $ logStream outputLogger jobid herr
  waitForProcess ph

logStream :: Ref OutputLogger -> JobId -> Handle -> IO ThreadId
logStream logger jobid h = forkIO go
 where
  go = do
    next <- tryJust (\e -> if isEOFError e then Just () else Nothing)
                    (hGetLine h)
    case next of
      Left  _    -> return ()
      Right line -> do
        logger ! AppendOutput jobid [line]
        go
