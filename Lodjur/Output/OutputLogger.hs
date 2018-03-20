{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
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
import           Lodjur.Deployment hiding (jobId)
import           Lodjur.Output
import qualified Lodjur.Output.Database as Database
import           Lodjur.Process

data OutputLogger = OutputLogger { dbPool :: DbPool , jobId :: JobId }

initialize :: DbPool -> JobId -> IO OutputLogger
initialize dbPool jobId = return OutputLogger {..}

data OutputLogMessage r where
  -- Public messages:
  AppendOutput :: [String] -> OutputLogMessage Async
  GetOutputLogs :: OutputLogMessage (Sync OutputLogs)

instance Process OutputLogger where
  type Message OutputLogger = OutputLogMessage

  receive _self (logger, AppendOutput lines') = do
    now <- getCurrentTime
    Database.appendOutput (dbPool logger) (jobId logger) (Output now lines')
    return logger

  receive _self (logger, GetOutputLogs) = do
    logs <- Database.getAllOutputLogs (dbPool logger)
    return (logger, logs)

  terminate _ = return ()

logCreateProcessWithExitCode
  :: Ref OutputLogger -> CreateProcess -> IO ExitCode
logCreateProcessWithExitCode outputLogger cp = do
  let cp_opts =
        cp { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }

  (_, Just hout, Just herr, ph) <- createProcess cp_opts
  outStreamDone <- newEmptyMVar
  errStreamDone <- newEmptyMVar
  void $ logStream outputLogger hout outStreamDone
  void $ logStream outputLogger herr errStreamDone
  code <- waitForProcess ph
  _ <- readMVar outStreamDone
  _ <- readMVar errStreamDone
  return code

logStream :: Ref OutputLogger -> Handle -> MVar () -> IO ThreadId
logStream logger h done = forkIO go
 where
  go = do
    next <- tryJust (\e -> if isEOFError e then Just () else Nothing)
                    (hGetLine h)
    case next of
      Left  _    -> putMVar done ()
      Right line -> do
        logger ! AppendOutput [line]
        go
