{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Lodjur.Output.OutputLogger
  ( Output (..)
  , OutputLogger
  , OutputLogMessage (..)
  , initialize
  , logCreateProcessWithExitCode
  ) where

import           Control.Concurrent
import           Control.Exception          (tryJust)
import           Control.Monad              (guard, void)
import           Data.Time.Clock
import           System.Exit
import           System.IO
import           System.IO.Error            (isEOFError)
import           System.Process

import           Lodjur.Database            (DbPool)
import           Lodjur.Deployment          (JobId)
import           Lodjur.Output
import qualified Lodjur.Output.Database     as Database
import           Lodjur.Process

data OutputLogger = OutputLogger { dbPool :: DbPool , jobId :: JobId }

initialize :: DbPool -> JobId -> IO OutputLogger
initialize dbPool jobId = return OutputLogger {..}

data OutputLogMessage r where
  -- Public messages:
  AppendOutput :: [String] -> OutputLogMessage Async
  OutputFence :: OutputLogMessage (Sync ())
  GetOutputLog :: OutputLogMessage (Sync [Output])

instance Process OutputLogger where
  type Message OutputLogger = OutputLogMessage

  receive _self (logger, AppendOutput lines') = do
    now <- getCurrentTime
    Database.appendOutput (dbPool logger) (jobId logger) now lines'
    return logger

  receive _self (logger, OutputFence) = do
    Database.fence (dbPool logger) (jobId logger)
    return (logger, ())

  receive _self (logger, GetOutputLog) = do
    out <- Database.getOutputLog (dbPool logger) Nothing Nothing (jobId logger)
    return (logger, out)

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
logStream logger h done = forkIO $ go []
 where
  go lns = do
    void $ tryJust (guard . isEOFError) $ do
      let nlines = length lns
      lns' <-
        if nlines >= 100
          then do
            logger ! AppendOutput (reverse lns)
            return []
          else
            return lns
      let delay = if nlines > 0 then 100 else 10000
      ready <- hWaitForInput h delay
      if ready
        then do
          line <- hGetLine h
          go (line : lns')
        else
          go lns'
    putMVar done ()
