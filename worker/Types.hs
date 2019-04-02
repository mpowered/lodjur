module Types where

import           Control.Monad.Log
import           Control.Monad.Reader

import           Env
import           Logging

type Worker = ReaderT Env (LoggingT LogMsg IO)

runWorker :: Env -> Worker a -> IO a
runWorker env = runLogging (logFile env) . flip runReaderT env
