module Types where

-- import           Control.Monad.Log
import           Control.Monad.Reader

import           Env
-- import           Lodjur.Logging

-- type Worker = ReaderT Env (LoggingT LogMsg IO)

-- runWorker :: Env -> Worker a -> IO a
-- runWorker env = runLogging (logTarget env) . flip runReaderT env
--
type Worker = ReaderT Env IO

runWorker :: Env -> Worker a -> IO a
runWorker = flip runReaderT
