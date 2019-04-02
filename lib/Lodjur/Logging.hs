module Lodjur.Logging where

import           Control.Monad
import           Control.Monad.Catch            ( MonadMask
                                                , bracket
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Log
import           Data.Text.Prettyprint.Doc
import           Data.Time.Format
import           System.IO

data LogTarget = LogStdout | LogFile FilePath

type LogMsg = WithSeverity (Doc ())

withHandler
  :: (MonadIO io, MonadMask io)
  => LogTarget
  -> (Handler io LogMsg -> io a)
  -> io a
withHandler tgt a = case tgt of
  LogStdout ->
    logfd stdout
  LogFile file ->
    bracket (liftIO $ openFile file AppendMode) (liftIO . hClose) logfd
 where
  logfd fh = withFDHandler
    defaultBatchingOptions
    fh
    0.4
    80
    (\logger -> a
      (   logger
      .   renderWithTimestamp
            (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S")
            (renderWithSeverity id)
      <=< timestamp
      )
    )

runLogging
  :: (MonadIO io, MonadMask io) => LogTarget -> LoggingT LogMsg io a -> io a
runLogging tgt = withHandler tgt . runLoggingT
