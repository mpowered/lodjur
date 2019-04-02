module Logging where

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

type LogMsg = WithSeverity (Doc ())

withHandler
  :: (MonadIO io, MonadMask io)
  => FilePath
  -> (Handler io LogMsg -> io a)
  -> io a
withHandler _file a = --bracket
  --(liftIO $ openFile file AppendMode)
  --(liftIO . hClose)
  (\fh -> withFDHandler
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
  ) stdout

runLogging
  :: (MonadIO io, MonadMask io) => FilePath -> LoggingT LogMsg io a -> io a
runLogging file = withHandler file . runLoggingT
