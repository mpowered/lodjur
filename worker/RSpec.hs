{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Log
import           Data.Aeson
import qualified Data.Text               as Text
import           Data.Text.Prettyprint.Doc
import           System.Directory
import           System.FilePath
import           System.IO                ( Handle, hGetLine )
import           System.IO.Error          ( isEOFError )
import           System.Process
import           System.Exit

import qualified Lodjur.GitHub           as GH
import qualified Lodjur.Job              as Job
import           Lodjur.RSpec

import           Types

logh :: Chan Job.Reply -> Handle -> IO ()
logh chan h = go
 where
  go = do
    l <- try $ hGetLine h
    case l of
      Left e
        | isEOFError e -> return ()
        | otherwise    -> go
      Right logline -> do
        writeChan chan (Job.LogOutput (Text.pack logline))
        go

process :: Chan Job.Reply -> CreateProcess -> IO ExitCode
process chan cp = do
  let cp_opts = cp { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }
  (_, Just hout, Just herr, ph) <- createProcess cp_opts
  out <- async $ logh chan hout
  err <- async $ logh chan herr
  code <- waitForProcess ph
  _ <- wait out
  _ <- wait err
  return code

runRSpec :: Chan Job.Reply -> CreateProcess -> Worker ExitCode
runRSpec chan p = do
  logDebug $ "RSPEC: " <> viaShow (cmdspec p)
  liftIO $ process chan p

rspecCmd :: [String] -> CreateProcess
rspecCmd = proc ".lodjur/rspec"

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

rspec :: Chan Job.Reply -> FilePath -> String -> Worker Job.Result
rspec chan d app = do
  liftIO $ createDirectoryIfMissing True (d </> checkOutput)
  exitcode <- runRSpec chan $ withCwd d $ rspecCmd [app, checkOutput]
  case exitcode of
    ExitSuccess -> do
      r <- liftIO $ parseCheckResults (d </> checkOutput)
      case r of
        Just result@RSpecResult{..} -> do
          let RSpecSummary{..} = rspecSummary
          if rspecFailureCount > 0
           then do
            let output = GH.CheckRunOutput
                          { checkRunOutputTitle       = "RSpec"
                          , checkRunOutputSummary     = Text.unlines
                              [ "rspec completed successfully"
                              , showText rspecFailureCount <> " of " <> showText rspecExampleCount <> " tests failed."
                              , showText rspecPendingCount <> " tests pending."
                              ]
                          , checkRunOutputText        = Nothing
                          , checkRunOutputAnnotations = []
                          }
            return $ Job.Result Job.Failure (Just output) [] (Just result)
           else do
            let output = GH.CheckRunOutput
                          { checkRunOutputTitle       = "RSpec"
                          , checkRunOutputSummary     = Text.unlines
                              [ "rspec completed successfully"
                              , showText (rspecExampleCount - rspecPendingCount) <> " tests successful."
                              , showText rspecPendingCount <> " tests pending."
                              ]
                          , checkRunOutputText        = Nothing
                          , checkRunOutputAnnotations = []
                          }
            return $ Job.Result Job.Success (Just output) [] (Just result)
        Nothing -> do
          let output = GH.CheckRunOutput
                        { checkRunOutputTitle       = "RSpec"
                        , checkRunOutputSummary     = "unable to parse rspec output"
                        , checkRunOutputText        = Nothing
                        , checkRunOutputAnnotations = []
                        }
          return $ Job.Result Job.Failure (Just output) [] Nothing
    ExitFailure code
      | code < 0 ->             -- exited due to signal
          return $ Job.Result Job.Cancelled Nothing [] Nothing
      | otherwise -> do
          let output = GH.CheckRunOutput
                        { checkRunOutputTitle       = "RSpec"
                        , checkRunOutputSummary     = "rspec exited with code " <> Text.pack (show code)
                        , checkRunOutputText        = Nothing
                        , checkRunOutputAnnotations = []
                        }
          return $ Job.Result Job.Failure (Just output) [] Nothing
 where
  checkOutput = "lodjur-check"
  showText = Text.pack . show

parseCheckResults :: FilePath -> IO (Maybe RSpecResult)
parseCheckResults dir = do
  direntries <- listDirectory dir
  let related = filter ("json" `isExtensionOf`) direntries
  files <- filterM doesFileExist $ map (dir </>) related
  mconcat <$> mapM parseCheckResult files

parseCheckResult :: FilePath -> IO (Maybe RSpecResult)
parseCheckResult = decodeFileStrict'
