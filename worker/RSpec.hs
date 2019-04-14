{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RSpec where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text               as Text
import           System.Directory
import           System.FilePath
import           System.Process
import           System.Exit

import qualified Lodjur.GitHub           as GH
import qualified Lodjur.Job              as Job
import           Lodjur.RSpec

import           Types

runRSpec :: CreateProcess -> IO ExitCode
runRSpec p = do
  putStrLn $ "RSPEC: " ++ show (cmdspec p)
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode p ""
  mapM_ putStrLn [ "> " <> l | l <- lines stdout ]
  mapM_ putStrLn [ ">> " <> l | l <- lines stderr ]
  return exitcode

rspecCmd :: [String] -> CreateProcess
rspecCmd = proc ".lodjur/rspec"

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

rspec :: FilePath -> String -> Worker Job.Result
rspec d app = liftIO $ do
  createDirectoryIfMissing True (d </> checkOutput)
  exitcode <- runRSpec $ withCwd d $ rspecCmd [app, checkOutput]
  case exitcode of
    ExitSuccess -> do
      r <- parseCheckResults (d </> checkOutput)
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
