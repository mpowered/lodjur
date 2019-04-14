{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Build where

import           Control.Exception
import           Control.Monad.Reader
import qualified Data.Text               as Text
import           System.Process
import           System.Exit

import qualified Lodjur.GitHub           as GH
import qualified Lodjur.Job              as Job

import           Config
import           Env
import           Types

data BuildError = BuildError Int String String

instance Show BuildError where
  show (BuildError code _ _) = "BuildError: build exited with code " <> show code

instance Exception BuildError where
  displayException (BuildError code _ _) = "BuildError: build exited with code " <> show code

runBuild :: CreateProcess -> Worker Job.Result
runBuild p = liftIO $ do
  putStrLn $ "GIT: " <> show (cmdspec p)
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode p ""
  mapM_ putStrLn [ "> " <> l | l <- lines stdout ]
  mapM_ putStrLn [ ">> " <> l | l <- lines stderr ]
  case exitcode of
    ExitSuccess -> do
      let output = GH.CheckRunOutput
                     { checkRunOutputTitle       = "Build"
                     , checkRunOutputSummary     = Text.unlines
                        [ "nix-build completed successfully"
                        , "`" <> Text.pack (last $ lines stdout) <> "`"
                        ]
                     , checkRunOutputText        = Nothing
                     , checkRunOutputAnnotations = []
                     }
      return $ Job.Result Job.Success (Just output) [] Nothing
    ExitFailure code
      | code < 0 ->             -- exited due to signal
          return $ Job.Result Job.Cancelled Nothing [] Nothing
      | otherwise -> do
          let txt = Text.pack $ unlines $ reverse $
                       take 50 (reverse $ lines stdout) ++
                       take 50 (reverse $ lines stderr)
          let output = GH.CheckRunOutput
                        { checkRunOutputTitle       = "Build"
                        , checkRunOutputSummary     = "nix-build exited with code " <> Text.pack (show code)
                        , checkRunOutputText        = Just txt
                        , checkRunOutputAnnotations = []
                        }
          return $ Job.Result Job.Failure (Just output) [] Nothing

nixBuild :: BuildConfig -> [String] -> CreateProcess
nixBuild BuildConfig{..} = proc buildCmd

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

build :: FilePath -> FilePath -> Worker Job.Result
build d file = do
  cfg <- asks Env.buildCfg
  runBuild $ withCwd d $ nixBuild cfg [file]
