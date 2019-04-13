{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Build where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Log
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Text               as Text
import           Data.Text.Prettyprint.Doc
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

runBuild :: GH.Source -> CreateProcess -> Worker Job.Result
runBuild src p = do
  BuildConfig{..} <- asks Env.buildCfg
  liftIO $ putStrLn $ "GIT: " <> show (cmdspec p)
  -- logDebug $ "GIT: " <> viaShow (cmdspec p)
  (exitcode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode p ""
  -- logDebug $ "GIT stdout:" <> line <> indent 4 (vsep (map pretty $ lines stdout))
  -- logDebug $ "GIT stderr:" <> line <> indent 4 (vsep (map pretty $ lines stderr))
  liftIO $ do
    mapM_ putStrLn [ "> " <> l | l <- lines stdout ]
    mapM_ putStrLn [ ">> " <> l | l <- lines stderr ]
  case exitcode of
    ExitSuccess -> do
      -- let output = GH.CheckRunOutput
      --                { checkRunOutputTitle       = "Build"
      --                , checkRunOutputSummary     = Text.unlines
      --                   [ "nix-build completed successfully"
      --                   , "`" <> Text.pack (last $ lines stdout) <> "`"
      --                   ]
      --                , checkRunOutputText        = Nothing
      --                , checkRunOutputAnnotations = []
      --                }
      return $ Job.Result Job.Success []
                [ Job.Request "check-toolkit1" src (Job.Check "toolkit1")
                , Job.Request "check-toolkit2" src (Job.Check "toolkit2")
                , Job.Request "check-toolkit3" src (Job.Check "toolkit3")
                , Job.Request "check-sms"      src (Job.Check "sms")
                , Job.Request "check-beagle"   src (Job.Check "beagle")
                ]
    ExitFailure code
      | code < 0 ->             -- exited due to signal
          return $ Job.Result Job.Cancelled [] []
      | otherwise -> do
          -- let txt = Text.pack $ unlines $ reverse $
          --              take 50 (reverse $ lines stdout) ++
          --              take 50 (reverse $ lines stderr)
          -- let output = GH.CheckRunOutput
          --               { checkRunOutputTitle       = "Build"
          --               , checkRunOutputSummary     = "nix-build exited with code " <> Text.pack (show code)
          --               , checkRunOutputText        = Just txt
          --               , checkRunOutputAnnotations = []
          --               }
          return $ Job.Result Job.Failure [] []

nixBuild :: BuildConfig -> [String] -> CreateProcess
nixBuild BuildConfig{..} = proc buildCmd

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

build :: FilePath -> FilePath -> GH.Source -> Worker Job.Result
build d file src = do
  cfg <- asks Env.buildCfg
  runBuild src $ withCwd d $ nixBuild cfg [file]
