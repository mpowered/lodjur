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

import qualified GitHub.Extra            as GH
import qualified Lodjur.Manager.Messages as Msg

import           Config
import           Env
import           Types

data BuildError = BuildError Int String String

instance Show BuildError where
  show (BuildError code _ _) = "BuildError: build exited with code " <> show code

instance Exception BuildError where
  displayException (BuildError code _ _) = "BuildError: build exited with code " <> show code

runBuild :: CreateProcess -> Worker Msg.Reply
runBuild p = do
  BuildConfig{..} <- asks Env.buildCfg
  logDebug $ "GIT: " <> viaShow (cmdspec p)
  (exitcode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode p ""
  logDebug $ "GIT stdout:" <> line <> indent 4 (vsep (map pretty $ lines stdout))
  logDebug $ "GIT stderr:" <> line <> indent 4 (vsep (map pretty $ lines stderr))
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
      return $ Msg.Completed GH.Success (Just output)
    ExitFailure code
      | code < 0 ->             -- exited due to signal
          return $ Msg.Completed GH.Cancelled Nothing
      | otherwise -> do
          let output = GH.CheckRunOutput
                        { checkRunOutputTitle       = "Build"
                        , checkRunOutputSummary     = "nix-build exited with code " <> Text.pack (show code)
                        , checkRunOutputText        = Just (Text.pack $ unlines $ reverse $ take 100 $ reverse $ lines stdout)
                        , checkRunOutputAnnotations = []
                        }
          return $ Msg.Completed GH.Failure (Just output)

nixBuild :: BuildConfig -> [String] -> CreateProcess
nixBuild BuildConfig{..} = proc buildCmd

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

build :: FilePath -> FilePath -> Worker Msg.Reply
build d file = do
  cfg <- asks Env.buildCfg
  runBuild $ withCwd d $ nixBuild cfg [file]
