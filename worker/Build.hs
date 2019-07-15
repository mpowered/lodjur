{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Build where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad.Log
import           Control.Monad.Reader
import qualified Data.Text               as Text
import           Data.Text.Prettyprint.Doc
import           System.IO                ( Handle, hGetLine )
import           System.IO.Error          ( isEOFError )
import           System.Process
import           System.Exit

import qualified Lodjur.GitHub           as GH
import qualified Lodjur.Job              as Job

import           Env
import           Types

data BuildError = BuildError Int String String

instance Show BuildError where
  show (BuildError code _ _) = "BuildError: build exited with code " <> show code

instance Exception BuildError where
  displayException (BuildError code _ _) = "BuildError: build exited with code " <> show code

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

runBuild :: Chan Job.Reply -> CreateProcess -> Worker Job.Result
runBuild chan p = do
  logDebug $ "BUILD: " <> viaShow (cmdspec p)
  exitcode <- liftIO $ process chan p
  case exitcode of
    ExitSuccess -> do
      let output = GH.CheckRunOutput
                     { checkRunOutputTitle       = "Build"
                     , checkRunOutputSummary     = Text.unlines
                        [ "nix-build completed successfully"
                        ]
                     , checkRunOutputText        = Nothing
                     , checkRunOutputAnnotations = []
                     }
      return $ Job.Result Job.Success (Just output) [] Nothing
    ExitFailure code
      | code < 0 ->             -- exited due to signal
          return $ Job.Result Job.Cancelled Nothing [] Nothing
      | otherwise -> do
          let output = GH.CheckRunOutput
                        { checkRunOutputTitle       = "Build"
                        , checkRunOutputSummary     = "nix-build exited with code " <> Text.pack (show code)
                        , checkRunOutputText        = Nothing   -- TODO
                        , checkRunOutputAnnotations = []
                        }
          return $ Job.Result Job.Failure (Just output) [] Nothing

nixBuild :: BuildEnv -> [String] -> CreateProcess
nixBuild BuildEnv{..} = proc envBuildCommand

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

build :: Chan Job.Reply -> FilePath -> FilePath -> Worker Job.Result
build chan d file = do
  cfg <- asks Env.envBuild
  runBuild chan $ withCwd d $ nixBuild cfg [file]
