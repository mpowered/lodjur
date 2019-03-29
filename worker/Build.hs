{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Build where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.Text               as Text
import           System.Process
import           System.Exit

import qualified GitHub.Extra            as GH
import qualified Lodjur.Manager.Messages as Msg

data BuildError = BuildError Int String String

instance Show BuildError where
  show (BuildError code _ _) = "BuildError: build exited with code " <> show code

instance Exception BuildError where
  displayException (BuildError code _ _) = "BuildError: build exited with code " <> show code

data Config = Config
  { buildCmd    :: FilePath
  , buildDebug  :: Bool
  }

instance FromJSON Config where
  parseJSON = withObject "Build Config" $ \o -> do
    buildCmd    <- o .: "command"
    buildDebug  <- o .: "debug"
    return Config{..}

runBuild :: Config -> CreateProcess -> IO Msg.Reply
runBuild Config{..} p = do
  when buildDebug $ putStrLn $ "GIT: " ++ show (cmdspec p)
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode p ""
  when buildDebug $ do
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
      return $ Msg.Completed GH.Success (Just output)
    ExitFailure code
      | code < 0 ->             -- exited due to signal
          return Msg.Cancelled
      | otherwise -> do
          let output = GH.CheckRunOutput
                        { checkRunOutputTitle       = "Build"
                        , checkRunOutputSummary     = "nix-build exited with code " <> Text.pack (show code)
                        , checkRunOutputText        = Just (Text.pack $ unlines $ reverse $ take 100 $ reverse $ lines stdout)
                        , checkRunOutputAnnotations = []
                        }
          return $ Msg.Completed GH.Failure (Just output)

nixBuild :: Config -> [String] -> CreateProcess
nixBuild Config{..} = proc buildCmd

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

build :: Config -> FilePath -> FilePath -> IO Msg.Reply
build env d file =
  runBuild env
    $ withCwd d
    $ nixBuild env [file]
