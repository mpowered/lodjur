{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Build where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           System.Process
import           System.Exit

newtype BuildError = BuildError Int

instance Show BuildError where
  show (BuildError code) = "BuildError: build exited with code " <> show code

instance Exception BuildError where
  displayException (BuildError code) = "BuildError: build exited with code " <> show code

data Env = Env
  { buildCmd    :: FilePath
  , buildDebug  :: Bool
  }

instance FromJSON Env where
  parseJSON = withObject "Git Env" $ \o -> do
    buildCmd    <- o .: "command"
    buildDebug  <- o .: "debug"
    return Env{..}

runBuild :: Env -> CreateProcess -> IO ()
runBuild Env{..} p = do
  when buildDebug $ putStrLn $ "GIT: " ++ show (cmdspec p)
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode p ""
  when buildDebug $ do
    mapM_ putStrLn [ "> " <> l | l <- lines stdout ]
    mapM_ putStrLn [ ">> " <> l | l <- lines stderr ]
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> throwIO $ BuildError code

nixBuild :: Env -> [String] -> CreateProcess
nixBuild Env{..} = proc buildCmd

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

build :: Env -> FilePath -> FilePath -> String -> [(String, String)] -> IO ()
build env cwd file attr args =
  runBuild env
    $ withCwd cwd
    $ nixBuild env
    $ [file, "-A", attr] ++ concat [ ["--argstr", arg, str] | (arg, str) <- args ]
