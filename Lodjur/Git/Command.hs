module Lodjur.Git.Command
  ( gitCmd
  , gitCmdLogged
  ) where

import           Control.Exception          (Exception, throwIO)
import           System.Exit
import           System.Process

import           Lodjur.Output.OutputLogger (OutputLogger,
                                             logCreateProcessWithExitCode)
import           Lodjur.Process

data GitFailed = GitFailed String String Int
  deriving (Eq, Show)

instance Exception GitFailed

gitCmd :: [String] -> FilePath -> IO String
gitCmd args gitWorkingDir = do
  (exitcode, out, err) <- readCreateProcessWithExitCode
    ((proc "git" args) { cwd = Just gitWorkingDir })
    ""
  case exitcode of
    ExitSuccess      -> return out
    ExitFailure code -> throwIO (GitFailed out err code)

gitCmdLogged :: Ref OutputLogger -> [String] -> FilePath -> IO String
gitCmdLogged outputLogger args gitWorkingDir = do
  exitcode <- logCreateProcessWithExitCode
    outputLogger
    ((proc "git" args) { cwd = Just gitWorkingDir })
  case exitcode of
    ExitSuccess      -> return ""
    ExitFailure code -> throwIO (GitFailed "" "" code)

