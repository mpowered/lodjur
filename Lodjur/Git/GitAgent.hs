{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module Lodjur.Git.GitAgent
  ( GitAgent
  , initialize
  , GitAgentMessage (..)
  ) where

import           Control.Exception          (Exception, throwIO)
import           Control.Monad
import qualified Data.Text                  as Text
import           System.Exit
import           System.Process

import           Lodjur.Deployment
import           Lodjur.Git
import           Lodjur.Process
import           Lodjur.Output.OutputLogger (OutputLogger,
                                             logCreateProcessWithExitCode)

newtype GitAgent = GitAgent FilePath

initialize :: FilePath -> IO GitAgent
initialize repoPath =
  return (GitAgent repoPath)

data GitAgentMessage r where
  -- Public messages:
  GetTags :: GitAgentMessage (Sync [Tag])
  FetchTags :: GitAgentMessage Async
  Checkout :: Tag -> Ref OutputLogger -> JobId -> GitAgentMessage (Sync ())

instance Process GitAgent where
  type Message GitAgent = GitAgentMessage

  receive _self (a@(GitAgent repoPath), GetTags) = do
    tags <- gitListTags repoPath
    return (a, tags)

  receive _self (a@(GitAgent repoPath), FetchTags) = do
    gitFetchTags repoPath
    return a

  receive _self (a@(GitAgent repoPath), Checkout tag outputLogger jobid) = do
    gitCheckout outputLogger jobid repoPath tag
    return (a, ())

  terminate _ = return ()

data GitFailed = GitFailed String String Int
  deriving (Eq, Show)

instance Exception GitFailed

gitListTags :: FilePath -> IO [Tag]
gitListTags workingDir = parseTags <$> gitCmd ["tag", "-l"] workingDir
  where parseTags = map Tag . filter (not . Text.null) . Text.lines . Text.pack

gitFetchTags :: FilePath -> IO ()
gitFetchTags = void . gitCmd ["fetch", "--tags"]

gitCheckout :: Ref OutputLogger -> JobId -> FilePath -> Tag -> IO ()
gitCheckout outputLogger jobid workingDir tag =
  void $ gitCmdLogged
    outputLogger
    jobid
    [ "checkout"
    , Text.unpack (unTag tag)
    , "--recurse-submodules"
    ]
    workingDir

gitCmd :: [String] -> FilePath -> IO String
gitCmd args gitWorkingDir = do
  (exitcode, out, err) <- readCreateProcessWithExitCode
    ((proc "git" args) { cwd = Just gitWorkingDir })
    ""
  case exitcode of
    ExitSuccess      -> return out
    ExitFailure code -> throwIO (GitFailed out err code)

gitCmdLogged :: Ref OutputLogger -> JobId -> [String] -> FilePath -> IO String
gitCmdLogged outputLogger jobid args gitWorkingDir = do
  exitcode <- logCreateProcessWithExitCode
    outputLogger
    jobid
    ((proc "git" args) { cwd = Just gitWorkingDir })
  case exitcode of
    ExitSuccess      -> return ""
    ExitFailure code -> throwIO (GitFailed "" "" code)
