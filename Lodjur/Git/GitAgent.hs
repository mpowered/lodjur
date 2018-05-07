{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module Lodjur.Git.GitAgent
  ( GitAgent
  , initialize
  , GitAgentMessage (..)
  ) where

import           Control.Monad
import qualified Data.Text                  as Text

import qualified Lodjur.Git                 as Git
import           Lodjur.Git.Command
import           Lodjur.Output.OutputLogger (OutputLogger)
import           Lodjur.Process

newtype GitAgent = GitAgent FilePath

initialize :: FilePath -> IO GitAgent
initialize repoPath =
  return (GitAgent repoPath)

data GitAgentMessage r where
  -- Public messages:
  FetchTags :: GitAgentMessage Async
  Checkout :: Git.Revision -> Ref OutputLogger -> GitAgentMessage (Sync ())

instance Process GitAgent where
  type Message GitAgent = GitAgentMessage

  receive _self (a@(GitAgent repoPath), FetchTags) = do
    gitFetchTags repoPath
    return a

  receive _self (a@(GitAgent repoPath), Checkout tag outputLogger) = do
    gitCheckout outputLogger repoPath tag
    return (a, ())

  terminate _ = return ()

gitFetchTags :: FilePath -> IO ()
gitFetchTags = void . gitCmd ["fetch", "--tags", "--prune", "origin", "tag", "*"]

gitCheckout :: Ref OutputLogger -> FilePath -> Git.Revision -> IO ()
gitCheckout outputLogger workingDir revision = do
  void $ gitCmdLogged
    outputLogger
    [ "checkout"
    , Text.unpack (Git.unRevision revision)
    ]
    workingDir
  void $ gitCmdLogged
    outputLogger
    [ "submodule"
    , "update"
    , "--recursive"
    ]
    workingDir
