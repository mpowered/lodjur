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
  FetchRemote :: GitAgentMessage Async
  Checkout :: Git.Revision -> Ref OutputLogger -> GitAgentMessage (Sync ())

instance Process GitAgent where
  type Message GitAgent = GitAgentMessage

  receive _self (a@(GitAgent repoPath), FetchRemote) = do
    gitFetchRemote repoPath
    return a

  receive _self (a@(GitAgent repoPath), Checkout tag outputLogger) = do
    gitCheckout outputLogger repoPath tag
    return (a, ())

  terminate _ = return ()

gitFetchRemote :: FilePath -> IO ()
gitFetchRemote = void . gitCmd ["fetch", "--tags", "--prune", "-f", "origin"]

gitCheckout :: Ref OutputLogger -> FilePath -> Git.Revision -> IO ()
gitCheckout outputLogger workingDir revision =
  void $ gitCmdLogged
    outputLogger
    [ "checkout"
    , Text.unpack (Git.unRevision revision)
    ]
    workingDir
