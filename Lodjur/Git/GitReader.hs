{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module Lodjur.Git.GitReader
  ( GitReader
  , initialize
  , GitReaderMessage (..)
  ) where

import qualified Data.Text          as Text

import           Lodjur.Git
import           Lodjur.Git.Command
import           Lodjur.Process

newtype GitReader = GitReader FilePath

initialize :: FilePath -> IO GitReader
initialize repoPath =
  return (GitReader repoPath)

data GitReaderMessage r where
  -- Public messages:
  GetTags :: GitReaderMessage (Sync [Tag])

instance Process GitReader where
  type Message GitReader = GitReaderMessage

  receive _self (a@(GitReader repoPath), GetTags) = do
    tags <- gitListTags repoPath
    return (a, tags)

  terminate _ = return ()

gitListTags :: FilePath -> IO [Tag]
gitListTags workingDir = parseTags <$> gitCmd ["tag", "-l"] workingDir
  where parseTags = map Tag . filter (not . Text.null) . Text.lines . Text.pack
