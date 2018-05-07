{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Lodjur.Git.GitReader
  ( GitReader
  , initialize
  , GitReaderMessage (..)
  ) where

import           Data.Maybe         (mapMaybe)
import qualified Data.Text          as Text

import qualified Lodjur.Git         as Git
import           Lodjur.Git.Command
import           Lodjur.Process

newtype GitReader = GitReader FilePath

initialize :: FilePath -> IO GitReader
initialize repoPath =
  return (GitReader repoPath)

data GitReaderMessage r where
  -- Public messages:
  GetRevisions :: GitReaderMessage (Sync [Git.Revision])
  GetRefs :: GitReaderMessage (Sync [Git.Ref])

instance Process GitReader where
  type Message GitReader = GitReaderMessage
  receive _self =
    \case
      (a@(GitReader repoPath), GetRevisions) -> do
        versions <- gitListRevisions repoPath
        return (a, versions)
      (a@(GitReader repoPath), GetRefs) -> do
        refs <- gitListRefs repoPath
        return (a, refs)
  terminate _ = return ()

gitListRevisions :: FilePath -> IO [Git.Revision]
gitListRevisions workingDir =
  parseRevs <$> gitCmd ["rev-list", "--all", "--remotes=*"] workingDir
  where
    parseRevs =
      map Git.Revision . filter (not . Text.null) . Text.lines . Text.pack

gitListRefs :: FilePath -> IO [Git.Ref]
gitListRefs workingDir =
  parseShowRefPairs <$> gitCmd ["show-ref"] workingDir
  where
    parseShowRefPairs =
      mapMaybe toPair . map Text.words . Text.lines . Text.pack
      where
        toPair [hash, ref] =
          case Text.splitOn "/" ref of
            ("refs" : "remotes" : "origin" : branch) ->
              Just (Git.Branch (Text.intercalate "/" ("origin" : branch)) (Git.Revision hash))
            ("refs" : "tags" : tag) ->
              Just (Git.Tag (Text.intercalate "/" tag) (Git.Revision hash))
            _ -> Nothing
        toPair _ = Nothing
