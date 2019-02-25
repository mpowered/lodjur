{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lodjur.Git where

import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime)
import           System.Directory
import           System.FilePath
import           System.Process

type Hash = Text

data Revision = Revision
  { revisionHash :: Hash
  , revisionTime :: UTCTime
  } deriving (Show, Eq)

data Ref
  = Tag Text
        Revision
  | Branch Text
           Revision
  deriving (Eq, Show)

refRevision :: Ref -> Revision
refRevision (Tag _ rev)    = rev
refRevision (Branch _ rev) = rev

------------------------------------------------------------------------------

type Logger = String -> IO ()

------------------------------------------------------------------------------

-- Specifically a GitHub repo
data Repo = Repo { repoOwner :: String, repoName :: String }

data Options = Options
  { gitCmd      :: FilePath
  , gitCache    :: FilePath
  -- , gitLogger   :: Logger
  }

runGit :: CreateProcess -> IO ()
runGit p = do
  putStrLn $ "GIT: " ++ show (cmdspec p)
  stdout <- readCreateProcess p ""
  putStrLn stdout
  return ()

git :: Options -> [String] -> CreateProcess
git Options{..} = proc gitCmd

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

githubUrlFor :: Options -> Repo -> String
githubUrlFor _ Repo{..} = "https://github.com/" <> repoOwner <> "/" <> repoName <> ".git"

cachePathFor :: Options -> Repo -> FilePath
cachePathFor Options{..} Repo{..} = gitCache </> repoOwner </> repoName

cacheClone :: Options -> Repo -> IO ()
cacheClone o@Options{..} r@Repo{..} =
  runGit
    $ git o ["clone", githubUrlFor o r, "--mirror", cachePathFor o r]

cacheUpdate :: Options -> Repo -> IO ()
cacheUpdate o r =
  runGit
    $ withCwd (cachePathFor o r)
    $ git o ["remote", "update", "--prune"]

cacheGetRepo :: Options -> Repo -> IO FilePath
cacheGetRepo o r = do
  let repoPath = cachePathFor o r
  exists <- doesPathExist repoPath
  if exists
    then cacheUpdate o r
    else cacheClone o r
  return repoPath

checkout :: Options -> Repo -> String -> FilePath -> IO ()
checkout o r sha dest = do
  repoPath <- cacheGetRepo o r
  runGit
    $ git o ["clone", repoPath, dest]
  runGit
    $ withCwd dest
    $ git o ["checkout", "--detach", sha]
