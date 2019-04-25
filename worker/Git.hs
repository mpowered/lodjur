{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Git where

import           Control.Concurrent.QSem
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.Text                  as Text
import           System.Directory
import           System.Directory.Internal.Prelude (isAlreadyExistsError, isDoesNotExistError)
import           System.FilePath
import           System.Process
import           System.Exit

import qualified Lodjur.GitHub              as GH

newtype GitError = GitError Int

instance Show GitError where
  show (GitError code) = "GitError: git exited with code " <> show code

instance Exception GitError where
  displayException (GitError code) = "GitError: git exited with code " <> show code

data Config = Config
  { gitCmd      :: FilePath
  , gitCache    :: FilePath
  , gitWorkRoot :: FilePath
  , gitDebug    :: Bool
  }

instance FromJSON Config where
  parseJSON = withObject "Git Config" $ \o -> do
    gitCmd      <- o .: "command"
    gitCache    <- o .: "cache"
    gitWorkRoot <- o .: "workdir"
    gitDebug    <- o .: "debug"
    return Config{..}

data Env = Env
  { gitCmd      :: FilePath
  , gitCache    :: FilePath
  , gitWorkRoot :: FilePath
  , gitDebug    :: Bool
  , gitSem      :: QSem
  }

setupEnv :: Config -> IO Env
setupEnv Config{..} = do
  gitSem <- newQSem 1
  return Env{..}

runGit :: Env -> CreateProcess -> IO ()
runGit Env{..} p =
  bracket_ (waitQSem gitSem) (signalQSem gitSem) $ do
    when gitDebug $ putStrLn $ "GIT: " ++ show (cmdspec p)
    (exitcode, stdout, stderr) <- readCreateProcessWithExitCode p ""
    when gitDebug $ do
      mapM_ putStrLn [ "> " <> l | l <- lines stdout ]
      mapM_ putStrLn [ ">> " <> l | l <- lines stderr ]
    case exitcode of
      ExitSuccess -> return ()
      ExitFailure code -> throwIO $ GitError code

git :: Env -> [String] -> CreateProcess
git Env{..} = proc gitCmd

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

login :: GH.RepoRef -> String
login = Text.unpack . GH.untagName . GH.simpleOwnerLogin . GH.repoRefOwner

name :: GH.RepoRef -> String
name = Text.unpack . GH.untagName . GH.repoRefRepo

githubUrlFor :: Env -> GH.GitHubCommit -> String
githubUrlFor _ GH.GitHubCommit{..} = "https://github.com/" <> Text.unpack ghcOwner <> "/" <> Text.unpack ghcRepo <> ".git"

cachePathFor :: Env -> GH.GitHubCommit -> FilePath
cachePathFor Env{..} GH.GitHubCommit{..} = gitCache </> Text.unpack ghcOwner </> Text.unpack ghcRepo

cacheClone :: Env -> GH.GitHubCommit -> IO ()
cacheClone env@Env{..} commit =
  runGit env
    $ git env ["clone", githubUrlFor env commit, "--mirror", cachePathFor env commit]

cacheUpdate :: Env -> GH.GitHubCommit -> IO ()
cacheUpdate env commit =
  runGit env
    $ withCwd (cachePathFor env commit)
    $ git env ["remote", "update", "--prune"]

cacheGetRepo :: Env -> GH.GitHubCommit -> IO FilePath
cacheGetRepo env commit = do
  let repoPath = cachePathFor env commit
  exists <- doesPathExist repoPath
  if exists
    then cacheUpdate env commit
    else cacheClone env commit
  return repoPath

checkout :: Env -> GH.GitHubCommit -> IO FilePath
checkout env commit = do
  workdir <- createNewWorkDir env (Text.unpack $ GH.ghcSha commit)
  repoPath <- cacheGetRepo env commit
  runGit env
    $ git env ["clone", repoPath, workdir]
  runGit env
    $ withCwd workdir
    $ git env ["checkout", "--detach", Text.unpack $ GH.ghcSha commit]
  return workdir

createNewWorkDir :: Env -> String -> IO FilePath
createNewWorkDir Env{..} sha = go 0
  where
    go :: Int -> IO FilePath
    go n = do
      e <- tryJust (guard . isAlreadyExistsError)
                   (createDirectory (workdir n) >> return (workdir n))
      case e of
        Left _ -> go (succ n)
        Right d -> return d

    workdir 0 = gitWorkRoot </> sha
    workdir n = workdir 0 <> "-" <> show n

deleteWorkDir :: FilePath -> IO ()
deleteWorkDir path = do
  e <- tryJust (guard . isDoesNotExistError) (removeDirectoryRecursive path)
  return $ either (const ()) id e
