{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Git where

import           Control.Concurrent.QSem
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Log
import           Data.String.Conversions            ( cs )
import           Data.Text.Prettyprint.Doc
import           System.Directory
import           System.Directory.Internal.Prelude  ( isAlreadyExistsError, isDoesNotExistError )
import           System.FilePath
import           System.Process
import           System.Exit

import qualified Lodjur.GitHub              as GH
import           Lodjur.Logging

import           Env

newtype GitError = GitError Int

instance Show GitError where
  show (GitError code) = "GitError: git exited with code " <> show code

instance Exception GitError where
  displayException (GitError code) = "GitError: git exited with code " <> show code

runGit :: (MonadIO m, MonadMask m, MonadLog LogMsg m) => GitEnv -> CreateProcess -> m ()
runGit GitEnv{..} p =
  bracket_ (liftIO $ waitQSem envGitSem) (liftIO $ signalQSem envGitSem) $ do
    logDebug $ "GIT: " <> viaShow (cmdspec p)
    (exitcode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode p ""
    when envGitDebug $ do
      mapM_ logDebug [ "> " <> pretty l | l <- lines stdout ]
      mapM_ logDebug [ ">> " <> pretty l | l <- lines stderr ]
    case exitcode of
      ExitSuccess -> return ()
      ExitFailure code -> do
        logError $ "git failed with code " <> viaShow code <> ":"
                    <> line <> vsep (pretty <$> lines stdout)
                    <> line <> vsep (pretty <$> lines stderr)
        throwM $ GitError code

git :: GitEnv -> [String] -> CreateProcess
git GitEnv{..} = proc envGitCommand

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

login :: GH.RepoRef -> String
login = cs . GH.untagName . GH.simpleOwnerLogin . GH.repoRefOwner

name :: GH.RepoRef -> String
name = cs . GH.untagName . GH.repoRefRepo

githubUrlFor :: GitEnv -> GH.GitHubCommit -> Maybe GH.Token -> String
githubUrlFor _ GH.GitHubCommit{..} Nothing = "https://github.com/" <> cs ghcOwner <> "/" <> cs ghcRepo <> ".git"
githubUrlFor _ GH.GitHubCommit{..} (Just token) = "https://x-access-token:" <> cs token <> "@github.com/" <> cs ghcOwner <> "/" <> cs ghcRepo <> ".git"

cachePathFor :: GitEnv -> GH.GitHubCommit -> FilePath
cachePathFor GitEnv{..} GH.GitHubCommit{..} = envGitCache </> cs ghcOwner </> cs ghcRepo

cacheClone :: (MonadIO m, MonadMask m, MonadLog LogMsg m) => GitEnv -> GithubEnv -> GH.GitHubCommit -> m ()
cacheClone env@GitEnv{..} GithubEnv{..} commit = do
  token <- liftIO $ GH.ensureToken envGithubInstallationAccessToken
  runGit env
    $ git env ["clone", githubUrlFor env commit (Just token), "--mirror", cachePathFor env commit]

cacheUpdate :: (MonadIO m, MonadMask m, MonadLog LogMsg m) => GitEnv -> GithubEnv -> GH.GitHubCommit -> m ()
cacheUpdate env GithubEnv{..} commit = do
  -- TODO: switch to use a custom credential manager?
  token <- liftIO $ GH.ensureToken envGithubInstallationAccessToken
  runGit env
    $ withCwd (cachePathFor env commit)
    $ git env ["remote", "set-url", "origin", githubUrlFor env commit (Just token)]
  runGit env
    $ withCwd (cachePathFor env commit)
    $ git env ["remote", "update", "--prune"]

cacheGetRepo :: (MonadIO m, MonadMask m, MonadLog LogMsg m) => GitEnv -> GithubEnv -> GH.GitHubCommit -> m FilePath
cacheGetRepo env ghenv commit = do
  let repoPath = cachePathFor env commit
  exists <- liftIO $ doesPathExist repoPath
  if exists
    then cacheUpdate env ghenv commit
    else cacheClone env ghenv commit
  return repoPath

checkout :: (MonadIO m, MonadMask m, MonadLog LogMsg m) => GitEnv -> GithubEnv -> GH.GitHubCommit -> m FilePath
checkout env ghenv commit = do
  workdir <- createNewWorkDir env (cs $ GH.ghcSha commit)
  repoPath <- cacheGetRepo env ghenv commit
  runGit env
    $ git env ["clone", repoPath, workdir]
  runGit env
    $ withCwd workdir
    $ git env ["checkout", "--detach", cs $ GH.ghcSha commit]
  return workdir

createNewWorkDir :: forall m. (MonadIO m, MonadMask m, MonadLog LogMsg m) => GitEnv -> String -> m FilePath
createNewWorkDir GitEnv{..} sha = go 0
  where
    go :: Int -> m FilePath
    go n = do
      e <- tryJust (guard . isAlreadyExistsError)
                   (liftIO $ createDirectory (workdir n) >> return (workdir n))
      case e of
        Left _ -> go (succ n)
        Right d -> return d

    workdir 0 = envGitWorkRoot </> sha
    workdir n = workdir 0 <> "-" <> show n

deleteWorkDir :: (MonadIO m, MonadMask m, MonadLog LogMsg m) => FilePath -> m ()
deleteWorkDir path = do
  e <- tryJust (guard . isDoesNotExistError) (liftIO $ removeDirectoryRecursive path)
  return $ either (const ()) id e
