{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Worker.Git where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime)
import           System.Directory
import           System.Directory.Internal.Prelude (isAlreadyExistsError, isDoesNotExistError)
import           System.FilePath
import           System.Process
import           System.Exit

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

newtype GitError = GitError Int

instance Show GitError where
  show (GitError code) = "GitError: git exited with code " <> show code

instance Exception GitError where
  displayException (GitError code) = "GitError: git exited with code " <> show code

data Env = Env
  { gitCmd      :: FilePath
  , gitCache    :: FilePath
  , gitWorkRoot :: FilePath
  , gitDebug    :: Bool
  }

instance FromJSON Env where
  parseJSON = withObject "Git Env" $ \o -> do
    gitCmd      <- o .: "command"
    gitCache    <- o .: "cache"
    gitWorkRoot <- o .: "workdir"
    gitDebug    <- o .: "debug"
    return Env{..}

runGit :: Env -> CreateProcess -> IO ()
runGit Env{..} p = do
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

githubUrlFor :: Env -> Repo -> String
githubUrlFor _ Repo{..} = "https://github.com/" <> repoOwner <> "/" <> repoName <> ".git"

cachePathFor :: Env -> Repo -> FilePath
cachePathFor Env{..} Repo{..} = gitCache </> repoOwner </> repoName

cacheClone :: Env -> Repo -> IO ()
cacheClone env@Env{..} repo@Repo{..} =
  runGit env
    $ git env ["clone", githubUrlFor env repo, "--mirror", cachePathFor env repo]

cacheUpdate :: Env -> Repo -> IO ()
cacheUpdate env repo =
  runGit env
    $ withCwd (cachePathFor env repo)
    $ git env ["remote", "update", "--prune"]

cacheGetRepo :: Env -> Repo -> IO FilePath
cacheGetRepo env repo = do
  let repoPath = cachePathFor env repo
  exists <- doesPathExist repoPath
  if exists
    then cacheUpdate env repo
    else cacheClone env repo
  return repoPath

checkout :: Env -> Repo -> String -> IO FilePath
checkout env repo sha = do
  workdir <- createNewWorkDir env sha
  repoPath <- cacheGetRepo env repo
  runGit env
    $ git env ["clone", repoPath, workdir]
  runGit env
    $ withCwd workdir
    $ git env ["checkout", "--detach", sha]
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
