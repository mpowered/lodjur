{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lodjur.Deploy
    ( DeployHistory
    , DeployEvent (..)
    , DeployState(..)
    , LodjurState(..)
    , LodjurEnv
    , Tag (..)
    , Deployment
    , newLodjurEnv
    , currentState
    , listTags
    , deployTag
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Semigroup
import           Data.Text          (Text)
import qualified Data.Text          as Text
import Data.String (IsString)
import           Data.Time.Clock
import           System.Exit
import           System.Process (proc, readCreateProcessWithExitCode, CreateProcess (cwd))

type DeployHistory = [DeployEvent]

data DeployEvent
  = DeployStarted Tag UTCTime
  | DeployFinished Tag UTCTime
  | DeployFailed Tag UTCTime Text

data DeployState = Idle | Deploying Tag

data LodjurState = LodjurState DeployState DeployHistory

data LodjurEnv = LodjurEnv
  { lodjurStateVar :: MVar LodjurState
  , lodjurGitSem   :: QSem
  , lodjurDeployment :: Deployment
  , lodjurGitWorkingDir :: FilePath
  }

newtype Tag = Tag { unTag :: Text } deriving (Eq, Show, IsString)

newtype Deployment = Deployment String deriving (Eq, Show, IsString)

newLodjurEnv :: Deployment -> FilePath -> IO LodjurEnv
newLodjurEnv deployment workingDir = do
  mvar <- newMVar (LodjurState Idle [])
  qsem <- newQSem 4
  return (LodjurEnv mvar qsem deployment workingDir)

currentState :: LodjurEnv -> IO LodjurState
currentState = readMVar . lodjurStateVar

data GitFailed = GitFailed String String Int
  deriving (Eq, Show)

instance Exception GitFailed

data NixopsFailed = NixopsFailed String String Int
  deriving (Eq, Show)

instance Exception NixopsFailed

gitCmd :: [String] -> FilePath -> IO String
gitCmd args gitWorkingDir = do
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode
    ((proc "git" args) { cwd = Just gitWorkingDir })
    ""
  case exitcode of
    ExitSuccess      -> return stdout
    ExitFailure code -> throwIO (GitFailed stdout stderr code)

nixopsCmd :: [String] -> IO String
nixopsCmd args = do
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode
    (proc "nixops" args)
    ""
  case exitcode of
    ExitSuccess      -> return stdout
    ExitFailure code -> throwIO (NixopsFailed stdout stderr code)

gitListTags :: LodjurEnv -> IO [Tag]
gitListTags env = do
  out <- gitCmd ["tag", "-l"] (lodjurGitWorkingDir env)
  return $ map Tag . filter (not . Text.null) . Text.lines $ Text.pack out

runDeploy :: LodjurEnv -> Tag -> IO String
runDeploy env (Tag tag) = do
  _ <- gitCmd ["checkout", Text.unpack tag] (lodjurGitWorkingDir env)
  let Deployment d = lodjurDeployment env
  nixopsCmd ["deploy", "-d", d]

listTags :: LodjurEnv -> IO [Tag]
listTags env@LodjurEnv { lodjurGitSem = qsem } =
  bracket_ (waitQSem qsem) (signalQSem qsem) (gitListTags env)

transitionState
  :: MVar LodjurState
  -> (DeployState -> IO (DeployState, [DeployEvent]))
  -> IO ()
transitionState var f = do
  LodjurState state history <- takeMVar var
  (state', es)              <- f state
  putMVar var (LodjurState state' (es <> history))

deployTag :: LodjurEnv -> Tag -> IO ()
deployTag env@LodjurEnv { lodjurStateVar = var } tag = do
  now <- getCurrentTime
  transitionState var $ \_ -> return (Deploying tag, [DeployStarted tag now])
  void $ forkFinally (runDeploy env tag) finishDeploy
 where
  finishDeploy (Left ex) = do
    now <- getCurrentTime
    transitionState var
      $ \_ -> return (Idle, [DeployFailed tag now (Text.pack (show ex))])

  finishDeploy (Right _output) = do
    now <- getCurrentTime
    let ev = DeployFinished tag now
    transitionState var $ \_ -> return (Idle, [ev])
