module Lodjur.Deploy
    ( DeployHistory
    , DeployEvent (..)
    , DeployState(..)
    , LodjurState(..)
    , LodjurEnv
    , newLodjurEnv
    , currentState
    , Tag
    , listTags
    , deployTag
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Semigroup
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Time.Clock
import           System.Exit
import           System.Process

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
  }

newLodjurEnv :: IO LodjurEnv
newLodjurEnv = do
  mvar <- newMVar (LodjurState Idle [])
  qsem <- newQSem 4
  return (LodjurEnv mvar qsem)

currentState :: LodjurEnv -> IO LodjurState
currentState = readMVar . lodjurStateVar

type Tag = Text

data GitFailed = GitFailed String String Int
  deriving (Eq, Show)

instance Exception GitFailed

data NixopsFailed = NixopsFailed String String Int
  deriving (Eq, Show)

instance Exception NixopsFailed

gitCmd :: [String] -> IO String
gitCmd args = do
  let gitWorkingDir = "/home/owi/projects/mpowered/nixops-empty"
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode ((proc "git" args) { cwd = Just gitWorkingDir }) ""
  case exitcode of
      ExitSuccess      -> return stdout
      ExitFailure code -> throwIO (GitFailed stdout stderr code)

nixopsCmd :: [String] -> IO String
nixopsCmd args = do
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode (proc "nixops" args) ""
  case exitcode of
      ExitSuccess      -> return stdout
      ExitFailure code -> throwIO (NixopsFailed stdout stderr code)

gitListTags :: IO [Tag]
gitListTags = do
  out <- gitCmd ["tag", "-l"]
  return $ filter (not . Text.null) $ Text.lines $ Text.pack out

runDeploy :: Tag -> IO String
runDeploy tag = do
  let deployment = "empty-deployment"
  _ <- gitCmd ["checkout", Text.unpack tag]
  nixopsCmd ["deploy", "-d", deployment]

listTags :: LodjurEnv -> IO [Tag]
listTags LodjurEnv { lodjurGitSem = qsem } =
  bracket_ (waitQSem qsem) (signalQSem qsem) gitListTags

transitionState
  :: MVar LodjurState
  -> (DeployState -> IO (DeployState, [DeployEvent]))
  -> IO ()
transitionState var f = do
  LodjurState state history <- takeMVar var
  (state', es)              <- f state
  putMVar var (LodjurState state' (es <> history))

deployTag :: LodjurEnv -> Tag -> IO ()
deployTag LodjurEnv { lodjurStateVar = var } tag = do
  now <- getCurrentTime
  transitionState var $ \_ -> return (Deploying tag, [DeployStarted tag now])
  void $ forkFinally (runDeploy tag) finishDeploy
 where
  finishDeploy (Left ex) = do
    now <- getCurrentTime
    transitionState var
      $ \_ -> return (Idle, [DeployFailed tag now (Text.pack (show ex))])

  finishDeploy (Right _output) = do
    now <- getCurrentTime
    let ev = DeployFinished tag now
    transitionState var $ \_ -> return (Idle, [ev])
