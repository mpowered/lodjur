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

import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Exception
import Control.Concurrent
import System.Process
import Control.Monad
import Data.Semigroup

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

us :: Int
us = 1000000

gitListTags :: IO [Tag]
gitListTags = do
    threadDelay (1 * us)
    out <- readProcess "git" ["tag", "-l"] ""
    return $ filter (not . Text.null) $ Text.lines $ Text.pack out

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
    void $ forkFinally gitDeploy finishDeploy
  where
    gitDeploy = threadDelay (5 * us)

    finishDeploy (Left ex) = do
        now <- getCurrentTime
        transitionState var $ \_ ->
            return (Idle, [DeployFailed tag now (Text.pack (show ex))])

    finishDeploy (Right ()) = do
        now <- getCurrentTime
        let ev = DeployFinished tag now
        transitionState var $ \_ -> return (Idle, [ev])








