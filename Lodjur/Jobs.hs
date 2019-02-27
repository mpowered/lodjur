{-# LANGUAGE RecordWildCards #-}

module Lodjur.Jobs where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad.Reader

import qualified Lodjur.Build as Build
import qualified Lodjur.Git as Git

type Job g a = ReaderT (g -> QSem) IO a

job :: g -> IO a -> Job g a
job grp action = do
  grpSem <- ask
  liftIO $ withQSem (grpSem grp) action

withQSem :: QSem -> IO a -> IO a
withQSem s = bracket_ (waitQSem s) (signalQSem s)

runJob :: (g -> QSem) -> Job g a -> IO a
runJob = flip runReaderT

asyncJob :: (g -> QSem) -> Job g a -> IO (Async a)
asyncJob f = async . runJob f

data JobGrp = GitGrp | BuildGrp

-- Build

build :: Git.Env -> Build.Env -> Git.Repo -> String -> Job JobGrp ()
build gitEnv buildEnv repo sha = do
  workdir <- job BuildGrp $ Git.checkout gitEnv repo sha
  job BuildGrp $ Build.build buildEnv workdir "release.nix" "mpowered-services" [("railsEnv", "production")]
