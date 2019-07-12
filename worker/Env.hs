{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Env where

import           Config
import           Control.Concurrent.QSem
import           Control.Monad.Except
import           Data.String.Conversions        ( cs )
import qualified Lodjur.Core                    as Core
import qualified Lodjur.Core.Util               as Core
import           Lodjur.Logging

data Env = Env
  { envLogTarget    :: LogTarget
  , envWebSocket    :: Core.ConnectInfo
  , envGit          :: GitEnv
  , envBuild        :: BuildEnv
  }

data BuildEnv = BuildEnv
  { envBuildCommand :: FilePath
  , envBuildDebug   :: Bool
  }

data GitEnv = GitEnv
  { envGitCommand   :: FilePath
  , envGitCache     :: FilePath
  , envGitWorkRoot  :: FilePath
  , envGitDebug     :: Bool
  , envGitSem       :: QSem
  }

buildEnv :: (MonadIO m, MonadError String m) => Config -> m Env
buildEnv cfg =
  Env <$> pure (maybe LogStdout LogFile (cs <$> cfgLogFile cfg))
      <*> maybe (throwError "Invalid WebSocket URI") return (Core.parseConnectURI (cs $ cfgWebSocket cfg))
      <*> buildGitEnv (cfgGit cfg)
      <*> buildBuildEnv (cfgBuild cfg)

buildBuildEnv :: (MonadIO m, MonadError String m) => BuildConfig -> m BuildEnv
buildBuildEnv cfg =
  BuildEnv <$> pure (cs $ buildCommand cfg)
           <*> pure (buildDebug cfg)

buildGitEnv :: (MonadIO m, MonadError String m) => GitConfig -> m GitEnv
buildGitEnv cfg =
  GitEnv <$> pure (cs $ gitCommand cfg)
         <*> pure (cs $ gitCache cfg)
         <*> pure (cs $ gitWorkRoot cfg)
         <*> pure (gitDebug cfg)
         <*> liftIO (newQSem 1)