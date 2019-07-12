{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Env where

import           Config
import           Control.Concurrent.QSem
import           Control.Monad.Except
import           Data.String.Conversions        ( cs )
import           GitHub.Data.Id                 ( Id(..) )
import qualified Lodjur.Core                    as Core
import qualified Lodjur.Core.Util               as Core
import           Lodjur.Logging
import qualified Lodjur.GitHub                  as GH
import           Network.HTTP.Client            ( Manager )
import qualified Web.JWT                        as JWT

data Env = Env
  { envLogTarget                      :: LogTarget
  , envWebSocket                      :: Core.ConnectInfo
  , envGit                            :: GitEnv
  , envGithub                         :: GithubEnv
  , envBuild                          :: BuildEnv
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

data GithubEnv = GithubEnv
  { envGithubInstallationAccessToken  :: GH.GitHubToken
  }

buildEnv :: (MonadIO m, MonadError String m) => Manager -> Config -> m Env
buildEnv httpManager cfg =
  Env <$> pure (maybe LogStdout LogFile (cs <$> cfgLogFile cfg))
      <*> maybe (throwError "Invalid WebSocket URI") return (Core.parseConnectURI (cs $ cfgWebSocket cfg))
      <*> buildGitEnv (cfgGit cfg)
      <*> buildGithubEnv httpManager (cfgGithub cfg)
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

buildGithubEnv :: (MonadIO m, MonadError String m) => Manager -> GithubConfig -> m GithubEnv
buildGithubEnv httpManager cfg = do
  signer      <- maybe (throwError "Invalid RSA secret.") (return . JWT.RSAPrivateKey) (JWT.readRsaSecret $ cs $ githubAppPrivateKey cfg)
  accessToken <- liftIO $ GH.installationToken
                  httpManager
                  (ghid $ githubAppId cfg)
                  signer
                  (ghid $ githubInstId cfg)
  return $ GithubEnv accessToken
  where
    ci :: (Integral a, Num b) => a -> b
    ci = fromInteger . toInteger

    ghid = Id . ci
    