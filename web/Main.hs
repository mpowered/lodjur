{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Data.Pool
import           Data.Semigroup               ((<>))
import qualified Database.PostgreSQL.Simple   as Pg
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Options.Applicative

import           Config
import qualified Lodjur.GitHub                as GH
import           Web
import           Base

import           GitHub.Data.Id

newtype LodjurOptions = LodjurOptions
  { configFile          :: FilePath
  }

lodjur :: Parser LodjurOptions
lodjur = LodjurOptions
  <$> strOption
    (  long "config-file"
    <> metavar "PATH"
    <> short 'c'
    <> value "lodjur.toml"
    <> help "Path to Lodjur configuration file"
    )

main :: IO ()
main = start =<< execParser opts
 where
  opts = info
    (lodjur <**> helper)
    (fullDesc <> progDesc "Lodjur" <> header
      "Mpowered's Nixops Deployment Frontend"
    )

start :: LodjurOptions -> IO ()
start LodjurOptions {..} = do
  Config
    { githubRepos = envGithubRepos
    , githubSecretToken = envGithubSecretToken
    , githubAppId = envGithubAppId
    , githubAppSigner = envGithubAppSigner
    , githubInstallationId = envGithubInstallationId
    , ..
    } <- readConfiguration configFile

  envManager <- newManager tlsManagerSettings
  dbpool <- createPool (Pg.connect databaseConnectInfo) Pg.close 1 60 16
  envGithubInstallationAccessToken <- newMVar Nothing

  let env = Env{..}

  githubToken <-
    GH.installationToken
      envManager
      (Id envGithubAppId)
      envGithubAppSigner
      (Id envGithubInstallationId)

  runServer httpPort env dbpool githubOauth
