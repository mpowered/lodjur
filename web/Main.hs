{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Pool
import           Data.Semigroup                 ( (<>) )
import qualified Database.PostgreSQL.Simple    as Pg
import qualified Network.HTTP.Client           as Http
import qualified Network.HTTP.Client.TLS       as Http
import           Options.Applicative

import           Config
import qualified Lodjur.Core                   as Core
import qualified Lodjur.GitHub                 as GH
import qualified Lodjur.Manager                as Work
import           Web
import           Base

import           GitHub.Data.Id

newtype LodjurOptions = LodjurOptions
  { configFile          :: FilePath
  }

lodjur :: Parser LodjurOptions
lodjur = LodjurOptions <$> strOption
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
    , githubInstallationId = envGithubInstallationId
    , .. } <- readConfiguration configFile

  envHttpManager  <- Http.newManager Http.tlsManagerSettings
  envWorkManager  <- Work.newManager
  envDbPool       <- createPool (Pg.connect databaseConnectInfo)
                                 Pg.close
                                 1 60 16
  envGithubInstallationAccessToken <- GH.installationToken
    envHttpManager
    (Id envGithubAppId)
    githubAppSigner
    (Id envGithubInstallationId)

  envCore <- Core.startCore envGithubInstallationAccessToken envHttpManager envDbPool

  let env = Env { .. }

  runServer httpPort env githubOauth
