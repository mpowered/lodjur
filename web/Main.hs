{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Pool
import           Data.Semigroup                 ( (<>) )
import           Data.String.Conversions
import qualified Database.PostgreSQL.Simple    as Pg
import qualified Network.HTTP.Client           as Http
import qualified Network.HTTP.Client.TLS       as Http
import           Options.Applicative
import qualified Web.JWT                       as JWT

import           Config
import           Lodjur.Core
import qualified Lodjur.GitHub                 as GH
import           Web

import           GitHub.Data.Id

import           Paths_lodjur

newtype LodjurOptions = LodjurOptions
  { configFile :: FilePath
  }

lodjur :: Parser LodjurOptions
lodjur = LodjurOptions <$> strOption
  (  long "config-file"
  <> metavar "PATH"
  <> short 'c'
  <> value "lodjur.dhall"
  <> help "Path to Lodjur configuration file"
  )

main :: IO ()
main = app =<< execParser opts
 where
  opts = info
    (lodjur <**> helper)
    (fullDesc <> progDesc "Lodjur" <> header "Lodjur CI and Deployment Server")

app :: LodjurOptions -> IO ()
app LodjurOptions {..} = do
  Config {..} <- readConfig configFile
  let HttpConfig {..}   = cfgHttp
      GithubConfig {..} = cfgGithub

  staticDir   <- maybe (getDataFileName "static") (return . cs) httpStaticDir

  httpManager <- Http.newManager Http.tlsManagerSettings

  dbPool <- createPool (Pg.connect (pgConnectInfo cfgDatabase)) Pg.close 1 60 32

  signer      <- parsePrivateKey (cs githubAppPrivateKey)
  accessToken <- GH.installationToken httpManager
                                      (ghid githubAppId)
                                      signer
                                      (ghid githubInstId)

  core <- startCore accessToken httpManager dbPool

  runServer (int httpPort)
            staticDir
            (int githubAppId)
            (cs githubWebhookSecret)
            dbPool
            core

 where
  pgConnectInfo DbConfig {..} = Pg.ConnectInfo
    { connectHost     = cs dbHost
    , connectPort     = int dbPort
    , connectDatabase = cs dbName
    , connectUser     = cs dbUser
    , connectPassword = cs dbPassword
    }

  int :: (Integral a, Num b) => a -> b
  int = fromInteger . toInteger

  ghid = Id . int

  parsePrivateKey key =
    maybe (fail "Invalid RSA secret.") (return . JWT.RSAPrivateKey)
      $ JWT.readRsaSecret key
