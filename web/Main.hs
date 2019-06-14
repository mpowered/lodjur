{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Main where

import           Control.Exception
import           Data.String.Conversions        ( cs )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Database.PostgreSQL.Simple    as Pg
import           GitHub.Data.Id                 ( Id(..) )
import qualified Language.JavaScript.Parser    as JS
import qualified Language.JavaScript.Process.Minify as JS
import qualified Network.HTTP.Client           as Http
import qualified Network.HTTP.Client.TLS       as Http
import           Network.Wai
import           Network.Wai.Middleware.Gzip
import qualified Network.Wai.Handler.Warp      as Warp
import           Options.Applicative
import           Servant
import           Servant.API.WebSocket
import           Servant.Server.Experimental.Auth
import qualified Web.JWT                       as JWT

import           Lodjur.Core
import           Lodjur.Database
import qualified Lodjur.GitHub                 as GH
import           Lodjur.GitHub.Webhook

import           Api
import           Auth
import           Config
import           ContentTypes
import           Stream
import           Types
import           Web
import           WebHook
import           WebSocket

import           Paths_lodjur

type App
    = "github-event" :> Webhook
 :<|> "js" :> "api.js" :> Get '[Javascript] Text
 :<|> "static" :> Raw
 :<|> "websocket" :> WebSocketPending
 :<|> Api
 :<|> StreamApi
 :<|> Web

app :: FilePath -> ServerT App AppM
app staticDir
      = webhook
  :<|> return apijs
  :<|> serveDirectoryFileServer staticDir
  :<|> websocket
  :<|> api
  :<|> streamapi
  :<|> web

apijs :: Text
apijs = mconcat [ apiAsJS, streamapiAsJS ]

apijsMin :: Text
apijsMin = minify apijs
 where
  minify = LT.toStrict . JS.renderToText . JS.minifyJS . JS.readJs . T.unpack

newtype LodjurOptions = LodjurOptions
  { configFile :: FilePath
  }

lodjurOpts :: Parser LodjurOptions
lodjurOpts = LodjurOptions <$> strOption
  (  long "config-file"
  <> metavar "PATH"
  <> short 'c'
  <> value "lodjur.dhall"
  <> help "Path to Lodjur configuration file"
  )

main :: IO ()
main = lodjur =<< execParser opts
 where
  opts = info
    (lodjurOpts <**> helper)
    (fullDesc <> progDesc "Lodjur" <> header "Lodjur CI and Deployment Server")

lodjur :: LodjurOptions -> IO ()
lodjur LodjurOptions {..} = do
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

  bracket (startCore accessToken httpManager dbPool) cancelCore $ \core -> do
    let key = gitHubKey (pure (cs githubWebhookSecret))
        env = Types.Env { envGithubAppId = ci githubAppId
                        , envGithubClientId = githubClientId
                        , envGithubClientSecret = githubClientSecret
                        , envCore = core
                        , envDbPool = dbPool
                        }

    let ctx = key :. authHandler :. EmptyContext
        api' = Proxy :: Proxy App

    putStrLn $ "Serving on port " ++ show httpPort ++ ", static from " ++ show staticDir

    Warp.run (ci httpPort) $ gzip def { gzipFiles = GzipCompress } $
      serveWithContext api' ctx $
        hoistServerWithContext api' (Proxy :: Proxy '[GitHubKey, AuthHandler Request Session]) (runApp env) $
          app staticDir

 where
  pgConnectInfo DbConfig {..} = Pg.ConnectInfo
    { connectHost     = cs dbHost
    , connectPort     = ci dbPort
    , connectDatabase = cs dbName
    , connectUser     = cs dbUser
    , connectPassword = cs dbPassword
    }

  ci :: (Integral a, Num b) => a -> b
  ci = fromInteger . toInteger

  ghid = Id . ci

  parsePrivateKey key =
    maybe (fail "Invalid RSA secret.") (return . JWT.RSAPrivateKey)
      $ JWT.readRsaSecret key
