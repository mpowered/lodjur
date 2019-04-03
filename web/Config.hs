{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Config where

import           Data.Aeson                   as JSON
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as Char8
import           Data.Text                    (Text)
import           Data.Text.IO                 as Text
import           Data.Text.Encoding           as Text
import qualified Database.PostgreSQL.Simple   as Pg
import           Network.OAuth.OAuth2
import           Text.Toml
import           URI.ByteString
import           URI.ByteString.QQ
import qualified Web.JWT                      as JWT

data Config = Config
  { workDir                 :: FilePath
  , httpPort                :: Int
  , databaseConnectInfo     :: Pg.ConnectInfo
  , githubSecretToken       :: ByteString
  , githubRepos             :: [Text]
  , githubOauth             :: OAuth2
  , githubAppId             :: Int
  , githubAppSigner         :: JWT.Signer
  , githubInstallationId    :: Int
  , staticDirectory         :: FilePath
  }

instance FromJSON Config where
  parseJSON = withObject "Configuration" $ \o -> do
    workDir <- o .: "work-dir"
    httpPort <- o .: "http-port"
    databaseConnectInfo <- o .: "database" >>= parseDatabaseConnectInfo
    githubSecretToken <- Char8.pack <$> (o .: "github-secret-token")
    githubRepos <- o .: "github-repos"
    githubAppId <- o .: "github-app-id"
    githubAppSigner <- o .: "github-app-private-key" >>= parsePrivateKey
    githubInstallationId <- o .: "github-installation-id"
    staticDirectory <- o .: "static-directory"

    oauthClientId <- o .: "github-oauth-client-id"
    oauthClientSecret <- o .: "github-oauth-client-secret"
    oauthCallbackUrlStr <- o .: "github-oauth-callback-url"
    oauthCallback <- either (fail . show) (pure . pure) (parseURI strictURIParserOptions (Text.encodeUtf8 oauthCallbackUrlStr))
    let githubOauth = OAuth2
          { oauthOAuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|]
          , oauthAccessTokenEndpoint = [uri|https://github.com/login/oauth/access_token|]
          , ..
          }
    return Config{..}
    where
      parseDatabaseConnectInfo o = do
        connectHost     <- o .: "host"
        connectPort     <- o .: "port"
        connectDatabase <- o .: "name"
        connectUser     <- o .: "user"
        connectPassword <- o .: "password"
        return Pg.ConnectInfo {..}
      parsePrivateKey key =
        maybe (fail "Invalid RSA secret.") (return . JWT.RSAPrivateKey) $
          JWT.readRsaSecret (Text.encodeUtf8 key)

readConfiguration :: FilePath -> IO Config
readConfiguration path = do
  f <- Text.readFile path
  case parseTomlDoc path f of
    Right toml -> case fromJSON (toJSON toml) of
      JSON.Success config -> pure config
      JSON.Error   e      -> fail e
    Left e -> fail (show e)
