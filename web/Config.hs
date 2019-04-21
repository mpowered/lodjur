{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Config
  ( Config(..)
  , HttpConfig(..)
  , DbConfig(..)
  , GithubConfig(..)
  , readConfig
  )
where

import Dhall

import Data.Text (Text)

data Config = Config
  { cfgHttp               :: HttpConfig
  , cfgGithub             :: GithubConfig
  , cfgLogDir             :: Text
  , cfgDatabase           :: DbConfig
  } deriving (Generic, Interpret)

data HttpConfig = HttpConfig
  { httpPort              :: Natural
  , httpStaticDir         :: Maybe Text
  } deriving (Generic, Interpret)

data DbConfig = DbConfig
  { dbHost                :: Text
  , dbPort                :: Natural
  , dbName                :: Text
  , dbUser                :: Text
  , dbPassword            :: Text
  } deriving (Generic, Interpret)

data GithubConfig = GithubConfig
  { githubWebhookSecret   :: Text
  , githubAppId           :: Natural
  , githubAppPrivateKey   :: Text
  , githubInstId          :: Natural
  } deriving (Generic, Interpret)

readConfig :: FilePath -> IO Config
readConfig = inputFile auto

-- instance FromJSON Config where
--   parseJSON = withObject "Configuration" $ \o -> do
--     workDir <- o .: "work-dir"
--     httpPort <- o .: "http-port"
--     databaseConnectInfo <- o .: "database" >>= parseDatabaseConnectInfo
--     githubSecretToken <- Char8.pack <$> (o .: "github-secret-token")
--     githubRepos <- o .: "github-repos"
--     githubAppId <- o .: "github-app-id"
--     githubAppSigner <- o .: "github-app-private-key" >>= parsePrivateKey
--     githubInstallationId <- o .: "github-installation-id"
--     staticDirectory <- o .: "static-directory"

--     oauthClientId <- o .: "github-oauth-client-id"
--     oauthClientSecret <- o .: "github-oauth-client-secret"
--     oauthCallbackUrlStr <- o .: "github-oauth-callback-url"
--     oauthCallback <- either (fail . show) (pure . pure) (parseURI strictURIParserOptions (Text.encodeUtf8 oauthCallbackUrlStr))
--     let githubOauth = OAuth2
--           { oauthOAuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|]
--           , oauthAccessTokenEndpoint = [uri|https://github.com/login/oauth/access_token|]
--           , ..
--           }
--     return Config{..}
--     where
--       parseDatabaseConnectInfo o = do
--         connectHost     <- o .: "host"
--         connectPort     <- o .: "port"
--         connectDatabase <- o .: "name"
--         connectUser     <- o .: "user"
--         connectPassword <- o .: "password"
--         return Pg.ConnectInfo {..}
--       parsePrivateKey key =
--         maybe (fail "Invalid RSA secret.") (return . JWT.RSAPrivateKey) $
--           JWT.readRsaSecret (Text.encodeUtf8 key)

-- readConfiguration :: FilePath -> IO Config
-- readConfiguration path = do
--   f <- Text.readFile path
--   case parseTomlDoc path f of
--     Right toml -> case fromJSON (toJSON toml) of
--       JSON.Success config -> pure config
--       JSON.Error   e      -> fail e
--     Left e -> fail (show e)
