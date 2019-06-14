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
  , httpCookieSecret      :: Text
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
  , githubClientId        :: Text
  , githubClientSecret    :: Text
  } deriving (Generic, Interpret)

readConfig :: FilePath -> IO Config
readConfig = inputFile auto