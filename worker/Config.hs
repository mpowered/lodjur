{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Config where

import Dhall

import Data.Text (Text)

data Config = Config
  { cfgLogFile    :: Maybe Text
  , cfgWebSocket  :: Text
  , cfgGit        :: GitConfig
  , cfgGithub     :: GithubConfig
  , cfgBuild      :: BuildConfig
  } deriving (Generic, Interpret)

data GitConfig = GitConfig
  { gitCommand  :: Text
  , gitCache    :: Text
  , gitWorkRoot :: Text
  , gitDebug    :: Bool
  } deriving (Generic, Interpret)

data GithubConfig = GithubConfig
  { githubAppId           :: Natural
  , githubAppPrivateKey   :: Text
  , githubInstId          :: Natural
  } deriving (Generic, Interpret)

data BuildConfig = BuildConfig
  { buildCommand  :: Text
  , buildDebug    :: Bool
  } deriving (Generic, Interpret)

readConfig :: FilePath -> IO Config
readConfig = inputFile auto