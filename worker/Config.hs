{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Config where

import           Data.Aeson                    as JSON
import           Data.Text.IO                  as Text
import           Text.Toml

import qualified Git
import qualified Lodjur.Core                   as Core
import qualified Lodjur.Core.Util              as Core

data Config = Config
  { logFile     :: Maybe FilePath
  , workDir     :: FilePath
  , managerCI   :: Core.ConnectInfo
  , gitCfg      :: Git.Config
  , buildCfg    :: BuildConfig
  }

data BuildConfig = BuildConfig
  { buildCmd    :: FilePath
  , buildDebug  :: Bool
  }

instance FromJSON Config where
  parseJSON = withObject "Configuration" $ \o -> do
    logFile   <- o .:?"log-file"
    workDir   <- o .: "work-dir"
    managerCI <- o .: "manager" >>= mgr
    gitCfg    <- o .: "git"
    buildCfg  <- o .: "nix-build"
    return Config { .. }
   where
    mgr s =
      maybe (fail "Not a valid websocket URI") return $ Core.parseConnectURI s

instance FromJSON BuildConfig where
  parseJSON = withObject "Build Config" $ \o -> do
    buildCmd    <- o .: "command"
    buildDebug  <- o .: "debug"
    return BuildConfig{..}

readConfiguration :: FilePath -> IO Config
readConfiguration path = do
  f <- Text.readFile path
  case parseTomlDoc path f of
    Right toml -> case fromJSON (toJSON toml) of
      JSON.Success config -> pure config
      JSON.Error   e      -> fail e
    Left e -> fail (show e)
