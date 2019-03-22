{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Config where

import           Data.Aeson                   as JSON
import qualified Data.ByteString.Char8        as Char8
import           Data.Text.IO                 as Text
import qualified Database.Redis               as Redis
import           Text.Toml

import qualified Build
import qualified Git

data Config = Config
  { workDir             :: FilePath
  , redisConnectInfo    :: Redis.ConnectInfo
  , gitCfg              :: Git.Config
  , buildCfg            :: Build.Config
  }

instance FromJSON Config where
  parseJSON = withObject "Configuration" $ \o -> do
    workDir             <- o .: "work-dir"
    redisConnectInfo    <- o .: "redis" >>= parseRedisConnectInfo
    gitCfg              <- o .: "git"
    buildCfg            <- o .: "nix-build"
    return Config{..}

    where
      parseRedisConnectInfo o = do
        connectHost     <- o .: "host"
        connectPort     <- Redis.PortNumber . fromInteger <$> o .: "port"
        connectAuth     <- fmap Char8.pack <$> o .:? "auth"
        connectDatabase <- o .: "database"
        return Redis.ConnInfo
          { connectMaxConnections = 50
          , connectMaxIdleTime = 30
          , connectTimeout = Nothing
          , connectTLSParams = Nothing
          , ..}

readConfiguration :: FilePath -> IO Config
readConfiguration path = do
  f <- Text.readFile path
  case parseTomlDoc path f of
    Right toml -> case fromJSON (toJSON toml) of
      JSON.Success config -> pure config
      JSON.Error   e      -> fail e
    Left e -> fail (show e)
