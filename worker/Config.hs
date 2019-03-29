{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Config where

import           Data.Aeson                    as JSON
import           Data.Text.IO                  as Text
import           Text.Toml

import qualified Build
import qualified Git
import qualified Lodjur.Manager                as Mgr

data Config = Config
  { workDir     :: FilePath
  , managerCI   :: Mgr.ConnectInfo
  , gitCfg      :: Git.Config
  , buildCfg    :: Build.Config
  }

instance FromJSON Config where
  parseJSON = withObject "Configuration" $ \o -> do
    workDir   <- o .: "work-dir"
    managerCI <- o .: "manager" >>= mgr
    gitCfg    <- o .: "git"
    buildCfg  <- o .: "nix-build"
    return Config { .. }
   where
    mgr s =
      maybe (fail "Not a valid websocket URI") return $ Mgr.parseManagerURI s

readConfiguration :: FilePath -> IO Config
readConfiguration path = do
  f <- Text.readFile path
  case parseTomlDoc path f of
    Right toml -> case fromJSON (toJSON toml) of
      JSON.Success config -> pure config
      JSON.Error   e      -> fail e
    Left e -> fail (show e)
