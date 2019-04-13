{-# LANGUAGE RecordWildCards #-}

module Lodjur.Core.Util where

import           Control.Exception
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader
import           Data.Pool
import           Database.Beam.Postgres
import qualified Network.URI                   as URI
import           Text.Read                      ( readMaybe )

import           Lodjur.Core.Types             as Core
import qualified Lodjur.Database               as DB
import qualified Lodjur.GitHub                 as GH

getEnv :: CoreM Env
getEnv = ask

database :: Pg a -> CoreM a
database = withConn . flip DB.beam

withConn :: (Connection -> IO a) -> CoreM a
withConn a = do
  Env {..} <- getEnv
  liftIO $ withResource envDbPool a
  -- TODO catch database errors?

githubE
  :: GH.ParseResponse mt a
  => GH.GenRequest mt rw a
  -> CoreM (Either GH.Error a)
githubE req = do
  Env {..} <- getEnv
  liftIO $ do
    tok <- GH.ensureToken envGithubInstallationAccessToken
    GH.executeRequestWithMgr envHttpManager (GH.OAuth tok) req

github :: GH.ParseResponse mt a => GH.GenRequest mt rw a -> CoreM a
github req = do
  rep <- githubE req
  case rep of
    Left  e -> liftIO $ throwIO $ GithubError e
    Right a -> return a

github_ :: GH.ParseResponse mt a => GH.GenRequest mt rw a -> CoreM ()
github_ = void . github

parseConnectURI :: String -> Maybe Core.ConnectInfo
parseConnectURI = fromURI <=< URI.parseURI

fromURI :: URI.URI -> Maybe Core.ConnectInfo
fromURI uri = do
  guard $ URI.uriIsAbsolute uri
  connectSecure <- case URI.uriScheme uri of
    "ws:"  -> return False
    "wss:" -> return True
    _      -> mzero
  guard $ URI.uriQuery uri == ""
  guard $ URI.uriFragment uri == ""
  auth <- URI.uriAuthority uri
  guard $ URI.uriUserInfo auth == ""
  let connectHost = URI.uriRegName auth
  connectPort <- readMaybe (dropWhile (== ':') $ URI.uriPort auth)
  let connectPath = URI.uriPath uri
  return Core.ConnectInfo { .. }
