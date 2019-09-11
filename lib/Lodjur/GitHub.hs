{-# LANGUAGE DeriveGeneric #-}

module Lodjur.GitHub
  ( GitHubCommit(..)
  , GitHubToken(..)
  , staticToken
  , installationToken
  , getToken
  , ensureToken
  , module GH
  )
where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           Data.Time.Clock
import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           GHC.Generics
import           Network.HTTP.Client            ( Manager )
import qualified Web.JWT                       as JWT

import           GitHub                        as GH
import           GitHub.Data.Name              as GH
import           GitHub.Extra                  as GH
import           GitHub.Endpoints.Apps         as GH
import           GitHub.Endpoints.Checks       as GH
import           GitHub.Endpoints.CheckSuite   as GH
import qualified Lodjur.Internal.JSON          as JSON

data GitHubCommit
  = GitHubCommit
  { ghcSha            :: !Text
  , ghcOwner          :: !Text
  , ghcRepo           :: !Text
  , ghcBranch         :: !(Maybe Text)
  , ghcMessage        :: !(Maybe Text)
  , ghcAuthor         :: !(Maybe Text)
  , ghcAuthorEmail    :: !(Maybe Text)
  , ghcCommitter      :: !(Maybe Text)
  , ghcCommitterEmail :: !(Maybe Text)
  , ghcTimestamp      :: !(Maybe UTCTime)
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON GitHubCommit where
  toEncoding = genericToEncoding JSON.options

instance FromJSON GitHubCommit where
  parseJSON = genericParseJSON JSON.options

data GitHubToken
  = StaticToken Token
  | RenewableToken (IO (Maybe Token))

staticToken :: Token -> GitHubToken
staticToken = StaticToken

installationToken
  :: Manager -> Id App -> JWT.Signer -> Id Installation -> IO GitHubToken
installationToken mgr appid signer instid = do
  tok <- newMVar Nothing
  return
    $ RenewableToken (getInstallationAccessToken mgr appid signer instid tok)

getToken :: GitHubToken -> IO (Maybe Token)
getToken (StaticToken    tok) = return (Just tok)
getToken (RenewableToken f  ) = f

ensureToken :: GitHubToken -> IO Token
ensureToken auth = do
  tok <- getToken auth
  case tok of
    Just t  -> return t
    Nothing -> do
      threadDelay (10 * 1000000)
      ensureToken auth

idToText :: Id a -> Text
idToText = Text.pack . show . untagId

createInstallationJWT :: Id App -> JWT.Signer -> NominalDiffTime -> Token
createInstallationJWT appid signer now = Text.encodeUtf8 jwt
 where
  claims = mempty { JWT.iss = JWT.stringOrURI (idToText appid)
                  , JWT.iat = JWT.numericDate now
                  , JWT.exp = JWT.numericDate (now + 600)
                  }
  jwt = JWT.encodeSigned signer mempty claims

newInstallationAccessToken
  :: Manager
  -> Id App
  -> JWT.Signer
  -> Id Installation
  -> IO (Maybe AccessToken)
newInstallationAccessToken mgr appid signer instid = do
  now <- getPOSIXTime
  let tok = createInstallationJWT appid signer now
  r <- executeRequestWithMgr mgr (Bearer tok) $ createInstallationTokenR instid
  return $ either (const Nothing) Just r

getInstallationAccessToken
  :: Manager
  -> Id App
  -> JWT.Signer
  -> Id Installation
  -> MVar (Maybe AccessToken)
  -> IO (Maybe Token)
getInstallationAccessToken mgr appid signer instid installationAccessToken = do
  now <- getCurrentTime
  tok <- takeMVar installationAccessToken
  at  <- maybe renew (return . Just) (validAccessToken now tok)
  putMVar installationAccessToken at
  return (accessToken <$> at)
 where
  validAccessToken now tok = do
    at <- tok
    e  <- accessTokenExpiresAt at
    guard (now < e)
    return at

  renew = newInstallationAccessToken mgr appid signer instid
