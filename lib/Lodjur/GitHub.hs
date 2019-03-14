
module Lodjur.GitHub where

import           Control.Concurrent
import           Control.Monad
import           Data.Time.Clock
import           Data.Time.Clock.POSIX         (getPOSIXTime)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Network.HTTP.Client           (Manager)
import qualified Web.JWT                       as JWT

import           GitHub
import           GitHub.Data.Apps
import           GitHub.Data.Installations
import           GitHub.Endpoints.Apps         hiding (Repo(..))

data GitHubToken
  = StaticToken Token
  | RenewableToken (IO (Maybe Token))

staticToken :: Token -> GitHubToken
staticToken = StaticToken

installationToken :: Manager -> Id App -> JWT.Signer -> Id Installation -> IO GitHubToken
installationToken mgr appid signer instid = do
  tok <- newMVar Nothing
  return $ RenewableToken (getInstallationAccessToken mgr appid signer instid tok)

getToken :: GitHubToken -> IO (Maybe Token)
getToken (StaticToken tok) = return (Just tok)
getToken (RenewableToken f) = f

idToText :: Id a -> Text
idToText = Text.pack . show . untagId

createInstallationJWT :: Id App -> JWT.Signer -> NominalDiffTime -> Token
createInstallationJWT appid signer now =
  Text.encodeUtf8 jwt
  where
    claims = mempty { JWT.iss = JWT.stringOrURI (idToText appid)
                    , JWT.iat = JWT.numericDate now
                    , JWT.exp = JWT.numericDate (now + 600)
                    }
    jwt = JWT.encodeSigned signer claims

newInstallationAccessToken :: Manager -> Id App -> JWT.Signer -> Id Installation -> IO (Maybe AccessToken)
newInstallationAccessToken mgr appid signer instid = do
  now <- getPOSIXTime
  let tok = createInstallationJWT appid signer now
  r <- executeRequestWithMgr mgr (Bearer tok) $ createInstallationTokenR instid
  return $ either (const Nothing) Just r

getInstallationAccessToken :: Manager -> Id App -> JWT.Signer -> Id Installation -> MVar (Maybe AccessToken) -> IO (Maybe Token)
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
      guard $ e < now
      return at

    renew = newInstallationAccessToken mgr appid signer instid
