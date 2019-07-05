{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Auth where

import           Prelude                 hiding ( exp )

import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Int                       ( Int64 )
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.Time.Clock.POSIX
import qualified Database.Beam.Postgres.Full   as Pg
import           GitHub                        as GH
import           GitHub.Endpoints.Users        as GH
import           Lodjur.Database               as Db
import           Network.HTTP.Req               ( (/:), (=:) )
import qualified Network.HTTP.Req              as Req
import           Servant.Auth.Server

import           Types

runDb' :: MonadIO m => DbPool -> Pg a -> m a
runDb' dbpool a = liftIO $ withConnection dbpool $ \conn -> beam conn a

lookupUser :: Int64 -> Pg (Maybe Db.User)
lookupUser userid =
  runSelectReturningOne $ select $ do
    filter_ (\u -> Db.userId u ==. val_ userid) $ all_ (dbUsers db)

upsertUser :: GH.User -> Token -> UTCTime -> Pg [Db.User]
upsertUser GH.User{..} token now =
  Pg.runPgInsertReturningList $
    Pg.insertReturning (dbUsers db)
      ( insertExpressions
        [ Db.User
            { userId            = val_ (fromIntegral $ untagId userId)
            , userLogin         = val_ (untagName userLogin)
            , userName          = val_ userName
            , userEmail         = val_ userEmail
            , userCompany       = val_ userCompany
            , userLocation      = val_ userLocation
            , userAvatarUrl     = val_ (Just $ getUrl userAvatarUrl)
            , userAccessToken   = val_ (Just $ cs token)
            , userCreatedAt     = val_ now
            , userUpdatedAt     = val_ now
            , userLastLogin     = val_ now
            }
        ]
      )
      ( Pg.onConflict (Pg.conflictingFields Db.userId) $
        Pg.onConflictUpdateInstead (\u -> (( Db.userLogin u
                                           , Db.userName u
                                           , Db.userEmail u
                                           )
                                          ,( Db.userCompany u
                                           , Db.userLocation u
                                           , Db.userAvatarUrl u
                                           , Db.userAccessToken u
                                           , Db.userUpdatedAt u
                                           , Db.userLastLogin u
                                          ))
                                   )
      )
      ( Just id )

data AccessToken = AccessToken
  { accessToken :: !Text
  , tokenType   :: !Text
  } deriving (Show, Eq, Ord)

instance FromJSON AccessToken where
  parseJSON = withObject "AccessToken" $ \o -> do
    accessToken <- o .: "access_token"
    tokenType   <- o .: "token_type"
    return AccessToken { .. }

getAccessToken :: Text -> AppM Token
getAccessToken code = do
  clientId <- getEnv envGithubClientId
  clientSecret <- getEnv envGithubClientSecret

  r <- Req.runReq def $
    Req.req
      Req.POST
      (Req.https "github.com" /: "login" /: "oauth" /: "access_token")
      (Req.ReqBodyUrlEnc
        (  "client_id" =: clientId
        <> "client_secret" =: clientSecret
        <> "code" =: code
        )
      )
      Req.jsonResponse
      (Req.header "accept" "application/json")
  return (cs $ accessToken $ Req.responseBody r)

userAuthenticated :: DbPool -> GH.User -> Token -> IO (Maybe AuthUser)
userAuthenticated dbpool user token = do
  now <- getCurrentTime
  us <- runDb' dbpool $ upsertUser user token now
  case us of
    [dbuser] -> do
      let authuser = AuthUser (Db.userId dbuser)
                              (fromMaybe (Db.userLogin dbuser) (Db.userName dbuser))
                              (Db.userAvatarUrl dbuser)
      return (Just authuser)
    _ ->
      return Nothing

checkAuthUserAccessToken :: DbPool -> AuthUser -> IO (Maybe AuthUser)
checkAuthUserAccessToken dbpool authuser = runMaybeT $ do
  user' <- MaybeT $ runDb' dbpool $ lookupUser (authUserId authuser)
  token <- maybe mzero return (cs <$> Db.userAccessToken user')
  user  <- exceptToMaybeT $ ExceptT $ userInfoCurrent' (OAuth token)
  MaybeT $ userAuthenticated dbpool user token