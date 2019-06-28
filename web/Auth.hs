{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}

module Auth where

import           Prelude                 hiding ( exp )

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString                ( ByteString )
import           Data.Int                       ( Int64 )
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                      ( Text )
import qualified Data.Text.Read                as T
import           Data.Time                      ( NominalDiffTime, UTCTime )
import           Data.Time.Clock.POSIX
import qualified Database.Beam.Postgres.Full   as Pg
import           GitHub                        as GH
import           GitHub.Endpoints.Users        as GH
import           GHC.Generics                   ( Generic )
import           Lodjur.Database               as Db
import           Network.HTTP.Req               ( (/:), (=:) )
import qualified Network.HTTP.Req              as Req
import           Network.HTTP.Types             ( HeaderName )
import           Network.Wai                   as Wai
import           Servant
import           Servant.Auth.Server
import           Servant.Server.Experimental.Auth
import           Web.Cookie
import           Web.JWT

import           Types

data AuthUser = AuthUser
    { authUserId      :: Int64
    , authUserName    :: Text
    , authUserAvatar  :: Maybe Text
    }
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

{-

authenticateUser :: AuthUser -> AppM (HeaderName, ByteString)
authenticateUser authuser = do
  tok <- userToken authuser
  return ("Set-Cookie", "lodjur-auth="<> cs tok <> "; SameSite=Strict")

data Validated a = Valid a | Expired a | Invalid
  deriving (Show, Functor, Foldable, Traversable)

validateToken :: Signer -> Text -> IO (Validated (JWT VerifiedJWT))
validateToken signer tok = do
    now <- getPOSIXTime
    return $ maybe Invalid (checkClaims now) (decodeAndVerifySignature signer tok)
  where
    checkClaims now jwt =
      let c = claims jwt
          validAud = "lodjur" `elem` map stringOrURIToText (auds c)
          validIss = stringOrURI "lodjur" == iss c
          validNbf = maybe False ((now >=) . secondsSinceEpoch) (nbf c)
          validExp = maybe False ((now <=) . secondsSinceEpoch) (exp c)
      in
        case (validAud && validIss && validNbf, validExp) of
          (True, True)  -> Valid jwt
          (True, False) -> Expired jwt
          (False, _)    -> Invalid

validateUserToken :: Signer -> Text -> IO (Validated AuthUser)
validateUserToken signer tok = do
  jwt <- validateToken signer tok
  return $ fromMaybe Invalid (mapM parseAuthUser jwt)

validateRedirToken :: Signer -> Text -> IO (Validated Text)
validateRedirToken signer tok = do
  jwt <- validateToken signer tok
  return $ fromMaybe Invalid (mapM parseRedirect jwt)

parseAuthUser :: JWT r -> Maybe AuthUser
parseAuthUser jwt =
  let c = claims jwt
      uc = unClaimsMap (unregisteredClaims c)
  in AuthUser <$> (parseInt =<< fmap stringOrURIToText (sub c))
              <*> (parseValue =<< Map.lookup "username" uc)
              <*> (parseValue =<< Map.lookup "avatar" uc)

parseRedirect :: FromJSON b => JWT r -> Maybe b
parseRedirect jwt =
  let c = claims jwt
      uc = unClaimsMap (unregisteredClaims c)
  in parseValue =<< Map.lookup "redirect" uc

parseInt :: Integral a => Text -> Maybe a
parseInt txt =
  case T.decimal txt of
    Right (i, "") -> Just i
    _             -> Nothing

parseValue :: FromJSON a => Value -> Maybe a
parseValue val =
  case fromJSON val of
    Success a -> Just a
    Error _ -> Nothing

lodjurToken :: NominalDiffTime -> JWTClaimsSet -> AppM Text
lodjurToken expiry cls = do
  now <- liftIO getPOSIXTime
  signer <- getEnv envCookieSigner
  return $ encodeSigned signer cls { aud = Left <$> stringOrURI "lodjur"
                                   , iss = stringOrURI "lodjur"
                                   , iat = numericDate now
                                   , exp = numericDate (now + expiry)
                                   , nbf = numericDate now
                                   }

userToken :: AuthUser -> AppM Text
userToken (AuthUser uid name avatar) =
  lodjurToken (24*60*60) $
    mempty { sub = stringOrURI (cs $ show uid)
           , unregisteredClaims = ClaimsMap $ Map.fromList [("username", toJSON name), ("avatar", toJSON avatar)]
           }

redirToken :: Text -> AppM Text
redirToken url =
  lodjurToken (15*60) $
    mempty { unregisteredClaims = ClaimsMap $ Map.fromList [("redirect", toJSON url)]
           }

validateGithubToken :: DbPool -> AuthUser -> Handler ()
validateGithubToken dbpool authuser = do
  user  <- maybe (throwError err401) return =<<
            runDb' dbpool (lookupUser (authUserId authuser))
  token <- maybe (throwError err401) (return . cs)
            (Db.userAccessToken user)
  ghUsr <- either (const $ throwError err401) return =<<
            liftIO (userInfoCurrent' (OAuth token))
  now <- liftIO getCurrentTime
  _ <- runDb' dbpool (upsertUser ghUsr token now)
  return ()
-}

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