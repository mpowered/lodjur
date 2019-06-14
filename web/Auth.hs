{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}

module Auth where

import           Prelude                 hiding ( exp )

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Int                       ( Int64 )
import qualified Data.Map.Strict               as Map
import           Data.String.Conversions
import           Data.Text                      ( Text )
import qualified Data.Text.Read                as T
import           Data.Time.Clock.POSIX
import           Network.Wai
import           Servant
import           Servant.Server.Experimental.Auth
import           Web.Cookie
import           Web.JWT

import           Types

data AuthUser = AuthUser
  { authUserId      :: Int64
  , authUserName    :: Text
  , authUserAvatar  :: Maybe Text
  }
  deriving (Eq, Show, Read)

type CookieAuth = AuthProtect "cookie-auth"

type instance AuthServerData CookieAuth = AuthUser

authHandler :: Signer -> AuthHandler Request AuthUser
authHandler signer = mkAuthHandler handler
 where
  handler req = maybe (throwError err401) checkClaims $ do
    cookie <- lookup "cookie" $ requestHeaders req
    jwt    <- lookup "lodjur-auth" $ parseCookies cookie
    decodeAndVerifySignature signer (cs jwt)

  checkClaims jwt = do
    now <- liftIO getPOSIXTime
    let c = claims jwt
        uc = unClaimsMap $ unregisteredClaims c
    unless ("lodjur" `elem` map stringOrURIToText (auds c)) $ throwError err401
    unless (stringOrURI "lodjur" == iss c) $ throwError err401
    unless
        (  maybe False ((now <=) . secondsSinceEpoch) (exp c)
        && maybe False ((now >=) . secondsSinceEpoch) (nbf c)
        )
      $ throwError err401
    maybe (throwError err401) return $ do
      uid <- parseInt =<< stringOrURIToText <$> sub c
      name <- parseValue =<< Map.lookup "username" uc
      avatar <- parseValue =<< Map.lookup "avatar" uc
      return $ AuthUser uid name avatar

  parseInt txt =
    case T.decimal txt of
      Right (i, "") -> Just i
      _             -> Nothing

  parseValue val =
    case fromJSON val of
      Success a -> Just a
      Error _ -> Nothing

authenticateUser :: AuthUser -> AppM (a -> Headers '[Header "Set-Cookie" Text] a)
authenticateUser (AuthUser uid name avatar) = do
  now <- liftIO getPOSIXTime
  signer <- getEnv envCookieSigner
  let cls = mempty { aud = Left <$> stringOrURI "lodjur"
                   , iss = stringOrURI "lodjur"
                   , sub = stringOrURI (cs $ show uid)
                   , iat = numericDate now
                   , exp = numericDate (now + 24*60*60)
                   , nbf = numericDate now
                   , unregisteredClaims = ClaimsMap $ Map.fromList ([("username", toJSON name), ("avatar", toJSON avatar)] :: [(Text, Value)])
                   }
      jwt = encodeSigned signer cls
  return $ addHeader ("lodjur-auth=" <> jwt)