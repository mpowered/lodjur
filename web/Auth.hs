{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}

module Auth where

import           Prelude                 hiding ( exp )

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Int                       ( Int64 )
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

newtype AuthUser = AuthUser { authUserId :: Int64 }
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
    unless ("lodjur" `elem` map stringOrURIToText (auds c)) $ throwError err401
    unless (stringOrURI "lodjur" == iss c) $ throwError err401
    unless
        (  maybe False ((now <=) . secondsSinceEpoch) (exp c)
        && maybe False ((now >=) . secondsSinceEpoch) (nbf c)
        )
      $ throwError err401
    case maybe (Left "No subject") (T.decimal . stringOrURIToText) (sub c) of
      Right (userid, "") -> return (AuthUser userid)
      _ -> throwError err401

authenticateUser :: AuthUser -> AppM (a -> Headers '[Header "Set-Cookie" Text] a)
authenticateUser (AuthUser userid) = do
  now <- liftIO getPOSIXTime
  signer <- getEnv envCookieSigner
  let cls = mempty { aud = Left <$> stringOrURI "lodjur"
                   , iss = stringOrURI "lodjur"
                   , sub = stringOrURI (cs $ show userid)
                   , iat = numericDate now
                   , exp = numericDate (now + 24*60*60)
                   , nbf = numericDate now
                   }
      jwt = encodeSigned signer cls
  return $ addHeader ("lodjur-auth=" <> jwt)