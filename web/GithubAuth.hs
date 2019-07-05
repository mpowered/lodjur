{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module GithubAuth where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Crypto.JOSE                   as Jose
import qualified Crypto.JWT                    as Jose
import qualified Data.ByteString               as BS
import           Data.String.Conversions
import           Network.HTTP.Types             ( hCookie )
import           Network.Wai                   as Wai
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.Internal.AddSetCookie
import           Servant.Auth.Server.Internal.Class
import           Servant.Server.Internal (DelayedIO, addAuthCheck, withRequest)
import           Web.Cookie

data GHAuth (auths :: [*]) val

instance ( n ~ 'S 'Z
         , HasServer (AddSetCookiesApi n api) ctxs, AreAuths auths ctxs v
         , HasServer api ctxs -- this constraint is needed to implement hoistServer
         , AddSetCookies n (ServerT api Handler) (ServerT (AddSetCookiesApi n api) Handler)
         , ToJWT v
         , HasContextEntry ctxs (GHSettings v)
         ) => HasServer (GHAuth auths v :> api) ctxs where
  type ServerT (GHAuth auths v :> api) m = AuthResult v -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route _ context subserver =
    route (Proxy :: Proxy (AddSetCookiesApi n api))
          context
          (fmap go subserver `addAuthCheck` authCheck)

    where
      authCheck :: DelayedIO (AuthResult v, SetCookieList ('S 'Z))
      authCheck = withRequest $ \req -> liftIO $ do
        authResult <- runAuthCheck (runAuths (Proxy :: Proxy auths) context) req
        cookie <- mkCookie authResult
        return (authResult, cookie `SetCookieCons` SetCookieNil)

      mkCookie (Authenticated usr) = makeGHCookie ghSettings usr
      mkCookie _                   = return Nothing

      ghSettings :: GHSettings v
      ghSettings = getContextEntry context

      go :: ( old ~ ServerT api Handler
            , new ~ ServerT (AddSetCookiesApi n api) Handler
            )
         => (AuthResult v -> ServerT api Handler)
         -> (AuthResult v, SetCookieList n) -> new
      go fn (authResult, cookies) = addSetCookies cookies $ fn authResult

data GH usr

instance FromJWT usr => IsAuth (GH usr) usr where
  type AuthArgs (GH usr) = '[GHSettings usr]
  runAuth _ _ = githubAuthCheck

data GHSettings usr = GHSettings
  { cookieName            :: !BS.ByteString
  , jwtSettings           :: !JWTSettings
  , jwtValidationSettings :: !Jose.JWTValidationSettings
  , validateUser          :: usr -> IO (Maybe usr)
  }

makeGHSettings :: BS.ByteString -> JWTSettings -> (usr -> IO (Maybe usr)) -> GHSettings usr
makeGHSettings name jwts vu =
  GHSettings { cookieName = name
              , jwtSettings = jwts
              , jwtValidationSettings = Jose.defaultJWTValidationSettings ((== Matches) . audienceMatches jwts)
              , validateUser = vu
              }

githubAuthCheck :: FromJWT usr => GHSettings usr -> AuthCheck usr
githubAuthCheck ghs = do
  req   <- ask
  token <- maybe mzero return $ do
    cookies <- lookup hCookie $ requestHeaders req
    lookup (cookieName ghs) (parseCookies cookies)
  jwt <- runExceptT $ do
    unverifiedJWT <- Jose.decodeCompact $ cs token
    Jose.verifyClaims (jwtValidationSettings ghs)
                      (validationKeys $ jwtSettings ghs)
                      unverifiedJWT
  case jwt of
    Left  (_ :: Jose.JWTError) -> mzero
    Right claims               -> case decodeJWT claims of
      Left  _   -> mzero
      Right usr -> maybe mzero return =<< liftIO (validateUser ghs usr)

makeGHCookie :: ToJWT usr => GHSettings usr -> usr -> IO (Maybe SetCookie)
makeGHCookie ghSettings usr = do
  ejwt   <- makeJWT usr (jwtSettings ghSettings) Nothing
  case ejwt of
    Left  _   -> return Nothing
    Right jwt -> return $ Just defaultSetCookie
      { setCookieName     = cookieName ghSettings
      , setCookieValue    = cs jwt
      , setCookiePath     = Just "/"
      , setCookieSameSite = Just sameSiteLax
      }

acceptGHLogin
  :: (ToJWT usr, AddHeader "Set-Cookie" SetCookie response withCookie)
  => GHSettings usr
  -> usr
  -> IO (Maybe (response -> withCookie))
acceptGHLogin ghSettings usr = do
  ejwt   <- makeJWT usr (jwtSettings ghSettings) Nothing
  cookie <- case ejwt of
    Left  _   -> return Nothing
    Right jwt -> return $ Just defaultSetCookie
      { setCookieName     = cookieName ghSettings
      , setCookieValue    = cs jwt
      , setCookiePath     = Just "/"
      , setCookieSameSite = Just sameSiteLax
      }
  return (addHeader <$> cookie)
