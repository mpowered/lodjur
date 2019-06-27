{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Web where

import           Prelude                 hiding ( head )

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString                ( ByteString )
import           Data.Int                       ( Int32 )
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time
import           GitHub                        as GH
import           GitHub.Endpoints.Users        as GH
import           GHC.TypeLits
import           Lodjur.Database               as Db hiding ( div_ )
import           Lucid
import           Servant
import           Servant.Auth.Server            ( throwAll )
import           Servant.HTML.Lucid

import           Auth
import           Job
import           Types

type Web
    = GetNoContent '[HTML] (Html ())
 :<|> Unprotected
 :<|> CookieAuth :> Protected

web :: ServerT Web AppM
web
    = home
 :<|> unprotected
 :<|> protected

type Unprotected
    = QueryParam "redirect" Text :> "login" :> Get '[HTML] (Html ())
 :<|> "auth" :> QueryParam "code" Text :> QueryParam "state" Text :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] (Html ()))

type Protected
    = "jobs" :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] (Html ()))
 :<|> "job" :> Capture "jobid" Int32 :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] (Html ()))

unprotected :: ServerT Unprotected AppM
unprotected
    = login
 :<|> auth

protected :: AuthResult -> ServerT Protected AppM
protected (Authenticated authuser)
    = headerAll ""
 (    getJobs authuser
 :<|> getJob authuser
 )
-- protected (AuthExpired authuser)
--     = protected (Authenticated authuser)
-- protected _ = throwAll $ err302 { errHeaders = [("Location", "/login")] }

class HeaderAll h v orig new
    | h v orig -> new
    , new -> h
    , new -> v
    , new -> orig
  where
    headerAll :: v -> orig -> new

instance (HeaderAll h v a c, HeaderAll h v b d) => HeaderAll h v (a :<|> b) (c :<|> d) where
  headerAll v (a :<|> b) = headerAll v a :<|> headerAll v b

instance (HeaderAll h v b c) => HeaderAll h v (a -> b) (a -> c) where
  headerAll v a = headerAll v <$> a

instance (HeaderAll h v (m a) (n b)) => HeaderAll h v (ReaderT r m a) (ReaderT r n b) where
  headerAll v a = mapReaderT (headerAll v) a

instance (KnownSymbol h, ToHttpApiData v, AddHeader h v a b) => HeaderAll h v (Handler a) (Handler b) where
  headerAll v a = addHeader v <$> a

deferredScript :: Text -> Html ()
deferredScript src =
  script_ [src_ src, defer_ "defer"] ("" :: Text)

static :: Text -> Text
static = ("/static" <>)

staticRef :: Text -> Attribute
staticRef = href_ . static

head :: Text -> Html ()
head title = 
  head_ $ do
    title_ (toHtml title)
    meta_ [charset_ "UTF-8"]
    favicon
    fonts
    scripts
    stylesheets

favicon :: Html ()
favicon = do
  link_ [rel_ "apple-touch-icon", sizes_ "57x57", staticRef "/icon/apple-icon-57x57.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "60x60", staticRef "/icon/apple-icon-60x60.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "72x72", staticRef "/icon/apple-icon-72x72.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "76x76", staticRef "/icon/apple-icon-76x76.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "114x114", staticRef "/icon/apple-icon-114x114.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "120x120", staticRef "/icon/apple-icon-120x120.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "144x144", staticRef "/icon/apple-icon-144x144.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "152x152", staticRef "/icon/apple-icon-152x152.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "180x180", staticRef "/icon/apple-icon-180x180.png"]
  link_ [rel_ "icon", type_ "image/png", sizes_ "192x192",  staticRef "/icon/android-icon-192x192.png"]
  link_ [rel_ "icon", type_ "image/png", sizes_ "96x96", staticRef "/icon/favicon-96x96.png"]
  link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", staticRef "/icon/favicon-32x32.png"]
  link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", staticRef "/icon/favicon-16x16.png"]
  link_ [rel_ "manifest", staticRef "/icon/manifest.json"]
  meta_ [name_ "msapplication-TileColor", content_ "#ffffff"]
  meta_ [name_ "msapplication-TileImage", content_ "/icon/ms-icon-144x144.png"]
  meta_ [name_ "theme-color", content_ "#ffffff"]

fonts :: Html ()
fonts = do
  link_ [rel_ "stylesheet", staticRef "/css/fa.css"]
  link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css?family=Roboto&display=swap"]
  link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css?family=Roboto+Condensed&display=swap"]
  link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css?family=Source+Code+Pro&display=swap"]

stylesheets :: Html ()
stylesheets =
  link_ [rel_ "stylesheet", staticRef "/css/lodjur.css"]

scripts :: Html ()
scripts = do
  deferredScript (static "/js/jquery-3.4.1.min.js")
  deferredScript (static "/js/underscore-min.js")
  deferredScript (static "/js/moment.js")
  deferredScript "/js/api.js"
  deferredScript (static "/js/lodjur.js")

redirects :: ByteString -> AppM a
redirects url = throwError err302 { errHeaders = [("Location", cs url)] }

viaShow :: Show a => a -> Text
viaShow = Text.pack . show

home :: AppM (Html ())
home = redirects "/jobs"

login :: Maybe Text -> AppM (Html ())
login redir = do
  clientId <- getEnv envGithubClientId
  state <- redirToken (fromMaybe "/" redir)
  let endpoint = "https://github.com/login/oauth/authorize?client_id=" <> cs clientId <> "&state=" <> state
  return $ doctypehtml_ $ html_ $ do
    head "Lodjur"
    body_ $ do
      div_ $ do
        a_ [ href_ endpoint ] "Login with GitHub"

auth :: Maybe Text -> Maybe Text -> AppM (Headers '[Header "Set-Cookie" Text] (Html ()))
auth mcode mstate = do
    code  <- maybe (throwError err404) return mcode
    state <- maybe (throwError err404) return mstate
    token <- getAccessToken code
    muser <- liftIO $ userInfoCurrent' (OAuth token)
    user' <- either (const $ throwError err404) return muser
    signer <- getEnv envCookieSigner
    redir <- liftIO $ validateRedirToken signer state
    loggedIn user' token (url redir)
  where
    url (Valid u) = u
    url _         = "/"

loggedIn :: GH.User -> Token -> Text -> AppM (Headers '[Header "Set-Cookie" Text] (Html ()))
loggedIn user' token redir = do
  now <- liftIO getCurrentTime
  us <- runDb $ upsertUser user' token now
  case us of
    [dbuser] -> do
      cookie <- authenticateUser $
        AuthUser (Db.userId dbuser)
                 (fromMaybe (Db.userLogin dbuser) (Db.userName dbuser))
                 (Db.userAvatarUrl dbuser)
      throwError err302 { errHeaders = [("Location", cs redir), cookie] }
    _ -> error "User upsert failed."

user :: AuthUser -> Html ()
user u = do
  case authUserAvatar u of
    Just url -> img_ [ class_ "user", src_ url ]
    Nothing -> div_ [ class_ "user"] $ span_ [ class_ "far fa-user" ] ""
  div_ [ class_ "user" ] (toHtml $ authUserName u)

getJobs :: AuthUser -> AppM (Html ())
getJobs authuser = do
  return $ doctypehtml_ $ html_ $ do
    head "Lodjur"
    body_ $ do
      div_ [ class_ "app basic" ] $ do
        div_ [ class_ "title-box header" ] $ do
          div_ [ class_ "title" ] $ do
            b_ "Lodjur"
            "\160"
            "3.0"
        div_ [ class_ "user-box header" ] $ do
          user authuser
          -- div_ [ class_ "user"] $ span_ [ class_ "far fa-chevron-down" ] ""
          -- div_ [ class_ "user"] $ span_ [ class_ "far fa-user" ] ""
          -- div_ [ class_ "user"] "Shaun"
        div_ [ class_ "head-box header" ] $ do
          div_ [ class_ "head"] "Recent Jobs"
        div_ [ class_ "content" ] $ do
          div_ [ class_ "job-list card-list" ] ""
        div_ [ class_ "footer" ] ""

getJob :: AuthUser -> Int32 -> AppM (Html ())
getJob authuser jobid = do
  job <- runDb $ lookupJob jobid
  case job of
    Nothing -> throwError err404
    Just Job'{..} -> do
      return $ doctypehtml_ $ html_ $ do
        head "Lodjur"
        body_ $ do
          div_ [ class_ "app basic" ] $ do
            div_ [ class_ "title-box header" ] $ do
              div_ [ class_ "title" ] $ do
                b_ "Lodjur"
                "\160"
                "3.0"
            div_ [ class_ "user-box header" ] $ do
              user authuser
              -- div_ [ class_ "user"] $ span_ [ class_ "far fa-chevron-down" ] ""
              -- div_ [ class_ "user"] $ span_ [ class_ "far fa-user" ] ""
              -- div_ [ class_ "user"] "Shaun"
            div_ [ class_ "head-box header" ] $ do
              div_ [ class_ "head"] $
                toHtml ("Job " <> viaShow job'Id)
            div_ [ class_ "content" ] $ do
              div_ [ class_ "job-detail", data_ "job-id" (cs $ show job'Id) ] ""
              -- div_ [ class_ "job-log", data_ "job-id" (cs $ show job'Id) ] ""
              div_ [ class_ "job-rspec", data_ "job-id" (cs $ show job'Id) ] ""
            div_ [ class_ "footer" ] ""