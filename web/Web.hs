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
import           Servant.Auth.Server           as S
import           Servant.HTML.Lucid

import           Auth
import           Job
import           GithubAuth
import           Types

type Web
    = GetNoContent '[HTML] (Html ())
 :<|> Unprotected
 :<|> GHAuth '[GH AuthUser] AuthUser :> Protected

web :: ServerT Web AppM
web
    = home
 :<|> unprotected
 :<|> protected

type Unprotected
    = QueryParam "redirect" Text :> "login" :> Get '[HTML] (Html ())
 :<|> "auth" :> QueryParam "code" Text :> QueryParam "state" Text :> Get '[HTML] (Headers '[Header "Set-Cookie" SetCookie] (Html ()))

type Protected
    = "jobs" :> Get '[HTML] (Html ())
 :<|> "job" :> Capture "jobid" Int32 :> Get '[HTML] (Html ())

unprotected :: ServerT Unprotected AppM
unprotected
    = login
 :<|> auth

protected :: AuthResult AuthUser -> ServerT Protected AppM
protected (Authenticated authuser)
    = getJobs authuser
 :<|> getJob authuser

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
  let endpoint = "https://github.com/login/oauth/authorize?client_id=" <> cs clientId
  return $ doctypehtml_ $ html_ $ do
    head "Lodjur"
    body_ $ do
      div_ $ do
        a_ [ href_ endpoint ] "Login with GitHub"

auth :: Maybe Text -> Maybe Text -> AppM (Headers '[Header "Set-Cookie" SetCookie] (Html ()))
auth mcode _mstate = do
  code  <- maybe (throwError err404) return mcode
  token <- getAccessToken code
  muser <- liftIO $ userInfoCurrent' (OAuth token)
  user' <- either (const $ throwError err404) return muser
  now <- liftIO getCurrentTime
  us <- runDb $ upsertUser user' token now
  case us of
    [dbuser] -> do
      ghSettings <- getEnv envGHSettings
      let authuser = AuthUser (Db.userId dbuser)
                              (fromMaybe (Db.userLogin dbuser) (Db.userName dbuser))
                              (Db.userAvatarUrl dbuser)
      mcookies <- liftIO $ acceptGHLogin ghSettings authuser
      applyCookies <- maybe (throwError err404) return mcookies
      return $ applyCookies $ doctypehtml_ $ html_ $ do
        head "Lodjur"
        body_ $ do
          div_ $ do
            h1_ "Logged in successfully."
            a_ [ href_ "/" ] "Homepage"
    _ -> error "User upsert failed."

-- loggedIn :: GH.User -> Token -> Text -> AppM (Headers '[Header "Set-Cookie" Text] (Html ()))
-- loggedIn user' token redir = undefined
  -- now <- liftIO getCurrentTime
  -- us <- runDb $ upsertUser user' token now
  -- case us of
  --   [dbuser] -> do
  --     cookie <- authenticateUser $
  --       AuthUser (Db.userId dbuser)
  --                (fromMaybe (Db.userLogin dbuser) (Db.userName dbuser))
  --                (Db.userAvatarUrl dbuser)
  --     throwError err302 { errHeaders = [("Location", cs redir), cookie] }
  --   _ -> error "User upsert failed."

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