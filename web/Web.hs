{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Web where

import           Prelude                 hiding ( head )

import           Data.ByteString                ( ByteString )
import           Data.Int                       ( Int32 )
import           Data.String.Conversions
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Lucid
import           Servant
import           Servant.HTML.Lucid

import           Job
import           Types

type Web
    = GetNoContent '[HTML] (Html ())
 :<|> "jobs" :> Get '[HTML] (Html ())
 :<|> "job" :> Capture "jobid" Int32 :> Get '[HTML] (Html ())

web :: ServerT Web AppM
web
    = home
 :<|> getJobs
 :<|> getJob

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
home = redirects "jobs"

getJobs :: AppM (Html ())
getJobs = do
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
          div_ [ class_ "user"] $ span_ [ class_ "far fa-chevron-down" ] ""
          div_ [ class_ "user"] $ span_ [ class_ "far fa-user" ] ""
          div_ [ class_ "user"] "Shaun"
        div_ [ class_ "head-box header" ] $ do
          div_ [ class_ "head"] "Recent Jobs"
        div_ [ class_ "content" ] $ do
          div_ [ class_ "job-list card-list" ] ""
        div_ [ class_ "footer" ] ""

getJob :: Int32 -> AppM (Html ())
getJob jobid = do
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
              div_ [ class_ "user"] $ span_ [ class_ "far fa-chevron-down" ] ""
              div_ [ class_ "user"] $ span_ [ class_ "far fa-user" ] ""
              div_ [ class_ "user"] "Shaun"
            div_ [ class_ "head-box header" ] $ do
              div_ [ class_ "head"] $
                toHtml ("Job " <> viaShow job'Id)
            div_ [ class_ "content" ] $ do
              div_ [ class_ "job-detail", data_ "job-id" (cs $ show job'Id) ] ""
              div_ [ class_ "job-log", data_ "job-id" (cs $ show job'Id) ] ""
            div_ [ class_ "footer" ] ""