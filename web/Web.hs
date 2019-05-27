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

import           Control.Monad.IO.Class         ( liftIO )
import           Data.ByteString                ( ByteString )
import           Data.Int                       ( Int32 )
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time
import           Data.Tree
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
static = ("static/" <>)

staticRef :: Text -> Attribute
staticRef = href_ . static

favicon :: Html ()
favicon = do
  link_ [rel_ "apple-touch-icon", sizes_ "57x57", staticRef "icon/apple-icon-57x57.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "60x60", staticRef "icon/apple-icon-60x60.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "72x72", staticRef "icon/apple-icon-72x72.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "76x76", staticRef "icon/apple-icon-76x76.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "114x114", staticRef "icon/apple-icon-114x114.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "120x120", staticRef "icon/apple-icon-120x120.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "144x144", staticRef "icon/apple-icon-144x144.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "152x152", staticRef "icon/apple-icon-152x152.png"]
  link_ [rel_ "apple-touch-icon", sizes_ "180x180", staticRef "icon/apple-icon-180x180.png"]
  link_ [rel_ "icon", type_ "image/png", sizes_ "192x192",  staticRef "icon/android-icon-192x192.png"]
  link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", staticRef "icon/favicon-32x32.png"]
  link_ [rel_ "icon", type_ "image/png", sizes_ "96x96", staticRef "icon/favicon-96x96.png"]
  link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", staticRef "icon/favicon-16x16.png"]
  link_ [rel_ "manifest", staticRef "icon/manifest.json"]
  meta_ [name_ "msapplication-TileColor", content_ "#ffffff"]
  meta_ [name_ "msapplication-TileImage", content_ "icon/ms-icon-144x144.png"]
  meta_ [name_ "theme-color", content_ "#ffffff"]

fonts :: Html ()
fonts = do
  link_ [rel_ "stylesheet", staticRef "css/fa.css"]
  link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css?family=Roboto"]
  link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css?family=Source+Code+Pro"]

stylesheets :: Html ()
stylesheets =
  link_ [rel_ "stylesheet", staticRef "css/lodjur.css"]

scripts :: Html ()
scripts = do
  deferredScript (static "js/jquery-3.4.1.min.js")
  deferredScript (static "js/underscore-min.js")
  deferredScript "js/api.js"
  deferredScript (static "js/lodjur.js")

leftPanel :: Forest Job' -> Html ()
leftPanel jobs = do
  div_ [ class_ "left-panel" ] $ do
    jobOutline jobs
    actionPanel

appHeader :: Html ()
appHeader = do
  div_ [ class_ "app-header" ] $ do
    b_ "Lodjur"
    "3.0"

jobOutline :: Forest Job' -> Html ()
jobOutline jobs = do
  div_ [ class_ "jobs-outline" ] $
    toHtml (Outline jobs)

actionPanel :: Html ()
actionPanel = do
  div_ [ class_ "actions" ] $ do
    div_ [ class_ "disabled" ] $ do
      span_ [ class_ "far fa-fw fa-cloud-upload" ] ""
      "Deploy to..."
    div_ [ class_ "disabled" ] $ do
      span_ [ class_ "far fa-fw fa-redo" ] ""
      "Rerun"
    div_ $ do
      span_ [ class_ "far fa-fw fa-times" ] ""
      "Cancel"

tabBar :: Text -> [(Text, Bool)] -> Html ()
tabBar username ts = do
  div_ [ class_ "left-panel" ]
    appHeader
  div_ [ class_ "main-panel" ] $ do
    mapM_ tab ts
    user username

user :: Text -> Html ()
user username = do
  div_ [ class_ "user" ] $ do
    div_ [ class_ "tab-text" ] $ do
      span_ [ class_ "far fa-chevron-down" ] ""
      "\160\160"
      span_ [ class_ "far fa-user" ] ""
      "\160\160"
      toHtml username

tab :: (Text, Bool) -> Html ()
tab (name, active) = do
  let c = if active then "active" else "inactive"
  div_ [ classes_ ["tab", c] ] $ do
    div_ [ class_ "tab-text" ] (toHtml name)

mainPanel :: UTCTime -> Maybe Job' -> [LogLine] -> Html ()
mainPanel now j l = do
  div_ [ class_ "main-panel" ] $ do
    jobDetail now j
    jobLog l

jobHead :: Job' -> Html ()
jobHead Job'{..} = do
  div_ [ class_ "job-head" ] $ do
    div_ $ statusIcon job'Status job'Conclusion
    div_ $ toHtml job'Name

jobAttr :: ToHtml a => Text -> a -> Html ()
jobAttr ico val = do
  div_ [ class_ "job-commit-attr" ] $ do
    div_ $ span_ [ class_ ico ] ""
    div_ $ toHtml val

jobDetail :: UTCTime -> Maybe Job' -> Html ()
jobDetail now (Just j@Job'{..}) = do
  div_ [ class_ "job-detail" ] $ do
    jobHead j
    div_ [ class_ "job-commit commit-left" ] $ do
      jobAttr "fab fa-fw fa-github" (job'CommitOwner <> "/" <> job'CommitRepo)
      jobAttr "far fa-fw fa-code-branch" (fromMaybe "" job'CommitBranch)
      jobAttr "far fa-fw fa-code-commit" job'CommitSha
      jobAttr "far fa-fw fa-comment-alt" (fromMaybe "" job'CommitMessage)
    div_ [ class_ "job-commit commit-right" ] $ do
      jobAttr "far fa-fw fa-at" committer
      jobAttr "far fa-fw fa-clock" (maybe "" (prettyTime now) job'StartedAt)
      jobAttr "far fa-fw fa-stopwatch" (maybe "" (prettyDuration . diffUTCTime (fromMaybe now job'CompletedAt)) job'StartedAt)
  where
    committer = Text.unwords $ catMaybes
      [ job'CommitCommitter
      , (\email -> "<" <> email <> ">") <$> job'CommitAuthorEmail
      ]

jobDetail _ Nothing = do
  div_ [ class_ "job-detail" ] ""

jobLog :: [LogLine] -> Html ()
jobLog l = do
  div_ [ class_ "job-info" ] $ do
    div_ [ class_ "job-log" ] $ do
      toHtml $ Text.unlines (map log'Text l)

redirects :: ByteString -> AppM a
redirects url = throwError err302 { errHeaders = [("Location", cs url)] }

home :: AppM (Html ())
home = redirects "jobs"

{-
getJobs :: AppM (Html ())
getJobs = do
  now <- liftIO getCurrentTime
  return $ doctypehtml_ $ html_ $ do
    head_ $ do
      title_ "Lodjur"
      meta_ [charset_ "UTF-8"]
      favicon
      fonts
      scripts
      stylesheets
    body_ $ do
      div_ [ class_ "heading" ] $ do
        tabBar "Shaun" [("Output", True), ("Test Results", False)]
      div_ [ class_ "app" ] $ do
        leftPanel []
        mainPanel now Nothing [] -- j l
-}

getJobs :: AppM (Html ())
getJobs = do
  now <- liftIO getCurrentTime
  jobs <- runDb (recentJobsForest 20)
  return $ doctypehtml_ $ html_ $ do
    head_ $ do
      title_ "Lodjur"
      meta_ [charset_ "UTF-8"]
      favicon
      fonts
      scripts
      stylesheets
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
          div_ [ class_ "card-list" ] $ do
            mapM_ (toHtml . Card now) jobs

getJob :: Int32 -> AppM (Html ())
getJob _jobid = undefined

viaShow :: Show a => a -> Text
viaShow = Text.pack . show
