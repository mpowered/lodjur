{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Web
  ( runServer
  )
where

import qualified Data.List                     as List
import           Data.Text                     (Text)
import           Data.Time.Clock               (UTCTime)
import           Data.Time.Format              (defaultTimeLocale, formatTime)
import           Lucid.Base                    (Html, toHtml)
import           Lucid.Bootstrap
import           Lucid.Html5
import qualified GitHub.Extra                  as GH
import           Network.OAuth.OAuth2
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.Wai.Handler.WebSockets
import           Network.Wai.Middleware.Static
import           Network.WebSockets      hiding ( runServer )
import           Web.Spock               hiding ( static )
import           Web.Spock.Config
import           Web.Spock.Lucid

import           Auth.GitHub
import           Base
import           WebHook

import           Lodjur.Database         hiding ( div_ )
import           Lodjur.Manager

import           Paths_lodjur

runServer :: Int -> Env -> OAuth2 -> IO ()
runServer port env githubOauth = do
  cfg    <- defaultSpockCfg emptySession (PCPool $ envDbPool env) env
  sbase  <- getDataFileName "static"
  lodjur <- spockAsApp $ spock cfg $ app sbase
  wsmgr  <- newManager
  putStrLn $ "Serving on port " ++ show port
  putStrLn $ "static from " ++ show sbase
  Warp.run port $ websocketsOr opts (managerWebServerApp wsmgr) lodjur
 where
  opts = defaultConnectionOptions
  staticPrefix = "static/"
  app staticBase = do
    -- Middleware
    middleware $ staticPolicy $ policy (List.stripPrefix staticPrefix) >-> addBase staticBase

    -- Webhooks

    -- This is protected by a shared secret that is expected in the
    -- X-HUB-SIGNATURE header
    post "/github/webhook" webhookAction

    -- Auth
    authRoutes githubOauth

    -- Routes
    -- get "/" (ifLoggedIn homeAction welcomeAction)
    get "/" homeAction

    -- requireLoggedIn $ do
    --   get "/jobs" getDeploymentJobsAction
    --   post "/jobs" newDeployAction
    --   get ("jobs" <//> var)               showJobAction
    --   get ("jobs" <//> var <//> "output") streamOutputAction
    --   get ("jobs" <//> var <//> "result" <//> var) getResultAction

ifLoggedIn :: Action () -> Action () -> Action ()
ifLoggedIn thenRoute elseRoute = readSession >>= \case
  Session { currentUser = Just _ } -> thenRoute
  _ -> elseRoute

welcomeAction :: Action ()
welcomeAction = redirect "/github/login"

homeAction :: Action ()
homeAction = do
  suites <- recentCheckSuites 50
  renderLayout "CheckSuites"
    $ BarePage
    $ renderCheckSuites suites
 where
  recentCheckSuites :: Integer -> Action [CheckSuite]
  recentCheckSuites n =
    runQuery $ \conn ->
      beam conn
        $ runSelectReturningList
        $ select
          $ limit_ n
          $ orderBy_ (desc_ . checksuiteStartedAt)
          $ all_ (dbCheckSuites db)

data Layout
  = WithNavigation [Html ()] (Html ())
  | BarePage (Html ())

deferredScript :: Text -> Html ()
deferredScript src =
  script_ [src_ src, defer_ "defer"] ("" :: Text)

renderHtml :: Html () -> Action a
renderHtml = lucid

renderLayout :: Html () -> Layout -> Action a
renderLayout title layout =
  -- sess <- readSession
  renderHtml $ doctypehtml_ $ html_ $ do
    head_ $ do
      title_ title
      link_ [rel_ "stylesheet", href_ "/static/bootstrap/css/bootstrap.min.css"]
      -- link_ [rel_ "stylesheet", href_ (static "lodjur.css")]
      deferredScript "/static/jquery-3.0.0.slim.min.js"
      deferredScript "/static/bootstrap/js/bootstrap.bundle.min.js"
      -- deferredScript "job.js"
      -- deferredScript "dashboard.js"
    case layout of
      WithNavigation _breadcrumbs contents ->
        body_ $
          -- nav_ [class_ "navbar navbar-expand navbar-dark bg-dark"] $ div_ [class_ "container"] $ do
          --   a_ [class_ "navbar-brand", href_ "/"] "Lodjur"
          --   toNavBarLinks [("/jobs", "Jobs")]
          --   currentUserNav sess
          -- nav_ [class_ "breadcrumb-nav"] $ div_ [class_ "container"] $ ol_
          --   [class_ "breadcrumb"]
          --   (toBreadcrumbItems (homeLink : breadcrumbs))
          container_ contents
          -- div_ [class_ "container text-center footer text-muted"] $
          --   span_ [] ("Lodjur " <> toHtml (showVersion version))
      BarePage contents ->
        body_ [class_ "bare-page"] $ container_ contents
 -- where
 --  toBreadcrumbItems :: [Html ()] -> Html ()
 --  toBreadcrumbItems []       = return ()
 --  toBreadcrumbItems elements = do
 --    foldMap (li_ [class_ "breadcrumb-item"]) (init elements)
 --    li_ [class_ "breadcrumb-item active"] (last elements)

 --  homeLink :: Html ()
 --  homeLink = a_ [href_ "/"] "Home"

 --  toNavBarLinks :: [(Text, Html ())] -> Html ()
 --  toNavBarLinks links =
 --    ul_ [class_ "navbar-nav mr-auto"] $ forM_ links $ \(href, name) ->
 --      li_ [class_ "nav-item"] $ a_ [href_ href, class_ "nav-link"] name

renderCheckSuites :: [CheckSuite] -> Html ()
renderCheckSuites []   = p_ [class_ "text-secondary"] "No jobs available."
renderCheckSuites ss = div_ [class_ "card"] $ do
  div_ [class_ "card-header"] "Latest Check Suites"
  table_ [class_ "table mb-0"] $ do
    tr_ $ do
      th_ "Id"
      th_ "Started At"
      th_ "Revision"
      th_ "Status"
    mapM_ renderCheckSuite ss
 where
  renderCheckSuite :: CheckSuite -> Html ()
  renderCheckSuite s = tr_ $ do
    td_ (toHtml $ show $ checksuiteId s)
    td_ (maybe mempty renderUTCTime $ checksuiteStartedAt s)
    td_ (toHtml $ checksuiteHeadSha s)
    td_ $
      case unDbEnum (checksuiteStatus s) of
        GH.Queued     -> td_ [class_ "text-muted"] "Queued"
        GH.InProgress -> td_ [class_ "text-primary"] "In Progress"
        GH.Completed  ->
          case unDbEnum <$> checksuiteConclusion s of
            Just GH.Success   -> td_ [class_ "text-success"] "Success"
            Just GH.Failure   -> td_ [class_ "text-danger"]  "Failure"
            Just GH.Cancelled -> td_ [class_ "text-warning"] "Cancelled"
            _                 -> td_ [class_ "text-warning"] "Complete"

formatUTCTime :: UTCTime -> String
formatUTCTime = formatTime defaultTimeLocale "%c"

renderUTCTime :: UTCTime -> Html ()
renderUTCTime = toHtml . formatUTCTime
