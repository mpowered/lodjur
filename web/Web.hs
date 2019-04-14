{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Web
  ( runServer
  )
where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Aeson
import qualified Data.Binary.Builder           as B
import qualified Data.List                     as List
import           Data.Maybe
import           Data.Ord                      (Down(..))
import           Data.Pool
import           Data.Text                     (Text)
import qualified Data.Text.Lazy                as LT
import           Data.Time.Clock               (UTCTime)
import           Data.Time.Format              (defaultTimeLocale, formatTime)
import           Data.Tree                     (Tree(..), Forest)
import           Lucid.Base                    (Html, toHtml, renderText)
-- import           Lucid.Bootstrap
import           Lucid.Html5
import           Network.OAuth.OAuth2
import           Network.Wai
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

import qualified Lodjur.Core                   as Core
import           Lodjur.Database               as DB hiding ( div_ )
import           Lodjur.Database.Enum
import qualified Lodjur.Job                    as Job

import           Paths_lodjur

runServer :: Int -> Env -> OAuth2 -> IO ()
runServer port env githubOauth = do
  cfg    <- defaultSpockCfg emptySession (PCPool $ envDbPool env) env
  sbase  <- getDataFileName "static"
  lodjur <- spockAsApp $ spock cfg $ app sbase
  putStrLn $ "Serving on port " ++ show port
  putStrLn $ "static from " ++ show sbase
  Warp.run port $ websocketsOr opts (Core.coreWebSocketApp $ envCore env) lodjur
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
    get "/jobs" streamJobUpdatesAction

    -- requireLoggedIn $ do
    --   get "/jobs" getDeploymentJobsAction
    --   post "/jobs" newDeployAction
    --   get ("jobs" <//> var)               showJobAction
    --   get ("jobs" <//> var <//> "output") streamOutputAction
    --   get ("jobs" <//> var <//> "result" <//> var) getResultAction

-- ifLoggedIn :: Action () -> Action () -> Action ()
-- ifLoggedIn thenRoute elseRoute = readSession >>= \case
--   Session { currentUser = Just _ } -> thenRoute
--   _ -> elseRoute

-- welcomeAction :: Action ()
-- welcomeAction = redirect "/github/login"

homeAction :: Action ()
homeAction = do
  jobs <- runQuery $ recentJobs 10
  renderLayout "jobs"
    $ BarePage
    $ div_ [id_ "jobs"]
    $ renderJobs jobs

recentJobs :: Integer -> DB.Connection -> IO (Forest Job)
recentJobs n conn = do
  roots <- recentRoots n conn
  mapM (jobTree conn) roots

recentRoots :: Integer -> DB.Connection -> IO [Job]
recentRoots n conn =
  beam conn
    $ runSelectReturningList
    $ select
      $ limit_ n
      $ orderBy_ (desc_ . jobId)
      $ filter_ (\j -> jobParent j ==. val_ (JobKey Nothing))
      $ all_ (dbJobs db)

jobTree :: DB.Connection -> Job -> IO (Tree Job)
jobTree conn p = do
  children <- beam conn
    $ runSelectReturningList
    $ select
      $ orderBy_ (asc_ . jobId)
    $ filter_ (\j -> jobParent j ==. val_ (JobKey (Just (jobId p))))
      $ all_ (dbJobs db)
  childForest <- mapM (jobTree conn) children
  return (Node p childForest)

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
      link_ [rel_ "stylesheet", href_ "/static/lodjur.css"]
      deferredScript "/static/jquery-3.0.0.slim.min.js"
      deferredScript "/static/bootstrap/js/bootstrap.bundle.min.js"
      deferredScript "/static/job.js"
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
          div_ [class_ "container-fluid"] contents
          -- div_ [class_ "container text-center footer text-muted"] $
          --   span_ [] ("Lodjur " <> toHtml (showVersion version))
      BarePage contents ->
        body_ [class_ "bare-page"] $ div_ [class_ "container-fluid"] contents
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

renderJobs :: Forest Job -> Html ()
renderJobs jobs =
  div_ [class_ "bg-secondary p-3"] $
    mapM_ renderJobTree (sortDesc jobs)
 where
  sortDesc = List.sortOn (Down . jobId . rootLabel)

renderJobTree :: Tree Job -> Html ()
renderJobTree (Node job children) = do
  let ty = case jobParent job of
            JobKey (Just _) -> "card p-1 my-0"
            JobKey Nothing  -> "card p-2 my-3"
  div_ [class_ ty] $ do
    renderJob job
    div_ [class_ "ml-3"] $
      mapM_ renderJobTree (sortAsc children)
 where
  sortAsc = List.sortOn (jobId . rootLabel)

renderJob :: Job -> Html ()
renderJob Job{..} =
  div_ [class_ "card-body p-0"] $ do
    div_ [class_ "row m-0 p-1"] $ do
      case unDbEnum jobStatus of
        Job.Queued     -> div_ [class_ "col-1 badge badge-secondary"]   "Queued"
        Job.InProgress -> div_ [class_ "col-1 badge badge-primary"] "In Progress"
        Job.Completed  ->
          case unDbEnum <$> jobConclusion of
            Just Job.Success   -> div_ [class_ "col-1 badge badge-success"]   "Success"
            Just Job.Failure   -> div_ [class_ "col-1 badge badge-danger"]    "Failure"
            Just Job.Cancelled -> div_ [class_ "col-1 badge badge-warning"]   "Cancelled"
            Just Job.Neutral   -> div_ [class_ "col-1 badge badge-info"]      "Neutral"
            _                  -> div_ [class_ "col-1 badge badge-warning"]   "Complete"
      div_ [class_ "col-1 card-text"] (toHtml jobName)
      div_ [class_ "col-4 card-text"] (toHtml $ jobSrcOwner <> " / " <> jobSrcRepo <> " / " <> fromMaybe jobSrcSha jobSrcBranch)
      div_ [class_ "col-1 card-text"] (toHtml $ show jobId)
      div_ [class_ "col-1 card-text"] (toHtml $ fromMaybe "" jobSrcCommitter)
      div_ [class_ "col-3 card-text"] (toHtml $ fromMaybe "" jobSrcMessage)
      case fromJSON jobAction of
        Success (Job.Build False) -> div_ [class_ "col-1 card-text"] "Build"
        Success (Job.Build True)  -> div_ [class_ "col-1 card-text"] "Build and Check"
        Success (Job.Check app)   -> div_ [class_ "col-1 card-text"] (toHtml $ "Check " <> app)
        _                         -> div_ [class_ "col-1 card-text"] ""

formatUTCTime :: UTCTime -> String
formatUTCTime = formatTime defaultTimeLocale "%c"

renderUTCTime :: UTCTime -> Html ()
renderUTCTime = toHtml . formatUTCTime

streamJobUpdatesAction :: Action ()
streamJobUpdatesAction = do
  Env{..} <- getState
  chan <- liftIO $ Core.subscribe envCore
  setHeader "Content-Type"      "text/event-stream"
  setHeader "Cache-Control"     "no-cache"
  setHeader "X-Accel-Buffering" "no"
  stream (streamJobUpdates envDbPool chan)

data JobEvent
  = JobEvent
    { jobeventHtml :: !Text
    }

instance ToJSON JobEvent where
  toJSON JobEvent{..} =
    object [ "html" .= jobeventHtml ]

streamJobUpdates :: Pool DB.Connection -> TChan Core.Event -> StreamingBody
streamJobUpdates dbpool chan write flush = forever $ do
  event <- atomically $ readTChan chan
  case event of
    Core.JobSubmitted -> go
    Core.JobUpdated   -> go
    _ -> return ()
 where
  go = do
    jobs <- withResource dbpool $ recentJobs 10
    let content = LT.toStrict $ renderText $ renderJobs jobs

    write $ B.fromByteString "event: update\n"
    let event = JobEvent { jobeventHtml = content }
    write $ B.fromLazyByteString
      ("data: " <> encode event <> "\n")
    write $ B.fromByteString "\n"
    flush
