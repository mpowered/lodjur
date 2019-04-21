{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Web
  ( runServer
  )
where

import           Control.Monad.Reader
import           Data.Text                     (Text)
import           GHC.Generics
import           Lucid
import           Network.OAuth.OAuth2
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant
import           Servant.API.WebSocket
import           Servant.JS
import           Servant.HTML.Lucid

import           Base
import qualified Lodjur.Core                   as Core
import qualified Lodjur.Core.Websocket         as Websocket
import           Lodjur.GitHub.Events
import           Lodjur.GitHub.Webhook

import           Paths_lodjur

type AppM = ReaderT AppCtx Handler

data AppCtx = AppCtx
  { appConfig               :: Config
  , appCore                 :: Core.Core
  }

data Config = Config
  { cfgGithubAppId          :: !Int
  } deriving (Generic, Show)

type App
    = "github-event" :> Webhook
 :<|> "js" :> "api.js" :> Get '[PlainText] Text
 :<|> "static" :> Raw
 :<|> "websocket" :> WebSocketPending
 :<|> Api
 :<|> Web

type Web
    = Get '[HTML] (Html ())
 :<|> "job" :> Capture "jobid" Int :> Get '[HTML] (Html ())

type Api = "api" :>
 (    "start"  :> Post '[JSON] ()
 :<|> "action" :> ReqBody '[JSON] () :> Post '[JSON] ()
 )

type Webhook
    = GitHubCheckEvent '[ 'WebhookCheckSuiteEvent ] :> GitHubSignedReqBody '[JSON] (EventWithHookRepo CheckSuiteEvent) :> Post '[JSON] ()
 :<|> GitHubCheckEvent '[ 'WebhookCheckRunEvent   ] :> GitHubSignedReqBody '[JSON] (EventWithHookRepo CheckRunEvent  ) :> Post '[JSON] ()

app :: Core.Env -> FilePath -> ServerT App AppM
app env static
      = webhook
  :<|> apijs
  :<|> serveDirectoryFileServer static
  :<|> websocket
  :<|> api
  :<|> web

web :: ServerT Web AppM
web
    = home
 :<|> job

home :: AppM (Html ())
home = return $
  return ()

job :: Int -> AppM (Html ())
job _ = return $
  return ()

api :: ServerT Api AppM
api
    = start
 :<|> act

start :: AppM ()
start = return ()

act :: () -> AppM ()
act _ = return ()

apijs :: AppM Text
apijs = return $ jsForAPI (Proxy :: Proxy Api) jquery

websocket :: ServerT WebSocketPending AppM
websocket = server
 where
  server pc = do
    core <- asks appCore
    liftIO $ Websocket.serverApp (Core.coreEnv core) pc

webhook :: ServerT Webhook AppM
webhook = checkSuiteEvent :<|> checkRunEvent

checkSuiteEvent :: RepoWebhookCheckEvent -> ((), EventWithHookRepo CheckSuiteEvent) -> AppM ()
checkSuiteEvent _ _ = return ()

checkRunEvent :: RepoWebhookCheckEvent -> ((), EventWithHookRepo CheckRunEvent) -> AppM ()
checkRunEvent _ _ = return ()

runServer :: Int -> Env -> OAuth2 -> IO ()
runServer port env githubOauth = do
  static <- getDataFileName "static"
  let key = gitHubKey (pure $ envGithubSecretToken env)
  let ctx = AppCtx undefined undefined
  putStrLn $ "Serving on port " ++ show port ++ ", static from " ++ show static
  Warp.run port $
    serveWithContext (Proxy :: Proxy App) (key :. EmptyContext) $
      hoistServerWithContext (Proxy :: Proxy App) (Proxy :: Proxy '[GitHubKey]) (flip runReaderT ctx) $
        app (Core.coreEnv $ envCore env) static

{-}
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
    get ("job" <//> var) showJobAction
    get ("job" <//> var <//> "card") streamJobCardAction
    get ("job" <//> var <//> "logs") streamJobLogsAction
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

showJobAction :: Int32 -> Action ()
showJobAction jobid = do
  job <- runQuery $ lookupJob jobid
  renderLayout "jobs"
    $ BarePage
    $ div_ $ do
      div_ [id_ "job", data_ "job-id" (Text.pack $ show jobid)] $
        maybe (p_ "Job not found") renderJob job
      div_ [id_ "logs", data_ "job-id" (Text.pack $ show jobid)] ""

lookupJob :: Int32 -> DB.Connection -> IO (Maybe Job)
lookupJob jobid conn =
  beam conn
    $ runSelectReturningOne
    $ select
      $ filter_ (\j -> jobId j ==. val_ jobid)
      $ all_ (dbJobs db)

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

recentLogs :: Int32 -> Int64 -> DB.Connection -> IO ([Text], Int64)
recentLogs jobid logid conn = do
  logs <- beam conn
    $ runSelectReturningList
    $ select
      $ orderBy_ (asc_ . logId)
    $ filter_ (\l -> logJob l ==. val_ (JobKey jobid) &&. logId l >. val_ logid)
      $ all_ (dbLogs db)
  let n = maximum (logid : map logId logs)
  return (map logText logs, n)

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
  div_ [class_ "card-body p-0"] $
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
      div_ [class_ "col-1 card-text"] $
        a_ [href_ ("/job/" <> Text.pack (show jobId))] (toHtml $ show jobId)
      div_ [class_ "col-1 card-text"] (toHtml $ fromMaybe "" jobSrcCommitter)
      div_ [class_ "col-3 card-text"] (toHtml $ fromMaybe "" jobSrcMessage)
      case fromJSON jobAction of
        Success (Job.Build False) -> div_ [class_ "col-1 card-text"] "Build"
        Success (Job.Build True)  -> div_ [class_ "col-1 card-text"] "Build and Check"
        Success (Job.Check app)   -> div_ [class_ "col-1 card-text"] (toHtml $ "Check " <> app)
        _                         -> div_ [class_ "col-1 card-text"] ""

-- formatUTCTime :: UTCTime -> String
-- formatUTCTime = formatTime defaultTimeLocale "%c"

-- renderUTCTime :: UTCTime -> Html ()
-- renderUTCTime = toHtml . formatUTCTime

streamJobUpdatesAction :: Action ()
streamJobUpdatesAction = do
  Env{..} <- getState
  chan <- liftIO $ Core.subscribe envCore
  setHeader "Content-Type"      "text/event-stream"
  setHeader "Cache-Control"     "no-cache"
  setHeader "X-Accel-Buffering" "no"
  stream (streamJobUpdates chan (action envDbPool))
 where
  action pool = do
    jobs <- withResource pool $ recentJobs 10
    return $ LT.toStrict $ renderText $ renderJobs jobs

streamJobCardAction :: Int32 -> Action ()
streamJobCardAction jobid = do
  Env{..} <- getState
  chan <- liftIO $ Core.subscribe envCore
  setHeader "Content-Type"      "text/event-stream"
  setHeader "Cache-Control"     "no-cache"
  setHeader "X-Accel-Buffering" "no"
  stream (streamJobUpdates chan (action envDbPool))
 where
  action pool = do
    job <- withResource pool $ lookupJob jobid
    return $ LT.toStrict $ renderText $
      maybe (p_ "Job not found") renderJob job

data JobEvent
  = JobEvent
    { jobeventHtml :: !Text
    }

instance ToJSON JobEvent where
  toJSON JobEvent{..} =
    object [ "html" .= jobeventHtml ]

streamJobUpdates :: TChan Core.Event -> IO Text -> StreamingBody
streamJobUpdates chan action write flush = forever $ do
  event <- atomically $ readTChan chan
  case event of
    Core.JobSubmitted -> go
    Core.JobUpdated   -> go
    _ -> return ()
 where
  go = do
    content <- action

    write $ B.fromByteString "event: update\n"
    let event = JobEvent { jobeventHtml = content }
    write $ B.fromLazyByteString
      ("data: " <> encode event <> "\n")
    write $ B.fromByteString "\n"
    flush

streamJobLogsAction :: Int32 -> Action ()
streamJobLogsAction jobid = do
  Env{..} <- getState
  chan <- liftIO $ Core.subscribe envCore
  setHeader "Content-Type"      "text/event-stream"
  setHeader "Cache-Control"     "no-cache"
  setHeader "X-Accel-Buffering" "no"
  stream (streamJobLogs envDbPool chan jobid)

data LogEvent
  = LogEvent
    { logData :: ![Text]
    }

instance ToJSON LogEvent where
  toJSON LogEvent{..} =
    object [ "data" .= logData ]

streamJobLogs :: Pool DB.Connection -> TChan Core.Event -> Int32 -> StreamingBody
streamJobLogs dbpool chan jobid write flush = go 0
 where
  go n = do
    (logs, n') <- withResource dbpool $ recentLogs jobid n
    unless (null logs) $ do
      write $ B.fromByteString "event: logs\n"
      let event = LogEvent { logData = logs }
      write $ B.fromLazyByteString
        ("data: " <> encode event <> "\n")
      write $ B.fromByteString "\n"
      flush
    next n'
  next n = do
    event <- atomically $ readTChan chan
    case event of
      Core.LogsUpdated jobid' | jobid == jobid' -> go n
      _ -> next n

-}
