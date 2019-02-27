{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Lodjur.Web
  ( runServer
  )
where

import           Control.Concurrent
import           Control.Exception             (bracket_)
import           Control.Monad
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader
import           Crypto.Hash
import           Crypto.MAC.HMAC
import           Data.Aeson                    hiding (json)
import           Data.Aeson.Types
import qualified Data.Binary.Builder           as Binary
import qualified Data.ByteString.Base16        as Base16
import qualified Data.ByteString.Char8         as C8
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.List                     as List
import           Data.String
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Time.Clock               (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX
import           Data.Time.Format              (defaultTimeLocale, formatTime)
import           Data.Version
import           GHC.Generics                  (Generic)
import           Lucid.Base                    (Html, makeAttribute, toHtml)
import qualified Lucid.Base                    as Html
import           Lucid.Bootstrap
import           Lucid.Html5
import           Network.HTTP.Types.Status
import           Network.OAuth.OAuth2
import           Network.Wai                   (StreamingBody, pathInfo)
import           Network.Wai.Middleware.Static (Policy, addBase, policy,
                                                staticPolicy, (>->))
import           URI.ByteString                (URIRef (..))
import           Web.Spock                     hiding (static)
import           Web.Spock.Config
import           Web.Spock.Lucid
import qualified Web.JWT                       as JWT

import           Database.Beam
import           Database.Beam.Backend.SQL

import           Lodjur.Auth
import qualified Lodjur.Database               as DB
import qualified Lodjur.Database.CheckRun      as DB
import qualified Lodjur.Database.CheckSuite    as DB
import qualified Lodjur.Database.Event         as DB
import qualified Lodjur.Jobs                   as Jobs
import qualified Lodjur.Git                    as Git
-- import           Lodjur.Deployment
-- import           Lodjur.Deployment.Deployer
-- import           Lodjur.Events.EventLogger
-- import qualified Lodjur.Git                    as Git
-- import           Lodjur.Git.GitAgent
-- import           Lodjur.Output
-- import           Lodjur.Output.OutputLogger
-- import           Lodjur.Output.OutputLoggers
-- import           Lodjur.Output.OutputStreamer
-- import           Lodjur.Process
-- import           Lodjur.User

import           Lodjur.Web.Auth.GitHub
import           Lodjur.Web.Base

import qualified GitHub                       as GH
import qualified GitHub.Data.Id               as GH
import qualified GitHub.Data.Name             as GH
import qualified GitHub.Extra                 as GH
import qualified GitHub.Endpoints.Apps        as GH
import qualified GitHub.Endpoints.Checks      as GH

import           Paths_lodjur

{-
readState :: Action DeployState
readState = getState >>= liftIO . (? GetCurrentState) . envDeployer

renderHtml :: Html () -> Action a
renderHtml = lucid

formatUTCTime :: UTCTime -> String
formatUTCTime = formatTime defaultTimeLocale "%c"

hourMinSec :: UTCTime -> String
hourMinSec = formatTime defaultTimeLocale "%H:%M:%S"

renderDeploymentRevision :: DeploymentJob -> Html ()
renderDeploymentRevision = toHtml . Git.unRevision . deploymentRevision

jobsLink :: Html ()
jobsLink = a_ [href_ "/jobs"] "Jobs"

userIdLink :: UserId -> Html ()
userIdLink userId =
  let uid = unUserId userId
  in a_ [href_ ("https://github.com/" <> uid)] $
      toHtml uid

currentUserNav :: Session -> Html ()
currentUserNav Session { .. } =
  ul_ [class_ "navbar-nav my-2"] $
    case currentUser of
      Just User {..} ->
        li_ [class_ "nav-item dropdown"] $ do
          a_
            [ class_ "nav-link dropdown-toggle"
            , href_ "#"
            , id_ "navbarDropdownMenuLink"
            , role_ "button"
            , data_ "toggle" "dropdown"
            , makeAttribute "aria-haspopup" "true"
            , makeAttribute "aria-expanded" "false"
            ] (toHtml (unUserId userId))
          div_ [class_ "dropdown-menu", makeAttribute "aria-labelledby" "navbarDropdownMenuLink"] $ do
            a_ [href_ "/auth/github/logout", class_ "dropdown-item"] "Log Out"
      Nothing ->
        li_ [class_ "nav-item"] $
          a_ [href_ "/auth/github/login", class_ "nav-link"] "Log In"

deferredScript :: Text -> Html ()
deferredScript src =
  Html.termRawWith
    "script"
    [src_ (static src), Html.makeAttribute "defer" "defer"]
    mempty

data Layout
  = WithNavigation [Html ()] (Html ())
  | BarePage (Html ())

renderLayout :: Html () -> Layout -> Action a
renderLayout title layout = do
  sess <- readSession
  renderHtml $ doctypehtml_ $ html_ $ do
    head_ $ do
      title_ title
      link_
        [rel_ "stylesheet", href_ (static "bootstrap/css/bootstrap.min.css")]
      link_ [rel_ "stylesheet", href_ (static "lodjur.css")]
      deferredScript "jquery-3.0.0.slim.min.js"
      deferredScript "bootstrap/js/bootstrap.bundle.min.js"
      deferredScript "job.js"
      deferredScript "dashboard.js"
    case layout of
      WithNavigation breadcrumbs contents ->
        body_ $ do
          nav_ [class_ "navbar navbar-expand navbar-dark bg-dark"] $ div_ [class_ "container"] $ do
            a_ [class_ "navbar-brand", href_ "/"] "Lodjur"
            toNavBarLinks [("/jobs", "Jobs")]
            currentUserNav sess
          nav_ [class_ "breadcrumb-nav"] $ div_ [class_ "container"] $ ol_
            [class_ "breadcrumb"]
            (toBreadcrumbItems (homeLink : breadcrumbs))
          container_ contents
          div_ [class_ "container text-center footer text-muted"] $
            span_ [] ("Lodjur " <> toHtml (showVersion version))
      BarePage contents ->
        body_ [class_ "bare-page"] $ container_ contents
 where
  toBreadcrumbItems :: [Html ()] -> Html ()
  toBreadcrumbItems []       = return ()
  toBreadcrumbItems elements = do
    foldMap (li_ [class_ "breadcrumb-item"]) (init elements)
    li_ [class_ "breadcrumb-item active"] (last elements)

  homeLink = a_ [href_ "/"] "Home"

  toNavBarLinks :: [(Text, Html ())] -> Html ()
  toNavBarLinks links =
    ul_ [class_ "navbar-nav mr-auto"] $ forM_ links $ \(href, name) ->
      li_ [class_ "nav-item"] $ a_ [href_ href, class_ "nav-link"] name

renderEventLog :: EventLog -> Html ()
renderEventLog []       = p_ [class_ "text-secondary"] "No events available."
renderEventLog eventLog = table_ [class_ "table"] $ do
  tr_ $ do
    th_ "Event"
    th_ "Time"
    th_ "Description"
  mapM_ renderEvent eventLog
 where
  renderEvent :: JobEvent -> Html ()
  renderEvent event = tr_ $ case event of
    JobRunning startedAt -> do
      td_ $ span_ [class_ "text-primary"] "Started"
      td_ (toHtml (formatUTCTime startedAt))
      td_ ""
    JobFinished JobSuccessful finishedAt -> do
      td_ $ span_ [class_ "text-success"] "Finished"
      td_ (toHtml (formatUTCTime finishedAt))
      td_ ""
    JobFinished (JobFailed e) finishedAt -> do
      td_ $ span_ [class_ "text-danger"] "Failed"
      td_ (toHtml (formatUTCTime finishedAt))
      td_ [style_ "color: red;"] (toHtml e)

renderDeployJobs :: DeploymentJobs -> Html ()
renderDeployJobs []   = p_ [class_ "text-secondary"] "No jobs available."
renderDeployJobs jobs = div_ [class_ "card"] $ do
  div_ [class_ "card-header"] "Latest Jobs"
  table_ [class_ "table mb-0"] $ do
    tr_ $ do
      th_ "Job"
      th_ "Revision"
      th_ "Created At"
      th_ "Created By"
      th_ "Result"
    mapM_ renderJob jobs
 where
  renderJob :: (DeploymentJob, Maybe JobResult) -> Html ()
  renderJob (job, r) = tr_ $ do
    td_ (jobLink job)
    td_ (renderDeploymentRevision job)
    td_ (toHtml (formatUTCTime (deploymentTime job)))
    td_ (userIdLink (deploymentJobStartedBy job))
    td_ $ do
      case r of
        Just JobSuccessful -> td_ [class_ "text-success"] "Successful"
        Just (JobFailed _) -> td_ [class_ "text-danger"] "Failed"
        Nothing            -> td_ [class_ "text-primary"] "Running"

renderCurrentState :: DeployState -> Html ()
renderCurrentState state = div_ [class_ "card"] $ do
  div_ [class_ "card-header"] "Current State"
  div_ [class_ "card-body text-center"] $ case state of
    Idle          -> span_ [class_ "text-muted h3"] "Idle"
    Deploying job -> do
      div_ [class_ "text-warning h3"] "Deploying"
      a_ [href_ (jobHref job), class_ "text-warning"] $ do
        renderDeploymentRevision job
        " to "
        toHtml (unDeploymentName (deploymentJobName job))

successfulJobsByDeploymentName
  :: [DeploymentName] -> DeploymentJobs -> [(DeploymentName, DeploymentJob)]
successfulJobsByDeploymentName deploymentNames jobs = foldMap
  (\name -> (name, ) . fst <$> take 1 (List.filter (successfulJobIn name) jobs))
  deploymentNames
 where
  successfulJobIn n = \case
    (job, Just JobSuccessful) -> deploymentJobName job == n
    _                         -> False

-- TODO: Convert this to a database query.
renderLatestSuccessful :: [Deployment] -> DeploymentJobs -> Html ()
renderLatestSuccessful deployments jobs = div_ [class_ "card"] $ do
  div_ [class_ "card-header text-success"] "Latest Successful"
  case successfulJobsByDeploymentName (map deploymentName deployments) jobs of
    [] -> div_ [class_ "card-body text-muted"] "No successful jobs yet."
    successfulJobs ->
      table_ [class_ "table table-bordered mb-0"]
        $ forM_ successfulJobs
        $ \(name, job) -> tr_ $ do
            td_ (toHtml (unDeploymentName name))
            td_ (renderDeploymentRevision job)
            td_ (jobLink job)

notFoundAction :: Action ()
notFoundAction = do
  setStatus status404
  renderLayout "Not Found" (BarePage content)
  where
    content = do
      h1_ [class_ "mt-5"] "Not Found"
      p_ [class_ "lead"] $ do
        "The requested page could not be found. Try "
        a_ [href_ "/"] "going back to the start page"
        "."

badRequestAction :: Html () -> Action ()
badRequestAction message = do
  setStatus status400
  renderLayout "Bad request!" $ WithNavigation [] $ do
    h1_ [class_ "mt-5"] "Bad request!"
    p_ [class_ "lead"] message

jobIdHref :: JobId -> Text
jobIdHref jobId = "/jobs/" <> jobId

jobHref :: DeploymentJob -> Text
jobHref = jobIdHref . jobId

jobIdLink :: JobId -> Html ()
jobIdLink jobId = a_ [href_ (jobIdHref jobId)] (toHtml jobId)

jobLink :: DeploymentJob -> Html ()
jobLink = jobIdLink . jobId

homeAction :: Action ()
homeAction = do
  Env {..}    <- getState
  deployments <- liftIO $ envDeployer ? GetDeployments
  deployState <- liftIO $ envDeployer ? GetCurrentState
  jobs        <- liftIO $ envDeployer ? GetJobs (Just 10)
  let content = do
        div_ [class_ "row mt-5"] $ do
          div_ [class_ "col col-4"] $ renderCurrentState deployState
          div_ [class_ "col col-8"] $ renderLatestSuccessful deployments jobs
        div_ [class_ "row mt-5"] $ div_ [class_ "col"] $ renderDeployJobs jobs
  renderLayout "Lodjur Deployment Manager" (WithNavigation [] content)

welcomeAction :: Action ()
welcomeAction =
  renderLayout "Lodjur Deployment Manager" $ BarePage $
    div_ [class_ "landing-page"] $ do
      h1_ "Lodjur Deployment Manager"
      p_ "Welcome to Lodjur, the NixOps deployment manager! To get going, please login."
      nav_ [class_ "log-in-nav"] $ do
        a_ [href_ "/auth/github/login", class_ "btn btn-large btn-primary"] "Log in with GitHub"

newDeployAction :: Action ()
newDeployAction = readState >>= \case
  Idle -> do
    user     <- requireUser
    deployer <- envDeployer <$> getState
    dName    <- DeploymentName <$> param' "deployment-name"
    revision <- Git.Revision <$> param' "revision"
    -- action   <- param' "action"
    now      <- liftIO getCurrentTime
    -- deployType <-
    --   case action of
    --     "Build" -> return BuildOnly
    --     "Check" -> return BuildCheck
    --     "Deploy" -> return BuildDeploy
    --     _ -> raise $ "Unknown deploy action: " <> Text.pack action
    liftIO (deployer ? Deploy dName revision now (userId user)) >>= \case
      Just job -> do
        setStatus status302
        setHeader "Location" (jobHref job)
      Nothing -> badRequestAction "Could not deploy!"
  Deploying job ->
    badRequestAction $ "Already deploying " <> jobLink job <> "."

getDeploymentJobsAction :: Action ()
getDeploymentJobsAction = do
  Env {..} <- getState
  jobs     <- liftIO (envDeployer ? GetJobs Nothing)
  renderLayout "Lodjur Deployment Manager" $ WithNavigation [jobsLink] $
    div_ [class_ "row mt-5"] $ div_ [class_ "col"] $
      renderDeployJobs jobs

getLogs :: JobId -> Action [Output]
getLogs jobId = do
  outputLoggers <- envOutputLoggers <$> getState
  liftIO $ do
    logger <- outputLoggers ? SpawnOutputLogger jobId
    output <- logger ? GetOutputLog
    kill logger
    return output

showJobAction :: Text -> Action ()
showJobAction jobId = do
  Env {..}   <- getState
  job        <- liftIO $ envDeployer ? GetJob jobId
  eventLogs  <- liftIO $ envEventLogger ? GetEventLogs
  -- outputLog  <- getLogs jobId
  -- appResults <- liftIO $ envDeployer ? GetCheckResults jobId
  case (job, HashMap.lookup jobId eventLogs) of
    (Just (job', _), Just eventLog) ->
      renderLayout "Job Details" $ WithNavigation ["Jobs", jobIdLink jobId] $ do
        div_ [class_ "row mt-5 mb-5"] $ div_ [class_ "col"] $ do
          "Deploy of revision "
          em_ $ toHtml (Git.unRevision (deploymentRevision job'))
          " to "
          em_ $ toHtml (unDeploymentName (deploymentJobName job'))
          ", by "
          userIdLink (deploymentJobStartedBy job')
          "."
        div_ [class_ "row mt-3"] $ div_ [class_ "col"] $ do
          h2_ [class_ "mb-3"] "Event Log"
          renderEventLog eventLog
        -- commandOutput jobId outputLog
        -- mapM_ (uncurry checkResults) appResults
    _ -> notFoundAction

commandOutput :: JobId -> [Output] -> Html ()
commandOutput jobId outputLog = do
  div_ [class_ "row mt-3 mb-5"] $ div_ [class_ "col"] $ do
    h2_ [class_ "mb-3"] "Command Output"
    let lineAttr = data_ "last-line-at" . lastLineAt $ outputLog
        allAttrs =
          lineAttr : [class_ "command-output", data_ "job-id" jobId]
    div_ allAttrs $ pre_ $ foldM_ displayOutput Nothing outputLog
  div_ [class_ "autoscroll"]
    $ div_ [class_ "form-check form-check-inline form-control-small"]
    $ do
        input_
          [ class_ "form-check-input"
          , type_ "checkbox"
          , id_ "autoscroll-check"
          ]
        label_ [class_ "form-check-label", for_ "autoscroll-check"]
                "Auto-Scroll"
 where
  displayOutput :: Maybe UTCTime -> Output -> Html (Maybe UTCTime)
  displayOutput previousTime output = foldM displayLine previousTime (outputLines output)
    where
      displayLine :: Maybe UTCTime -> String -> Html (Maybe UTCTime)
      displayLine previousTime' line = div_ [class_ "line"] $ do
        case previousTime' of
          Just t | t `sameSecond` outputTime output -> return ()
          _ -> time_ $ toHtml (hourMinSec (outputTime output))
        toHtml line
        return (Just (outputTime output))
  sameSecond t1 t2 = toSeconds t1 == toSeconds t2
  toSeconds :: UTCTime -> Integer
  toSeconds  = round . utcTimeToPOSIXSeconds
  lastLineAt = \case
    []        -> ""
    outputLog' -> Text.pack (show $ outputIndex (last outputLog'))

checkResults :: AppName -> RSpecResult -> Html ()
checkResults appName result =
  div_ [class_ "row mt-3 mb-5"] $ div_ [class_ "col"] $ do
    h2_ [class_ "mb-3"] (toHtml $ "RSpec Results for " <> appName)
    div_ [] $ pre_ $ mapM_ displayTest (rspecExamples result)
  where
    displayTest :: TestResult -> Html ()
    displayTest TestResult {..} =
      div_ [class_ "row"] $ do
        div_ [class_ "col"] $ toHtml testDescription
        div_ [class_ "col"] $ toHtml testFullDescription
        div_ [class_ "col"] $ toHtml testStatus
        div_ [class_ "col"] $ toHtml testFilePath
        div_ [class_ "col"] $ toHtml (show testLineNumber)

data OutputEvent = OutputLineEvent
  { outputEventIndex :: Integer
  , outputEventTime  :: UTCTime
  , outputEventLines :: [String]
  } deriving (Generic, ToJSON)

streamOutputAction :: Text -> Action ()
streamOutputAction jobId = do
  Env {..} <- getState
  from     <- param "from"
  setHeader "Content-Type"      "text/event-stream"
  setHeader "Cache-Control"     "no-cache"
  setHeader "X-Accel-Buffering" "no"
  chan <- liftIO newChan
  stream (streamLog envOutputStreamer chan jobId from)

streamLog
  :: Ref OutputStreamer
  -> Chan OutputStream
  -> JobId
  -> Maybe Integer
  -> StreamingBody
streamLog outputStreamer chan jobId from send flush = bracket_
  (outputStreamer ! SubscribeOutputLog jobId from chan)
  (outputStreamer ? UnsubscribeOutputLog jobId chan)
  go
 where
  go = do
    moutput <- readChan chan
    case moutput of
      NextOutput output -> do
        void . send $ Binary.fromByteString "event: output\n"
        let event = OutputLineEvent
              { outputEventIndex = outputIndex output
              , outputEventTime  = outputTime output
              , outputEventLines = outputLines output
              }
        void . send $ Binary.fromLazyByteString
          ("data: " <> encode event <> "\n")
        void . send $ Binary.fromByteString "\n"
        void flush
        go
      Fence -> do
        void . send $ Binary.fromByteString "event: end\n"
        void flush

getResultAction :: Text -> Text -> Action ()
getResultAction jobId appName = do
  Env {..}  <- getState
  appResult <- liftIO $ envDeployer ? GetCheckResult jobId appName
  json appResult
-}

data GithubOwner = GithubOwner
  { ownerId     :: !Int
  , ownerLogin  :: !Text
  } deriving (Eq, Show)

instance FromJSON GithubOwner where
  parseJSON (Object o) = do
    ownerId     <- o .: "id"
    ownerLogin  <- o .: "login"
    return GithubOwner {..}
  parseJSON invalid = typeMismatch "GithubOwner" invalid

data GithubRepository = GithubRepository
  { repositoryId       :: !Int
  , repositoryName     :: !Text
  , repositoryFullName :: !Text
  , repositoryOwner    :: !GithubOwner
  } deriving (Eq, Show)

instance FromJSON GithubRepository where
  parseJSON (Object o) = do
    repositoryId        <- o .: "id"
    repositoryName      <- o .: "name"
    repositoryFullName  <- o .: "full_name"
    repositoryOwner     <- o .: "owner"
    return GithubRepository {..}
  parseJSON invalid = typeMismatch "GithubRepository" invalid

data GithubApp = GithubApp
  { appId       :: !Int
  , appName     :: !Text
  } deriving (Eq, Show)

instance FromJSON GithubApp where
  parseJSON (Object o) = do
    appId       <- o .: "id"
    appName     <- o .: "name"
    return GithubApp {..}
  parseJSON invalid = typeMismatch "GithubApp" invalid

data GithubCheckSuite = GithubCheckSuite
  { checkSuiteId            :: !Int
  , checkSuiteApp           :: !GithubApp
  , checkSuiteStatus        :: !Text
  , checkSuiteHeadSha       :: !GH.Sha
  , checkSuiteHeadBranch    :: !Text
  } deriving (Eq, Show)

instance FromJSON GithubCheckSuite where
  parseJSON (Object o) = do
    checkSuiteId            <- o .: "id"
    checkSuiteApp           <- o .: "app"
    checkSuiteStatus        <- o .: "status"
    checkSuiteHeadSha       <- o .: "head_sha"
    checkSuiteHeadBranch    <- o .: "head_branch"
    return GithubCheckSuite {..}
  parseJSON invalid = typeMismatch "GithubCheckSuite" invalid

data GithubCheckRun = GithubCheckRun
  { checkRunId              :: !Int
  , checkRunCheckSuite      :: !GithubCheckSuite
  , checkRunName            :: !Text
  , checkRunStatus          :: !Text
  , checkRunConclusion      :: !(Maybe Text)
  , checkRunStartedAt       :: !(Maybe UTCTime)
  , checkRunCompletedAt     :: !(Maybe UTCTime)
  } deriving (Eq, Show)

instance FromJSON GithubCheckRun where
  parseJSON (Object o) = do
    checkRunId              <- o .: "id"
    checkRunCheckSuite      <- o .: "check_suite"
    checkRunName            <- o .: "name"
    checkRunStatus          <- o .: "status"
    checkRunConclusion      <- o .:?"conclusion"
    checkRunStartedAt       <- o .:?"started_at"
    checkRunCompletedAt     <- o .:?"completed_at"
    return GithubCheckRun {..}
  parseJSON invalid = typeMismatch "GithubCheckRun" invalid

data GithubPushEvent = GithubPushEvent
  { pushRef        :: !Text
  , pushRepository :: !GithubRepository
  } deriving (Eq, Show)

instance FromJSON GithubPushEvent where
  parseJSON (Object o) = do
    pushRef         <- o .: "ref"
    pushRepository  <- o .: "repository"
    return GithubPushEvent {..}
  parseJSON invalid = typeMismatch "GithubPushEvent" invalid

data GithubCreateEvent = GithubCreateEvent
  { createRef        :: !Text
  , createRepository :: !GithubRepository
  } deriving (Eq, Show)

instance FromJSON GithubCreateEvent where
  parseJSON (Object o) = do
    createRef        <- o .: "ref"
    createRepository <- o .: "repository"
    return GithubCreateEvent {..}
  parseJSON invalid = typeMismatch "GithubCreateEvent" invalid

data GithubDeleteEvent = GithubDeleteEvent
  { deleteRef        :: !Text
  , deleteRepository :: !GithubRepository
  } deriving (Eq, Show)

instance FromJSON GithubDeleteEvent where
  parseJSON (Object o) = do
    deleteRef        <- o .: "ref"
    deleteRepository <- o .: "repository"
    return GithubDeleteEvent {..}
  parseJSON invalid = typeMismatch "GithubDeleteEvent" invalid

data GithubCheckSuiteEvent = GithubCheckSuiteEvent
  { checkSuiteEventAction       :: !Text
  , checkSuiteEventRepository   :: !GithubRepository
  , checkSuiteEventCheckSuite   :: !GithubCheckSuite
  } deriving (Eq, Show)

instance FromJSON GithubCheckSuiteEvent where
  parseJSON (Object o) = do
    checkSuiteEventAction       <- o .: "action"
    checkSuiteEventRepository   <- o .: "repository"
    checkSuiteEventCheckSuite   <- o .: "check_suite"
    return GithubCheckSuiteEvent {..}
  parseJSON invalid = typeMismatch "GithubCheckSuiteEvent" invalid

data GithubCheckRunEvent = GithubCheckRunEvent
  { checkRunEventAction       :: !Text
  , checkRunEventCheckRun     :: !GithubCheckRun
  , checkRunEventRepository   :: !GithubRepository
  } deriving (Eq, Show)

instance FromJSON GithubCheckRunEvent where
  parseJSON (Object o) = do
    checkRunEventAction       <- o .: "action"
    checkRunEventCheckRun     <- o .: "check_run"
    checkRunEventRepository   <- o .: "repository"
    return GithubCheckRunEvent {..}
  parseJSON invalid = typeMismatch "GithubCheckRunEvent" invalid

secureJsonData :: FromJSON a => Action a
secureJsonData = do
  key     <- envGithubSecretToken <$> getState
  message <- body
  -- liftIO $ C8.putStrLn message
  xhubsig <-
    header "X-HUB-SIGNATURE"
      >>= maybe (raise "Github didn't send a valid X-HUB-SIGNATURE") return
  signature <- maybe
    (raise "Github X-HUB-SIGNATURE didn't start with 'sha1='")
    return
    (Text.stripPrefix "sha1=" xhubsig)
  digest <- maybe
    (raise "Invalid SHA1 digest sent in X-HUB-SIGNATURE")
    return
    (digestFromByteString $ fst $ Base16.decode $ Text.encodeUtf8 signature)
  unless (hmac key message == HMAC (digest :: Digest SHA1))
    $ raise "Signatures don't match"
  either
    (\e ->
      raise
        $  "jsonData - no parse: "
        <> Text.pack e
        <> ". Data was:"
        <> Text.decodeUtf8 message
    )
    return
    (eitherDecodeStrict message)

matchRepo :: [Text] -> Text -> Bool
matchRepo [] _ = True
matchRepo rs r = r `elem` rs

requiredHeader :: MonadIO m => Text -> ActionCtxT ctx m Text
requiredHeader hdr =
  header hdr >>= \case
    Just bs -> return bs
    Nothing -> raise $ "Missing required header " <> hdr

webhookAction :: Action ()
webhookAction = do
  pool        <- envDbPool <$> getState
  delivery    <- header "X-GitHub-Delivery"
  githubEvent <- requiredHeader "X-GitHub-Event"
  now         <- liftIO getCurrentTime
  event       <- secureJsonData
  DB.withConn pool $ \conn -> DB.insertEventsE conn
    [ DB.Event  { DB.eventId = default_
                , DB.eventSource = val_ "GitHub"
                , DB.eventDelivery = val_ delivery
                , DB.eventType = val_ githubEvent
                , DB.eventCreatedAt = val_ now
                , DB.eventData = val_ event
                }
    ]
  case githubEvent of
    "check_suite" -> checkSuiteEvent now =<< parseEvent event
    "check_run"   -> checkRunEvent =<< parseEvent event
    _             -> raise "Unknown event received"
  text "Event received"
  where
    parseEvent e =
      case fromJSON e of
        Success a -> return a
        Error err -> raise $ "event - no parse: " <> Text.pack err

checkSuiteEvent :: UTCTime -> GithubCheckSuiteEvent -> Action ()
checkSuiteEvent now GithubCheckSuiteEvent {..} = do
  let action = checkSuiteEventAction
      repo   = checkSuiteEventRepository
      owner  = repositoryOwner repo
      suite  = checkSuiteEventCheckSuite
      app    = checkSuiteApp suite
  ourAppId <- envGithubAppId <$> getState
  if ourAppId == appId app
    then
    case action of
      "requested" -> do
        pool <- envDbPool <$> getState
        mgr <- envManager <$> getState
        auth <- getInstallationAccessToken
        r <- liftIO $ do
          DB.withConn pool $ \conn -> DB.upsertCheckSuite conn $
            DB.CheckSuite { DB.checksuiteId = checkSuiteId suite
                          , DB.checksuiteRepositoryOwner = ownerLogin owner
                          , DB.checksuiteRepositoryName = repositoryName repo
                          , DB.checksuiteHeadSha = GH.getSha (checkSuiteHeadSha suite)
                          , DB.checksuiteStatus = checkSuiteStatus suite
                          , DB.checksuiteStartedAt = Just now
                          , DB.checksuiteCompletedAt = Nothing
                          }
          GH.executeRequestWithMgr mgr auth $
            GH.createCheckRunR
            (GH.N $ ownerLogin owner)
            (GH.N $ repositoryName repo)
            GH.NewCheckRun
              { newCheckRunName         = "nix build"
              , newCheckRunHeadSha      = checkSuiteHeadSha suite
              , newCheckRunDetailsUrl   = Nothing
              , newCheckRunExternalId   = Nothing
              , newCheckRunStatus       = Nothing
              , newCheckRunStartedAt    = Nothing
              , newCheckRunConclusion   = Nothing
              , newCheckRunCompletedAt  = Nothing
              , newCheckRunOutput       = Nothing
              , newCheckRunActions      = Nothing
              }
        liftIO $ print r
        return ()
      _ ->
        raise "Unknown check_suite action received"
    else
      text "Event ignore, different AppId"

checkRunEvent :: GithubCheckRunEvent -> Action ()
checkRunEvent GithubCheckRunEvent {..} = do
  let action = checkRunEventAction
      run    = checkRunEventCheckRun
      suite  = checkRunCheckSuite run
      app    = checkSuiteApp suite
      repo   = checkRunEventRepository
      owner  = repositoryOwner repo
  Env {..} <- getState
  if envGithubAppId == appId app
    then
    case action of
      "created" ->
        liftIO $ do
          DB.withConn envDbPool $ \conn -> DB.upsertCheckRun conn $
            DB.CheckRun   { DB.checkrunId = checkRunId run
                          , DB.checkrunCheckSuite = DB.CheckSuiteKey (checkSuiteId suite)
                          , DB.checkrunName = checkRunName run
                          , DB.checkrunStatus = checkRunStatus run
                          , DB.checkrunConclusion = checkRunConclusion run
                          , DB.checkrunStartedAt = checkRunStartedAt run
                          , DB.checkrunCompletedAt = checkRunCompletedAt run
                          }
          let repo' = Git.Repo (Text.unpack $ ownerLogin owner) (Text.unpack $ repositoryName repo)
          sem <- newQSem 10
          Jobs.runJob (const sem) $
            Jobs.build envGitEnv envBuildEnv repo' (Text.unpack $ GH.getSha $ checkSuiteHeadSha suite)
      _ ->
        raise "Unknown check_run action received"
    else
      text "Event ignore, different AppId"

raise :: MonadIO m => Text -> ActionCtxT ctx m b
raise msg = do
  setStatus status400
  text msg

{-
staticPrefix :: String
staticPrefix = "static/"

static :: (Data.String.IsString a, Semigroup a) => a -> a
static x = "/static/" <> x

redirectStatic :: String -> Policy
redirectStatic staticBase =
  policy (List.stripPrefix staticPrefix) >-> addBase staticBase

requireUser :: Action User
requireUser  =
  readSession >>= \case
    Session{ currentUser = Just u } -> return u
    _ -> do
      currentPath <- ("/" <>) . Text.intercalate "/" . pathInfo <$> request
      let continueTo = Just (RelativeRef Nothing (Text.encodeUtf8 currentPath) mempty Nothing)
      writeSession emptySession { continueTo }
      setStatus status401
      renderLayout "Authentication Required" $ BarePage $
        div_ [class_ "authentication-required"] $ do
          h1_ "Authentication Required"
          p_ "The page you are looking for requires authentication."
          nav_ [class_ "log-in-nav"] $
            a_ [href_ "/auth/github/login", class_ "btn btn-large btn-primary"] "Log in with GitHub"

requireLoggedIn :: App () -> App ()
requireLoggedIn = prehook (void requireUser)

-}

homeAction :: Action ()
homeAction = do
  sess <- readSession
  text (Text.pack $ show sess)

welcomeAction :: Action ()
welcomeAction =
  redirect "/github/login"

ifLoggedIn :: Action () -> Action () -> Action ()
ifLoggedIn thenRoute elseRoute =
  readSession >>= \case
    Session{ currentUser = Just _ } -> thenRoute
    _ -> elseRoute

runServer
  :: Int
  -> String
  -> Env
  -> OAuth2
  -> TeamAuthConfig
  -> IO ()
runServer port staticBase env githubOauth teamAuth = do
    cfg <- defaultSpockCfg emptySession PCNoDatabase env
    runSpock port (spock cfg app)
 where
  app = do
    -- Middleware
    -- middleware (staticPolicy (redirectStatic staticBase))

    -- Auth
    authRoutes githubOauth teamAuth

    -- Routes
    get "/"     (ifLoggedIn homeAction welcomeAction)
    -- requireLoggedIn $ do
    --   get "/jobs" getDeploymentJobsAction
    --   post "/jobs" newDeployAction
    --   get ("jobs" <//> var)               showJobAction
    --   get ("jobs" <//> var <//> "output") streamOutputAction
    --   get ("jobs" <//> var <//> "result" <//> var) getResultAction
    post "/github/webhook" webhookAction

    -- Fallback
    -- hookAnyAll (const notFoundAction)

createInstallationJWT :: Action GH.Token
createInstallationJWT = do
  Env {..} <- getState
  now <- liftIO getPOSIXTime
  let claims = mempty { JWT.iss = JWT.stringOrURI (Text.pack $ show envGithubAppId)
                      , JWT.iat = JWT.numericDate now
                      , JWT.exp = JWT.numericDate (now + 600)
                      }
      jwt = JWT.encodeSigned envGithubAppSigner claims
  return $ Text.encodeUtf8 jwt

newInstallationAccessToken :: Action GH.AccessToken
newInstallationAccessToken = do
  Env {..} <- getState
  tok <- createInstallationJWT
  result <- liftIO $
    GH.executeRequestWithMgr envManager (GH.Bearer tok) $
      GH.createInstallationTokenR (GH.Id envGithubInstallationId)
  case result of
    Left err ->
      raise $ "Unable to fetch installation access token: " <> Text.pack (show err)
    Right token ->
      return token

getInstallationAccessToken :: Action GH.Auth
getInstallationAccessToken = do
  now <- liftIO getCurrentTime
  Env {..} <- getState
  let renew = do
        at <- newInstallationAccessToken
        liftIO $ putMVar envGithubInstallationAccessToken (Just at)
        return (asAuth at)
  tok <- liftIO $ takeMVar envGithubInstallationAccessToken
  case tok of
    Just at ->
      case GH.accessTokenExpiresAt at of
        Just e ->
          if now >= e then renew else return (asAuth at)
        Nothing ->
          return (asAuth at)
    Nothing -> renew
  where
    asAuth = GH.OAuth . GH.accessToken
