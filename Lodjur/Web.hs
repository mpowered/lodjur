{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
module Lodjur.Web
  ( Port
  , runServer
  )
where

import           Control.Concurrent
import           Control.Exception             (bracket_)
import           Control.Monad
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader
import           Crypto.Hash
import           Crypto.MAC.HMAC
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Binary.Builder           as Binary
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Base16        as Base16
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
import           Lucid.Base                    (Html, toHtml, makeAttribute)
import qualified Lucid.Base                    as Html
import           Lucid.Bootstrap
import           Lucid.Html5
import           Network.HTTP.Types.Status
import           Network.OAuth.OAuth2
import           Network.Wai                   (StreamingBody)
import           Network.Wai.Middleware.Static (Policy, addBase, policy,
                                                staticPolicy, (>->))
import           Web.Spock                     hiding (static)
import           Web.Spock.Config
import           Web.Spock.Lucid

import           Lodjur.Deployment
import           Lodjur.Deployment.Deployer
import           Lodjur.Events.EventLogger
import qualified Lodjur.Git                    as Git
import           Lodjur.Git.GitAgent
import           Lodjur.Git.GitReader
import           Lodjur.Output.OutputLogger
import           Lodjur.Output.OutputLoggers
import           Lodjur.Output.OutputStreamer
import           Lodjur.Process
import           Lodjur.User

import           Lodjur.Web.Auth.GitHub
import           Lodjur.Web.Base

import           Paths_lodjur

readState :: Action DeployState
readState = getState >>= liftIO . (? GetCurrentState) . envDeployer

renderHtml :: Html () -> Action ()
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

currentUserNav :: Maybe Session -> Html ()
currentUserNav sess =
  ul_ [class_ "navbar-nav my-2"] $
    case sess of
      Just Session { currentUser = User {..}} ->
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

renderLayout :: Html () -> Layout -> Action ()
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
      th_ "Deployment"
      th_ "Revision"
      th_ "Created At"
      th_ "Created By"
      th_ "Result"
    mapM_ renderJob jobs
 where
  renderJob :: (DeploymentJob, Maybe JobResult) -> Html ()
  renderJob (job, r) = tr_ $ do
    td_ (jobLink job)
    if deploymentBuildOnly job
      then td_ [class_ "text-secondary"] "(Build-only)"
      else td_ (toHtml (unDeploymentName (deploymentJobName job)))
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

renderDeployDatalist :: [Git.Revision] -> [Git.Ref] -> Text -> Html ()
renderDeployDatalist revs refs listId = datalist_ [id_ listId] $ do
  forM_ refs $ \case
    Git.Branch name rev ->
      option_ [value_ (Git.unRevision rev)] (toHtml name <> " (branch)")
    Git.Tag name rev ->
      option_ [value_ (Git.unRevision rev)] (toHtml name <> " (tag)")
  forM_ (revs <> map Git.refRevision refs)
    $ \rev -> option_ [value_ (Git.unRevision rev)] mempty


renderDeployCard
  :: [Deployment] -> [Git.Revision] -> [Git.Ref] -> DeployState -> Html ()
renderDeployCard deployments revisions refs state = case state of
  Idle -> do
    div_ [class_ "card", id_ "deploy"] $ do
      div_ [class_ "card-header"] "New Deploy"
      div_ [class_ "card-body"]
        $ form_ [method_ "post", action_ "/jobs"]
        $ div_ [class_ "row"]
        $ do
            div_ [class_ "col"] $ do
              select_
                  [ name_ "deployment-name"
                  , class_ "form-control"
                  , id_ "deployment-selector"
                  ]
                $ forM_ deployments
                $ \Deployment {..} ->
                    let n = unDeploymentName deploymentName
                        warnAttrs =
                          if deploymentWarn then [data_ "warn" "warn"] else []
                    in  option_ (value_ n : warnAttrs) (toHtml n)
              small_ [class_ "text-muted"]
                     "Name of the Nixops deployment to target."
            div_ [class_ "col"] $ do
              input_
                [name_ "revision", list_ "revisions", class_ "form-control"]
              renderDeployDatalist revisions refs "revisions"
              small_ [class_ "text-muted"] "Which git revision to deploy."
            div_ [class_ "col"] $ input_
              [ class_ "btn btn-secondary form-control"
              , type_ "submit"
              , name_ "action"
              , value_ "Build"
              ]
            div_ [class_ "col"] $ input_
              [ class_ "btn btn-primary form-control"
              , type_ "submit"
              , name_ "action"
              , value_ "Deploy"
              ]
  Deploying _ -> return ()

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
  revisions   <- liftIO $ envGitReader ? GetRevisions
  refs        <- liftIO $ envGitReader ? GetRefs
  deployState <- liftIO $ envDeployer ? GetCurrentState
  jobs        <- liftIO $ envDeployer ? GetJobs (Just 10)
  let content = do
      div_ [class_ "row mt-5"] $ do
        div_ [class_ "col col-4"] $ renderCurrentState deployState
        div_ [class_ "col col-8"] $ renderLatestSuccessful deployments jobs
      div_ [class_ "row mt-5"] $ div_ [class_ "col"] $ renderDeployJobs jobs
      div_ [class_ "row mt-5 mb-5"] $ div_ [class_ "col"] $ renderDeployCard
        deployments
        revisions
        refs
        deployState
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
    user <- requireUser
    deployer <- envDeployer <$> getState
    dName    <- DeploymentName <$> param' "deployment-name"
    revision <- Git.Revision <$> param' "revision"
    action   <- param' "action"
    now      <- liftIO getCurrentTime
    let buildOnly = action == ("Build" :: String)
    liftIO (deployer ? Deploy dName revision now buildOnly (userId user)) >>= \case
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

getJobLogs :: JobId -> Action [Output]
getJobLogs jobId = do
  outputLoggers <- envOutputLoggers <$> getState
  liftIO $ do
    logger <- outputLoggers ? SpawnOutputLogger jobId
    output <- logger ? GetOutputLog
    kill logger
    return output

showJobAction :: Text -> Action ()
showJobAction jobId = do
  Env {..}  <- getState
  job       <- liftIO $ envDeployer ? GetJob jobId
  eventLogs <- liftIO $ envEventLogger ? GetEventLogs
  outputLog <- getJobLogs jobId
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
    _ -> notFoundAction
 where
  displayOutput :: Maybe UTCTime -> Output -> Html (Maybe UTCTime)
  displayOutput previousTime output = div_ [class_ "line"] $ do
    case previousTime of
      Just t | t `sameSecond` outputTime output -> return ()
      _ -> time_ $ toHtml (hourMinSec (outputTime output))
    toHtml (unlines (outputLines output))
    return (Just (outputTime output))
  sameSecond t1 t2 = toSeconds t1 == toSeconds t2
  toSeconds :: UTCTime -> Integer
  toSeconds  = round . utcTimeToPOSIXSeconds
  lastLineAt = \case
    []        -> ""
    outputLog -> Text.pack (show $ outputIndex (last outputLog))

data OutputEvent = OutputLineEvent
  { outputEventIndex :: Integer
  , outputEventTime  :: UTCTime
  , outputEventLines :: [String]
  } deriving (Generic, ToJSON)

streamOutputAction :: Text -> Action ()
streamOutputAction jobId = do
  from           <- param "from"
  outputStreamer <- envOutputStreamer <$> getState
  setHeader "Content-Type"      "text/event-stream"
  setHeader "Cache-Control"     "no-cache"
  setHeader "X-Accel-Buffering" "no"
  chan <- liftIO newChan
  stream (streamLog outputStreamer chan jobId from)

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

data GithubRepository = GithubRepository
  { repositoryId       :: Integer
  , repositoryName     :: Text
  , repositoryFullName :: Text
  } deriving (Eq, Show)

instance FromJSON GithubRepository where
  parseJSON (Object o) = do
    repositoryId        <- o .: "id"
    repositoryName      <- o .: "name"
    repositoryFullName  <- o .: "full_name"
    return GithubRepository {..}
  parseJSON invalid = typeMismatch "GithubRepository" invalid

data GithubPushEvent = GithubPushEvent
  { pushRef        :: Text
  , pushRepository :: GithubRepository
  } deriving (Eq, Show)

instance FromJSON GithubPushEvent where
  parseJSON (Object o) = do
    pushRef         <- o .: "ref"
    pushRepository  <- o .: "repository"
    return GithubPushEvent {..}
  parseJSON invalid = typeMismatch "GithubPushEvent" invalid

data GithubCreateEvent = GithubCreateEvent
  { createRef        :: Text
  , createRepository :: GithubRepository
  } deriving (Eq, Show)

instance FromJSON GithubCreateEvent where
  parseJSON (Object o) = do
    createRef        <- o .: "ref"
    createRepository <- o .: "repository"
    return GithubCreateEvent {..}
  parseJSON invalid = typeMismatch "GithubCreateEvent" invalid

data GithubDeleteEvent = GithubDeleteEvent
  { deleteRef        :: Text
  , deleteRepository :: GithubRepository
  } deriving (Eq, Show)

instance FromJSON GithubDeleteEvent where
  parseJSON (Object o) = do
    deleteRef        <- o .: "ref"
    deleteRepository <- o .: "repository"
    return GithubDeleteEvent {..}
  parseJSON invalid = typeMismatch "GithubDeleteEvent" invalid

secureJsonData :: FromJSON a => Action a
secureJsonData = do
  key     <- envGithubSecretToken <$> getState
  message <- body
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
    (digestFromByteString $ fst $ Base16.decode $ Text.encodeUtf8 $ signature)
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

refreshRemoteAction :: Action ()
refreshRemoteAction = do
  event <- header "X-GitHub-Event"
  case event of
    Just "push" -> do
      payload <- secureJsonData
      refresh (repositoryFullName $ pushRepository payload)
    Just "create" -> do
      payload <- secureJsonData
      refresh (repositoryFullName $ createRepository payload)
    Just "delete" -> do
      payload <- secureJsonData
      refresh (repositoryFullName $ deleteRepository payload)
    _ -> raise "Unsupported event"
 where
  refresh repo = do
    repos <- envGithubRepos <$> getState
    if matchRepo repos repo
      then do
        gitAgent <- envGitAgent <$> getState
        liftIO (gitAgent ! FetchRemote)
        text "Queued FetchRemote"
      else text "Ignored refresh request for uninteresting repository"

type Port = Int

staticPrefix :: String
staticPrefix = "static/"

static :: (Data.String.IsString a, Semigroup a) => a -> a
static x = "/static/" <> x

raise :: MonadIO m => Text -> ActionCtxT ctx m b
raise msg = do
  setStatus status400
  text msg

redirectStatic :: String -> Policy
redirectStatic staticBase =
  policy (List.stripPrefix staticPrefix) >-> addBase staticBase

requireUser :: Action User
requireUser  =
  readSession >>= \case
    Just Session{..} -> return currentUser
    Nothing -> do
      setStatus status401
      text "Not authenticated"

requireLoggedIn :: App () -> App ()
requireLoggedIn = prehook $ do
  readSession >>= \case
    Just Session{} -> return ()
    Nothing -> notFoundAction

ifLoggedIn :: Action () -> Action () -> Action ()
ifLoggedIn thenRoute elseRoute = do
  readSession >>= \case
    Just Session{} -> thenRoute
    Nothing -> elseRoute

runServer
  :: Port
  -> String
  -> Ref Deployer
  -> Ref EventLogger
  -> Ref OutputLoggers
  -> Ref OutputStreamer
  -> Ref GitAgent
  -> Ref GitReader
  -> ByteString
  -> [Text]
  -> OAuth2
  -> IO ()
runServer port staticBase envDeployer envEventLogger envOutputLoggers envOutputStreamer envGitAgent envGitReader envGithubSecretToken envGithubRepos githubOauth
  = do
    cfg <- defaultSpockCfg Nothing PCNoDatabase Env {..}
    runSpock port (spock cfg app)
 where
  app = do
    -- Middleware
    middleware (staticPolicy (redirectStatic staticBase))

    -- Auth
    authRoutes githubOauth

    -- Routes
    get "/"     (ifLoggedIn homeAction welcomeAction)
    requireLoggedIn $ do
      get "/jobs" getDeploymentJobsAction
      post "/jobs" newDeployAction
      get ("jobs" <//> var)               showJobAction
      get ("jobs" <//> var <//> "output") streamOutputAction
      post "/webhook/git/refresh" refreshRemoteAction

    -- Fallback
    hookAnyAll (const notFoundAction)
