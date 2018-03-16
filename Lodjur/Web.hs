{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
module Lodjur.Web (Port, runServer) where

import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader
import qualified Data.HashMap.Strict       as HashMap
import qualified Data.List                 as List
import           Data.Semigroup
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as Lazy
import           Data.Time.Clock           (DiffTime, UTCTime,
                                            diffTimeToPicoseconds,
                                            getCurrentTime, utctDayTime)
import           Data.Time.Clock.POSIX
import           Data.Time.Format          (defaultTimeLocale, formatTime)
import           Lucid.Base                (Html, toHtml)
import qualified Lucid.Base                as Html
import           Lucid.Bootstrap
import           Lucid.Html5
import           Network.HTTP.Types.Status
import           Web.Scotty.Trans

import           Lodjur.Deployer
import           Lodjur.EventLogger
import           Lodjur.OutputLogger
import           Lodjur.Process

data Env = Env
  { envDeployer     :: Ref Deployer
  , envEventLogger  :: Ref EventLogger
  , envOutputLogger :: Ref OutputLogger
  }

type Action = ActionT Lazy.Text (ReaderT Env IO)

readState :: Action DeployState
readState = lift (asks envDeployer) >>= liftIO . (? GetCurrentState)

renderHtml :: Html () -> Action ()
renderHtml = html . Html.renderText

formatUTCTime :: UTCTime -> String
formatUTCTime = formatTime defaultTimeLocale "%c"

hourMinSec :: UTCTime -> String
hourMinSec = formatTime defaultTimeLocale "%H:%M:%S"

renderLayout :: Html () -> [Html ()] -> Html () -> Action ()
renderLayout title breadcrumbs contents =
  renderHtml $ doctypehtml_ $ html_ $ do
    head_ $ do
      title_ title
      link_
        [ rel_ "stylesheet"
        , href_
          "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
        ]
      style_ " .breadcrumb-nav { background-color: #eee; } \
             \ .breadcrumb { background-color: transparent; } \
             \ .command-output { position: relative; } \
             \ .command-output { overflow: auto; } \
             \ .command-output .timestamp { position: absolute; display: inline-block; padding: 0 .25em; right: 0; background: #fff; z-index: 2; } \
             \ .command-output .line:hover { background: #eee; } \
             \ .command-output .line:hover .timestamp { background: #eee; } \
             \ "
    body_ $ do
      nav_ [class_ "navbar navbar-dark bg-dark"] $
        div_ [class_ "container"] $
          a_ [class_ "navbar-brand", href_ "/"] "Lodjur"
      nav_ [class_ "breadcrumb-nav"] $
        div_ [class_ "container"] $
          ol_ [class_ "breadcrumb"] (toBreadcrumbItems (homeLink : breadcrumbs))
      container_ contents
 where
  toBreadcrumbItems :: [Html ()] -> Html ()
  toBreadcrumbItems []       = return ()
  toBreadcrumbItems elements = do
    foldMap (li_ [class_ "breadcrumb-item"])  (init elements)
    li_     [class_ "breadcrumb-item active"] (last elements)
  homeLink = a_ [href_ "/"] "Home"

renderEventLog :: EventLog -> Html ()
renderEventLog []       = p_ [class_ "text-secondary"] "No events available."
renderEventLog eventLog = table_ [class_ "table"] $ do
  tr_ $ do
    th_ "Event"
    th_ "Tag"
    th_ "Time"
    th_ "Description"
  mapM_ renderEvent eventLog
 where
  renderEvent :: JobEvent -> Html ()
  renderEvent event = tr_ $ case event of
    JobRunning startedAt -> do
      td_ $ span_ [class_ "text-primary"] "Started"
      td_ "tag"
      td_ (toHtml (formatUTCTime startedAt))
      td_ ""
    JobFinished JobSuccessful finishedAt -> do
      td_ $ span_ [class_ "text-success"] "Finished"
      td_ "tag"
      td_ (toHtml (formatUTCTime finishedAt))
      td_ ""
    JobFinished (JobFailed e) finishedAt -> do
      td_ $ span_ [class_ "text-danger"] "Failed"
      td_ "tag"
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
      th_ "Tag"
      th_ "Created At"
      th_ "Result"
    mapM_ renderJob jobs
 where
  renderJob :: (DeploymentJob, Maybe JobResult) -> Html ()
  renderJob (job, r) = tr_ $ do
    td_ (jobLink job)
    td_ (toHtml (unDeploymentName (deploymentName job)))
    td_ (toHtml (unTag (deploymentTag job)))
    td_ (toHtml (formatUTCTime (deploymentTime job)))
    case r of
      Just JobSuccessful      -> td_ [class_ "text-success"] "Successful"
      Just (JobFailed reason) -> td_ [class_ "text-danger"] (toHtml reason)
      Nothing                 -> td_ [class_ "text-primary"] "Running"

renderCurrentState :: DeployState -> Html ()
renderCurrentState state = div_ [class_ "card"] $ do
  div_ [class_ "card-header"] "Current State"
  div_ [class_ "card-body text-center"] $ case state of
    Idle          -> span_ [class_ "text-muted h3"] "Idle"
    Deploying job -> do
      div_ [class_ "text-warning h3"] "Deploying"
      a_ [href_ (jobHref job), class_ "text-warning"] $ do
        toHtml (unTag (deploymentTag job))
        " to "
        toHtml (unDeploymentName (deploymentName job))

-- TODO: Convert this to a database query.
renderLatestSuccessful :: [DeploymentName] -> DeploymentJobs -> Html ()
renderLatestSuccessful deploymentNames jobs = div_ [class_ "card"] $ do
  div_ [class_ "card-header text-success"] "Latest Successful"
  table_ [class_ "table table-bordered mb-0"] $
    forM_ successfulJobsByDeploymentName $ \(name, job) -> tr_ $ do
      td_ (toHtml (unDeploymentName name))
      td_ (toHtml (unTag (deploymentTag job)))
      td_ (jobLink job)
 where
  successfulJobsByDeploymentName :: [(DeploymentName, DeploymentJob)]
  successfulJobsByDeploymentName = foldMap
    (\name -> (name,) . fst <$> take 1 (List.filter (successfulJobIn name) jobs))
    deploymentNames
  successfulJobIn n = \case
    (job, Just JobSuccessful) -> deploymentName job == n
    _                         -> False

renderDeployCard :: [DeploymentName] -> [Tag] -> DeployState -> Html ()
renderDeployCard deploymentNames tags state = case state of
  Idle -> div_ [class_ "card"] $ do
    div_ [class_ "card-header"] "New Deploy"
    div_ [class_ "card-body"]
      $ form_ [method_ "post", action_ "/jobs"]
      $ div_ [class_ "row"]
      $ do
          div_ [class_ "col"] $ do
            select_ [name_ "deployment-name", class_ "form-control"]
              $ forM_ deploymentNames
              $ \(unDeploymentName -> n) ->
                  option_ [value_ (Text.pack n)] (toHtml n)
            small_ [class_ "text-muted"]
                   "Name of the Nixops deployment to target."
          div_ [class_ "col"] $ do
            select_ [name_ "tag", class_ "form-control"]
              $ forM_ tags
              $ \(unTag -> tag) -> option_ [value_ tag] (toHtml tag)
            small_ [class_ "text-muted"] "Which git tag to deploy."
          div_ [class_ "col"]
            $ input_
                [ class_ "btn btn-primary form-control"
                , type_ "submit"
                , value_ "Deploy"
                ]
  Deploying _ -> return ()

notFoundAction :: Action ()
notFoundAction = do
  status status404
  renderLayout "Not Found" [] $ do
    h1_ [class_ "mt-5"] "Not Found"
    p_ [class_ "lead"] $ do
      "The requested page could not be found. Try "
      a_ [href_ "/"] "going back to the start page"
      "."

badRequestAction :: Html () -> Action ()
badRequestAction message = do
  status status400
  renderLayout "Bad request!" [] $ do
    h1_ [class_ "mt-5"] "Bad request!"
    p_  [class_ "lead"] message

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
  deployer        <- lift (asks envDeployer)
  deploymentNames <- liftIO $ deployer ? GetDeploymentNames
  tags            <- liftIO $ deployer ? GetTags
  deployState     <- liftIO $ deployer ? GetCurrentState
  jobs            <- liftIO $ deployer ? GetJobs
  renderLayout "Lodjur Deployment Manager" [] $ do
    div_ [class_ "row mt-5"] $ do
      div_ [class_ "col col-4"] $ renderCurrentState deployState
      div_ [class_ "col"] $ renderLatestSuccessful deploymentNames jobs
    div_ [class_ "row mt-5"] $ div_ [class_ "col"] $ renderDeployJobs jobs
    div_ [class_ "row mt-5 mb-5"] $ div_ [class_ "col"] $ renderDeployCard
      deploymentNames
      tags
      deployState

newDeployAction :: Action ()
newDeployAction = readState >>= \case
  Idle -> do
    deployer <- lift (asks envDeployer)
    dName    <- DeploymentName <$> param "deployment-name"
    tag      <- Tag <$> param "tag"
    now      <- liftIO getCurrentTime
    liftIO (deployer ? Deploy dName tag now) >>= \case
      Just job -> do
        status status302
        setHeader "Location" (Lazy.fromStrict (jobHref job))
      Nothing -> badRequestAction "Could not deploy!"
  Deploying job ->
    badRequestAction $ "Already deploying " <> jobLink job <> "."

showJobAction :: Action ()
showJobAction = do
  jobId        <- param "job-id"
  eventLogger  <- lift (asks envEventLogger)
  outputLogger <- lift (asks envOutputLogger)
  eventLogs    <- liftIO $ eventLogger ? GetEventLogs
  outputLog    <- liftIO $ HashMap.lookup jobId <$> outputLogger ? GetOutputLogs
  case HashMap.lookup jobId eventLogs of
    Just eventLog -> renderLayout "Job Details" ["Jobs", jobIdLink jobId] $ do
      div_ [class_ "row mt-5"] $ div_ [class_ "col"] $ do
        h2_ [class_ "mb-3"] "Event Log"
        renderEventLog eventLog
      div_ [class_ "row mt-2"] $ div_ [class_ "col"] $ do
        h2_ [class_ "mb-3"] "Command Output"
        div_ [class_ "command-output"] $ pre_ $
          case outputLog of
            Just outputs
              | not (null outputs) -> foldM_ displayOutput Nothing outputs
            _ -> span_ [class_ "text-muted"] "No output available."
    Nothing -> notFoundAction
 where
  displayOutput :: Maybe UTCTime -> Output -> Html (Maybe UTCTime)
  displayOutput previousTime output = div_ [class_ "line"] $ do
    case previousTime of
      Just t
        | t `sameSecond` outputTime output -> return ()
      _ -> span_ [class_ "timestamp"]
           $ code_
           $ small_
           $ toHtml (hourMinSec (outputTime output))
    toHtml (unlines (outputLines output))
    return (Just (outputTime output))
  sameSecond t1 t2 = toSeconds t1 == toSeconds t2
  toSeconds :: UTCTime -> Integer
  toSeconds = round . utcTimeToPOSIXSeconds

type Port = Int

runServer
  :: Port -> Ref Deployer -> Ref EventLogger -> Ref OutputLogger -> IO ()
runServer port envDeployer envEventLogger envOutputLogger =
  scottyT port (`runReaderT` Env {..}) $ do
    get  "/"             homeAction
    post "/jobs"         newDeployAction
    get  "/jobs/:job-id" showJobAction
    notFound notFoundAction
