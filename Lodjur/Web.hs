{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Lodjur.Web (Port, runServer) where

import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader
import qualified Data.HashMap.Strict       as HashMap
import           Data.Semigroup
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as Lazy
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

renderLayout :: Html () -> Html () -> Action ()
renderLayout title contents = renderHtml $ doctypehtml_ $ html_ $ do
  head_ $ do
    title_ title
    link_
      [ rel_ "stylesheet"
      , href_
        "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
      ]
  body_ (container_ contents)

renderEventLog :: EventLog -> Html ()
renderEventLog []       = p_ [class_ "text-secondary"] "No events available."
renderEventLog eventLog = table_ [class_ "table table-striped"] $ do
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
      td_ $ span_ [class_ "text-info"] "Started"
      td_ "tag"
      td_ (toHtml (show startedAt))
      td_ ""
    JobFinished JobSuccessful finishedAt -> do
      td_ $ span_ [class_ "text-success"] "Finished"
      td_ "tag"
      td_ (toHtml (show finishedAt))
      td_ ""
    JobFinished (JobFailed e) finishedAt -> do
      td_ $ span_ [class_ "text-danger"] "Failed"
      td_ "tag"
      td_ (toHtml (show finishedAt))
      td_ [style_ "color: red;"] (toHtml e)

renderDeployJobs :: DeploymentJobs -> Html ()
renderDeployJobs []   = p_ [class_ "text-secondary"] "No jobs available."
renderDeployJobs jobs = table_ [class_ "table table-striped"] $ do
  tr_ $ do
    th_ "Job"
    th_ "Deployment"
    th_ "Tag"
    th_ "Result"
  mapM_ renderJob jobs
 where
  renderJob :: (DeploymentJob, Maybe JobResult) -> Html ()
  renderJob (job, r) = tr_ $ do
    td_ (jobLink job)
    td_ (toHtml (unDeploymentName (deploymentName job)))
    td_ (toHtml (unTag (deploymentTag job)))
    case r of
      Just JobSuccessful      -> td_ [class_ "text-success"] "Successful"
      Just (JobFailed reason) -> td_ [class_ "text-danger"] (toHtml reason)
      Nothing                 -> td_ [class_ "text-primary"] "Running"

renderDeployCard :: [DeploymentName] -> [Tag] -> DeployState -> Html ()
renderDeployCard deploymentNames tags state = do
  h2_ [class_ "mt-5"] "Current State"
  case state of
    Idle -> do
      p_ [class_ "text-muted"] "Idle"
      div_ [class_ "card"] $ do
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
    Deploying job ->
      p_ [class_ "text-info"]
        $  toHtml
        $  "Deploying tag "
        <> unTag (deploymentTag job)
        <> "..."

notFoundAction :: Action ()
notFoundAction = do
  status status404
  renderLayout "Not Found" $ do
    h1_ [class_ "mt-5"] "Not Found"
    p_ [class_ "lead"] $ do
      "The requested page could not be found. Try "
      a_ [href_ "/"] "going back to the start page"
      "."

badRequestAction :: Html () -> Action ()
badRequestAction message = do
  status status400
  renderLayout "Bad request!" $ do
    h1_ [class_ "mt-5"] "Bad request!"
    p_  [class_ "lead"] message

jobHref :: DeploymentJob -> Text
jobHref job = "/jobs/" <> jobId job

jobLink :: DeploymentJob -> Html ()
jobLink job = a_ [href_ (jobHref job)] (toHtml (jobId job))

homeAction :: Action ()
homeAction = do
  deployer        <- lift (asks envDeployer)
  deploymentNames <- liftIO $ deployer ? GetDeploymentNames
  tags            <- liftIO $ deployer ? GetTags
  deployState     <- liftIO $ deployer ? GetCurrentState
  jobs            <- liftIO $ deployer ? GetJobs
  renderLayout "Lodjur Deployment Manager" $ do
    div_ [class_ "row"] $ div_ [class_ "col"] $ do
      h1_ [class_ "mt-5"] "Lodjur"
      p_  [class_ "lead"] "Mpowered's Nixops Deployment Frontend"
    div_ [class_ "row"] $ div_ [class_ "col"] $ renderDeployCard
      deploymentNames
      tags
      deployState
    div_ [class_ "row"] $ div_ [class_ "col"] $ do
      h2_ [class_ "mt-5"] "Jobs"
      renderDeployJobs jobs

newDeployAction :: Action ()
newDeployAction = readState >>= \case
  Idle -> do
    deployer <- lift (asks envDeployer)
    dName    <- DeploymentName <$> param "deployment-name"
    tag      <- Tag <$> param "tag"
    liftIO (deployer ? Deploy dName tag) >>= \case
      Just job -> do
        status status302
        setHeader "Location" (Lazy.fromStrict (jobHref job))
      Nothing -> badRequestAction "Could not deploy!"
  Deploying job ->
    badRequestAction $ "Already deploying " <> jobLink job <> "."

showJobAction :: Action ()
showJobAction = do
  jobId         <- param "job-id"
  eventLogger   <- lift (asks envEventLogger)
  outputLogger  <- lift (asks envOutputLogger)
  eventLogs     <- liftIO $ eventLogger ? GetEventLogs
  outputLog    <- liftIO $ HashMap.lookup jobId <$> outputLogger ? GetOutputLogs
  case HashMap.lookup jobId eventLogs of
    Just eventLog -> do
      let title = "Job Details"
      renderLayout title $ do
        h1_ [class_ "mt-5"] title
        p_  [class_ "lead"] (toHtml jobId)
        div_ [class_ "row mt-5"] $ div_ [class_ "col"] $ do
          h2_ [class_ "mb-3"] "Event Log"
          renderEventLog eventLog
        div_ [class_ "row mt-2"] $ div_ [class_ "col"] $ do
          h2_   [class_ "mb-3"]       "Command Output"
          displayOutput outputLog
    Nothing -> notFoundAction

  where
    displayOutput :: Maybe Output -> Html ()
    displayOutput Nothing =
      span_ [class_ "text-muted"] "No output available."
    displayOutput (Just output) =
      pre_ (code_ (toHtml (unlines output)))

type Port = Int

runServer
  :: Port -> Ref Deployer -> Ref EventLogger -> Ref OutputLogger -> IO ()
runServer port envDeployer envEventLogger envOutputLogger =
  scottyT port (`runReaderT` Env {..}) $ do
    get  "/"             homeAction
    post "/jobs"         newDeployAction
    get  "/jobs/:job-id" showJobAction
    notFound notFoundAction
