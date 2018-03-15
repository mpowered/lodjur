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
import           Lodjur.Process

data Env = Env
  { envDeployer    :: Ref Deployer
  , envEventLogger :: Ref EventLogger
  }

type Action = ActionT Lazy.Text (ReaderT Env IO)

readState :: Action DeployState
readState = lift (asks envDeployer) >>= liftIO . (? GetCurrentState)

renderHtml :: Html () -> Action ()
renderHtml = html . Html.renderText

renderLayout :: Text -> Html () -> Action ()
renderLayout title contents = renderHtml $ doctypehtml_ $ html_ $ do
  head_ $ do
    title_ (toHtml title)
    link_
      [ rel_ "stylesheet"
      , href_
        "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
      ]
  body_ contents

renderEventLogs :: EventLogs -> Html ()
renderEventLogs eventLogs = do
  h2_ [class_ "mt-5"] "Event Log"
  renderBody eventLogs
 where
  renderBody :: EventLogs -> Html ()
  renderBody eventlogs
    | HashMap.null eventlogs = p_ [class_ "text-secondary"]
                                  "No history available."
    | otherwise = table_ [class_ "table table-striped"] $ do
      tr_ $ do
        th_ "Event"
        th_ "Tag"
        th_ "Time"
        th_ "Description"
      forM_ (HashMap.toList eventlogs) $ \(jobid, eventlog) -> do
        tr_ [class_ "table-primary"] $ td_ [colspan_ "4"] (toHtml jobid)
        renderEvents eventlog
  renderEvents :: EventLog -> Html ()
  renderEvents events = forM_ events $ \event -> tr_ $ case event of
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
                small_ [class_ "text-muted"] "Name of the Nixops deployment to target."
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

homeAction :: Action ()
homeAction = do
  deployer        <- lift (asks envDeployer)
  deploymentNames <- liftIO $ deployer ? GetDeploymentNames
  tags            <- liftIO $ deployer ? GetTags
  deployState     <- liftIO $ deployer ? GetCurrentState
  eventLogger     <- lift (asks envEventLogger)
  eventLogs       <- liftIO $ eventLogger ? GetEventLogs
  renderLayout "Lodjur Deployment Manager" $ container_ $ do
    div_ [class_ "row"] $ div_ [class_ "col"] $ do
      h1_ [class_ "mt-5"] "Lodjur"
      p_  [class_ "lead"] "Mpowered's Nixops Deployment Frontend"
    div_ [class_ "row"] $ div_ [class_ "col"] $ renderDeployCard
      deploymentNames
      tags
      deployState
    div_ [class_ "row"] $ div_ [class_ "col"] $ renderEventLogs eventLogs

newDeployAction :: Action ()
newDeployAction = readState >>= \case
  Idle -> do
    deployer <- lift (asks envDeployer)
    dName    <- DeploymentName <$> param "deployment-name"
    tag      <- Tag <$> param "tag"
    status status302
    setHeader "Location" "/"
    void $ liftIO $ deployer ? Deploy dName tag
  Deploying job -> do
    status status400
    renderLayout "Already Deploying"
      $  p_
      $  toHtml
      $  "Already deploying a tag: "
      <> unTag (deploymentTag job)

type Port = Int

runServer :: Port -> Ref Deployer -> Ref EventLogger -> IO ()
runServer port envDeployer envEventLogger =
  scottyT port (`runReaderT` Env {..}) $ do
    get  "/" homeAction
    post "/jobs" newDeployAction
