{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Lodjur.Web (Port, runServer) where

import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader
import           Data.Semigroup
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as Lazy
import           Lucid.Base                (Html, toHtml)
import qualified Lucid.Base                as Html
import           Lucid.Bootstrap
import           Lucid.Html5
import           Network.HTTP.Types.Status
import           Web.Scotty.Trans
import qualified Data.Text as Text

import           Lodjur.Process
import           Lodjur.Deploy

type Action = ActionT Lazy.Text (ReaderT (Ref Deployer) IO)

readState :: Action DeployState
readState = lift ask >>= liftIO . (? GetCurrentState)

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

{-
renderEventLog :: EventLog -> Html ()
renderEventLog eventLog = do
  h2_ [class_ "mt-5"] "Event Log"
  renderBody eventLog
 where
  renderBody :: EventLog -> Html ()
  renderBody []     = p_ [class_ "text-secondary"] "No history available."
  renderBody events = table_ [class_ "table table-striped"] $ do
    tr_ $ do
      th_ "Event"
      th_ "Tag"
      th_ "Time"
      th_ "Description"
    forM_ events $ \event -> tr_ $ case snd event of
      JobRunning job startedAt -> do
        td_ $ span_ [class_ "text-info"] "Started"
        td_ (toHtml (unTag (deploymentTag job)))
        td_ (toHtml (show startedAt))
        td_ ""
      JobSuccessful job finishedAt -> do
        td_ $ span_ [class_ "text-success"] "Finished"
        td_ (toHtml (unTag (deploymentTag job)))
        td_ (toHtml (show finishedAt))
        td_ ""
      JobFailed job failedAt e -> do
        td_ $ span_ [class_ "text-danger"] "Failed"
        td_ (toHtml (unTag (deploymentTag job)))
        td_ (toHtml (show failedAt))
        td_ [style_ "color: red;"] (toHtml e)
-}

renderDeployCard :: [DeploymentName] -> [Tag] -> DeployState -> Html ()
renderDeployCard deploymentNames tags state = do
  h2_ [class_ "mt-5"] "Current State"
  case state of
    Idle -> do
      p_ [class_ "text-muted"] "Idle"
      div_ [class_ "card"] $ do
        div_ [class_ "card-header"] "New Deploy"
        div_ [class_ "card-body"]
          $ form_ [method_ "post"]
          $ div_ [class_ "row"]
          $ do
            div_ [class_ "col"] $
              select_ [name_ "deployment-name", class_ "form-control"]
                $ forM_ deploymentNames
                $ \(unDeploymentName -> n) ->
                    option_ [value_ (Text.pack n)] (toHtml n)
            div_ [class_ "col"] $
              select_ [name_ "tag", class_ "form-control"]
                $ forM_ tags
                $ \(unTag -> tag) -> option_ [value_ tag] (toHtml tag)
            div_ [class_ "col"] $
              span_ [class_ "input-group-button form-control"] $ input_
                [class_ "btn btn-primary", type_ "submit", value_ "Deploy"]
    Deploying job ->
      p_ [class_ "text-info"]
        $  toHtml
        $  "Deploying tag "
        <> unTag (deploymentTag job)
        <> "..."

showAllTagsAction :: Action ()
showAllTagsAction = do
  deployer        <- lift ask
  deploymentNames <- liftIO $ deployer ? GetDeploymentNames
  tags            <- liftIO $ deployer ? GetTags
  deployState     <- liftIO $ deployer ? GetCurrentState
  renderLayout "Lodjur Deployment Manager" $ container_ $ do
    div_ [class_ "row"] $ div_ [class_ "col"] $ do
      h1_ [class_ "mt-5"] "Lodjur"
      p_  [class_ "lead"] "Mpowered's Nixops Deployment Frontend"
    div_ [class_ "row"] $ div_ [class_ "col"] $ renderDeployCard
      deploymentNames
      tags
      deployState
    -- div_ [class_ "row"] $ div_ [class_ "col"] $ renderEventLog eventLog

deployTagAction :: Action ()
deployTagAction = readState >>= \case
  Idle -> do
    deployer <- lift ask
    dName    <- DeploymentName <$> param "deployment-name"
    tag      <- Tag <$> param "tag"
    status status302
    setHeader "Location" "/"
    void $ liftIO $ deployer ? Deploy dName tag
  Deploying job ->
    renderLayout "Already Deploying"
      $  p_
      $  toHtml
      $  "Already deploying a tag: "
      <> unTag (deploymentTag job)

type Port = Int

runServer :: Port -> Ref Deployer -> IO ()
runServer port ref = scottyT port (`runReaderT` ref) $ do
  get  "/" showAllTagsAction
  post "/" deployTagAction
