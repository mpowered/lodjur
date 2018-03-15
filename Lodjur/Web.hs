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

renderHistory :: EventLog -> Html ()
renderHistory history = do
  h2_ [class_ "mt-5"] "History"
  renderBody history
 where
  renderBody :: EventLog -> Html ()
  renderBody []     = p_ [class_ "text-secondary"] "No history available."
  renderBody events = table_ [class_ "table table-striped"] $ do
    tr_ $ do
      th_ "Event"
      th_ "Tag"
      th_ "Time"
      th_ "Description"
    forM_ events $ \event -> tr_ $ case event of
      JobRunning (unTag -> tag) startedAt -> do
        td_ $ span_ [class_ "text-info"] "Started"
        td_ (toHtml tag)
        td_ (toHtml (show startedAt))
        td_ ""
      JobSuccessful (unTag -> tag) finishedAt -> do
        td_ $ span_ [class_ "text-success"] "Finished"
        td_ (toHtml tag)
        td_ (toHtml (show finishedAt))
        td_ ""
      JobFailed (unTag -> tag) failedAt e -> do
        td_ $ span_ [class_ "text-danger"] "Failed"
        td_ (toHtml tag)
        td_ (toHtml (show failedAt))
        td_ [style_ "color: red;"] (toHtml e)

renderDeployCard :: [Tag] -> DeployState -> Html ()
renderDeployCard tags state = do
  h2_ [class_ "mt-5"] "Current State"
  case state of
    Idle -> div_ [class_ "card"] $ do
      div_ [class_ "card-header"] "New Deploy"
      div_ [class_ "card-body"]
        $ form_ [method_ "post"]
        $ div_ [class_ "input-group"]
        $ do
            select_ [name_ "tag", class_ "form-control"] $ forM_ tags $ \(unTag -> tag) ->
              option_ [value_ tag] (toHtml tag)
            span_ [class_ "input-group-button"] $ input_
              [class_ "btn btn-primary", type_ "submit", value_ "Deploy"]
    Deploying tag ->
      p_ [class_ "text-info"] $ toHtml $ "Deploying tag " <> unTag tag <> "..."

showAllTagsAction :: Action ()
showAllTagsAction = do
  deployer                        <- lift ask
  tags                            <- liftIO $ deployer ! GetTags
  renderLayout "Lodjur Deployment Manager" $ container_ $ do
    div_ [class_ "row"] $ div_ [class_ "col"] $ do
      h1_ [class_ "mt-5"] "Lodjur"
      p_  [class_ "lead"] "Mpowered's Nixops Deployment Frontend"
    div_ [class_ "row"] $ div_ [class_ "col"] $ renderDeployCard tags
                                                                 deployState
    div_ [class_ "row"] $ div_ [class_ "col"] $ renderHistory history

deployTagAction :: Action ()
deployTagAction = readState >>= \case
  Idle _ -> do
    deployer <- lift ask
    tag <- Tag <$> param "tag"
    status status302
    setHeader "Location" "/"
    liftIO $ deployer ! Deploy tag
  Deploying job ->
    renderLayout "Already Deploying"
      $  p_
      $  toHtml
      $  "Already deploying a tag: "
      <> unTag (deploymentTag job)

type Port = Int

runServer :: Port -> Ref Deployer -> IO ()
runServer port ref =
  scottyT port (`runReaderT` ref) $ do
    get  "/" showAllTagsAction
    post "/" deployTagAction
