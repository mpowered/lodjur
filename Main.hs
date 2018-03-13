{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

import Data.Semigroup
import Web.Scotty.Trans
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.Text as Text
import           Lucid.Base (Html, toHtml)
import qualified Lucid.Base as Html
import Lucid.Html5
import Lucid.Bootstrap
import Network.HTTP.Types.Status
import System.Process
import Data.Time.Clock

type DeployHistory = [DeployEvent]

data DeployEvent
  = DeployStarted Tag UTCTime
  | DeployFinished Tag UTCTime
  | DeployFailed Tag UTCTime Text

data DeployState = Idle | Deploying Tag

data LodjurState = LodjurState DeployState DeployHistory

data LodjurEnv = LodjurEnv
  { lodjurStateVar :: MVar LodjurState
  , lodjurGitSem   :: QSem
  }

type Tag = Text

listTags :: Action [Tag]
listTags = do
  qsem <- lift $ asks lodjurGitSem
  liftIO $ bracket_ (waitQSem qsem) (signalQSem qsem) gitListTags

us :: Int
us = 1000000

gitListTags :: IO [Tag]
gitListTags = do
  threadDelay (1 * us)
  out <- readProcess "git" ["tag", "-l"] ""
  return $ filter (not . Text.null) $ Text.lines $ Text.pack out

readState :: Action LodjurState
readState = do
  var <- lift $ asks lodjurStateVar
  liftIO (readMVar var)

modifyState :: MVar LodjurState -> (LodjurState -> IO LodjurState) -> IO ()
modifyState var f = do
  state <- takeMVar var
  putMVar var =<< f state

addEvent :: MVar LodjurState -> DeployEvent -> IO ()
addEvent var event =
  modifyState var $ \(LodjurState state history) ->
    return (LodjurState state (event : history))

deployTag :: Tag -> Action ()
deployTag tag = do
  var <- lift $ asks lodjurStateVar
  liftIO $ do
    now <- getCurrentTime
    addEvent var (DeployStarted tag now)
    void $ forkFinally gitDeploy (notify var)
 where
  gitDeploy = threadDelay (5 * us)

  notify var (Left ex) = do
    now <- getCurrentTime
    addEvent var (DeployFailed tag now (Text.pack (show ex)))

  notify var (Right ()) = do
    now <- getCurrentTime
    addEvent var (DeployFinished tag now)

-- type Scotty = ScottyT Lazy.Text (ReaderT LodjurEnv IO)
type Action = ActionT Lazy.Text (ReaderT LodjurEnv IO)

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

renderHistory :: DeployHistory -> Html ()
renderHistory history = do
  h2_ [class_ "mt-5"] "History"
  renderBody history
 where
  renderBody :: DeployHistory -> Html ()
  renderBody []     = p_ [class_ "text-secondary"] "No history available."
  renderBody events = table_ [class_ "table table-striped"] $ do
    tr_ $ do
      th_ "Event"
      th_ "Tag"
      th_ "Time"
      th_ "Description"
    forM_ events $ \event -> tr_ $ case event of
      DeployStarted tag startedAt -> do
        td_ $ span_ [class_ "text-info"] "Started"
        td_ (toHtml tag)
        td_ (toHtml (show startedAt))
        td_ ""
      DeployFinished tag finishedAt -> do
        td_ $ span_ [class_ "text-success"] "Finished"
        td_ (toHtml tag)
        td_ (toHtml (show finishedAt))
        td_ ""
      DeployFailed tag failedAt e -> do
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
      div_ [class_ "card-body"] $ form_ [method_ "post"] $ do
        div_ [class_ "input-group"] $ do
          select_ [name_ "tag", class_ "form-control"] $ forM_ tags $ \tag ->
            option_ [value_ tag] (toHtml tag)
          span_ [class_ "input-group-button"] $ input_
            [class_ "btn btn-primary", type_ "submit", value_ "Deploy"]
    Deploying tag ->
      p_ [class_ "text-info"] $ toHtml $ "Deploying tag " <> tag <> "..."

showAllTagsAction :: Action ()
showAllTagsAction = do
  LodjurState deployState history <- readState
  tags                            <- listTags
  renderLayout "Lodjur Deployment Manager" $ container_ $ do
    div_ [class_ "row"] $ div_ [class_ "col"] $ do
      h1_ [class_ "mt-5"] "Lodjur"
      p_  [class_ "lead"] "Mpowered's Nixops Deployment Frontend"
    div_ [class_ "row"] $ div_ [class_ "col"] $ renderDeployCard tags
                                                                 deployState
    div_ [class_ "row"] $ div_ [class_ "col"] $ renderHistory history

deployTagAction :: Action ()
deployTagAction = readState >>= \case
  LodjurState Idle _ -> do
    tag <- param "tag" :: Action Tag
    status status302
    setHeader "Location" "/"
    deployTag tag
  LodjurState (Deploying tag) _ ->
    renderLayout "Already Deploying"
      $  p_
      $  toHtml
      $  "Already deploying a tag: "
      <> tag

main :: IO ()
main = do
  mvar <- newMVar (LodjurState Idle [])
  qsem <- newQSem 4
  scottyT 4000 (`runReaderT` LodjurEnv mvar qsem) $ do
    get  "/" showAllTagsAction
    post "/" deployTagAction
