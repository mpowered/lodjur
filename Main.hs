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
import Network.HTTP.Types.Status
import System.Process

type DeployHistory = [DeployEvent]

data DeployEvent = DeployEvent Tag DeployResult

data DeployState = Idle | Deploying Tag

data DeployResult = DeploySuccess | DeployError Text

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

deployTag :: Tag -> Action ()
deployTag tag = do
  var <- lift $ asks lodjurStateVar
  liftIO $ void $ forkFinally gitDeploy (notify var)
 where
  gitDeploy = threadDelay (5 * us)

  notify var (Left ex) = do
    LodjurState _state history <- takeMVar var
    putMVar var $ LodjurState
      Idle
      (DeployEvent tag (DeployError (Text.pack (show ex))) : history)

  notify var (Right ()) = do
    LodjurState _state history <- takeMVar var
    putMVar var $ LodjurState Idle (DeployEvent tag DeploySuccess : history)

readState :: Action LodjurState
readState = do
  var <- lift $ asks lodjurStateVar
  liftIO (readMVar var)

modifyState :: (LodjurState -> Action LodjurState) -> Action ()
modifyState f = do
  var   <- lift $ asks lodjurStateVar
  state <- liftIO (takeMVar var)
  liftIO . putMVar var =<< f state

type Scotty = ScottyT Lazy.Text (ReaderT LodjurEnv IO)
type Action = ActionT Lazy.Text (ReaderT LodjurEnv IO)

renderHtml :: Html () -> Action ()
renderHtml = html . Html.renderText

showAllTagsAction :: Action ()
showAllTagsAction = do
  LodjurState deployState history <- readState
  stateHtml                       <- renderState deployState
  renderHtml $ do
    h1_ "Lodjur Deployment Manager"
    hr_ []
    div_ stateHtml
    hr_ []
    renderHistory history
 where
  renderState = \case
    Idle -> do
      tags <- listTags
      return $ do
        form_ [method_ "post"] $ do
          select_ [name_ "tag"] $ forM_ tags $ \tag ->
            option_ [value_ tag] (toHtml tag)
          input_ [type_ "submit", value_ "Deploy"]
    Deploying tag -> return $ do
      p_ $ toHtml $ "Deploying tag: " <> tag
  renderHistory :: DeployHistory -> Html ()
  renderHistory results = table_ $ do
    tr_ $ do
      th_ "Tag"
      th_ "Result"
    forM_ results $ \(DeployEvent tag result) -> tr_ $ do
      td_ (toHtml tag)
      case result of
        DeploySuccess -> td_ [style_ "color: green;"] "Success"
        DeployError e -> td_ [style_ "color: red;"] ("Error: " <> toHtml e)

deployTagAction :: Action ()
deployTagAction = modifyState $ \case
  LodjurState Idle history -> do
    tag <- param "tag" :: Action Tag
    status status302
    setHeader "Location" "/"
    deployTag tag
    return $ LodjurState (Deploying tag) history
  LodjurState (Deploying tag) history -> do
    renderHtml $ p_ $ toHtml $ "Already deploying a tag: " <> tag
    return $ LodjurState Idle history

main :: IO ()
main = do
  mvar <- newMVar (LodjurState Idle [])
  qsem <- newQSem 4
  scottyT 4000 (flip runReaderT (LodjurEnv mvar qsem)) $ do
    get  "/" showAllTagsAction
    post "/" deployTagAction
