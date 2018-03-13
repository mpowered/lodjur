{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

import Data.Semigroup
import Web.Scotty.Trans
import Control.Concurrent
import Control.Monad
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Lucid.Base (Html, toHtml)
import qualified Lucid.Base as Html
import Lucid.Html5

renderHtml :: Html () -> Action ()
renderHtml = html . Html.renderText

data LodjurState = Idle | Deploying

type LodjurEnv = MVar LodjurState

type Scotty = ScottyT Lazy.Text (ReaderT LodjurEnv IO)
type Action = ActionT Lazy.Text (ReaderT LodjurEnv IO)

type Tag = Text

listTags :: IO [Tag]
listTags = return ["foo", "bar", "baz"]

showAllTags :: Action ()
showAllTags = do
  tags <- liftIO listTags
  renderHtml $ do
    h1_ "Lodjur Deployment Manager"
    form_ [method_ "post"] $ do
      select_ [name_ "tag"] $ forM_ tags $ \tag ->
        option_ [value_ tag] (toHtml tag)
      input_ [type_ "submit", value_ "Deploy"]

modifyState :: (LodjurState -> Action LodjurState) -> Action ()
modifyState f = do
  var <- lift ask
  state <- liftIO (takeMVar var)
  liftIO . putMVar var =<< f state

deployTag :: Action ()
deployTag = do
  tag <- param "tag"
  modifyState $
    \case
      Idle -> do
        renderHtml $ do
          p_ $ toHtml $ "Deploying tag " <> (tag :: Tag)
        return Deploying
      Deploying -> do
        renderHtml $ do
          p_ "Already deploying a tag."
        return Idle

main :: IO ()
main = do
  mvar <- newMVar Idle
  scottyT 4000 (flip runReaderT mvar) $ do
    get "/" showAllTags
    post "/" deployTag
