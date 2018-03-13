{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup
import Web.Scotty.Trans
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

type LodjurEnv = ()

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

deployTag :: Action ()
deployTag = do
  tag <- param "tag"
  renderHtml $ do
    p_ $ toHtml $ "Deploying tag " <> (tag :: Tag)

main :: IO ()
main = scottyT 4000 (flip runReaderT ()) $ do
  get "/" showAllTags
  post "/" deployTag
