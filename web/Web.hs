{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Web where

import           Data.Int                       ( Int32 )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Lucid
import           Servant
import           Servant.HTML.Lucid

import           Types

type Web
    = Get '[HTML] (Html ())
 :<|> "job" :> Capture "jobid" Int32 :> Get '[HTML] (Html ())

web :: ServerT Web AppM
web
    = home
 :<|> job

deferredScript :: Text -> Html ()
deferredScript src =
  script_ [src_ src, defer_ "defer"] ("" :: Text)

lpage :: Html () -> Html () -> Html ()
lpage title content =
  doctypehtml_ $ html_ $
    head_ $ do
      title_ title
      link_ [rel_ "stylesheet", href_ "/static/bootstrap/css/bootstrap.min.css"]
      link_ [rel_ "stylesheet", href_ "/static/lodjur.css"]
      deferredScript "/static/jquery-3.0.0.slim.min.js"
      deferredScript "/static/bootstrap/js/bootstrap.bundle.min.js"
      deferredScript "/js/api.js"
      deferredScript "/static/job.js"
      body_ $
        div_ [class_ "container-fluid"]
          content

home :: AppM (Html ())
home =
  return $ lpage "Jobs" $
    div_ [id_ "recent-jobs"] mempty

job :: Int32 -> AppM (Html ())
job jobid =
  return $ lpage "Job" $ do
    div_ [id_ "job", data_ "job-id" (viaShow jobid)] mempty
    div_ [id_ "logs", data_ "job-id" (viaShow jobid)] mempty

viaShow :: Show a => a -> Text
viaShow = Text.pack . show
