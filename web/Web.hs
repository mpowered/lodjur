{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Web where

import           Data.Aeson
import           Control.Monad.Reader
import qualified Data.List                     as List
import           Data.Maybe
import           Data.Ord
import           Data.String.Conversions
import           Data.Text                     (Text)
import           Data.Tree
import           Lucid
import           Servant
import           Servant.HTML.Lucid

import           Lodjur.Database               hiding (div_)
import           Lodjur.Database.Enum
import qualified Lodjur.Job                    as Job

import           Types

type Web
    = Get '[HTML] (Html ())
 :<|> "job" :> Capture "jobid" Int :> Get '[HTML] (Html ())

web :: ServerT Web AppM
web
    = home
 :<|> job

deferredScript :: Text -> Html ()
deferredScript src =
  script_ [src_ src, defer_ "defer"] ("" :: Text)

runQuery :: (Connection -> IO a) -> AppM a
runQuery q = do
  pool <- asks Types.envDbPool
  liftIO $ withConnection pool q

home :: AppM (Html ())
home = do
  jobs <- runQuery $ recentJobs 10
  return $ doctypehtml_ $ html_ $
    head_ $ do
      title_ "Jobs"
      link_ [rel_ "stylesheet", href_ "/static/bootstrap/css/bootstrap.min.css"]
      link_ [rel_ "stylesheet", href_ "/static/lodjur.css"]
      deferredScript "/static/jquery-3.0.0.slim.min.js"
      deferredScript "/static/bootstrap/js/bootstrap.bundle.min.js"
      deferredScript "/static/job.js"
      body_ [class_ "bare-page"] $ div_ [class_ "container-fluid"] $
        div_ [id_ "jobs"] $
          renderJobs jobs

recentJobs :: Integer -> Connection -> IO (Forest Job)
recentJobs n conn = do
  roots <- recentRoots n conn
  mapM (jobTree conn) roots

recentRoots :: Integer -> Connection -> IO [Job]
recentRoots n conn =
  beam conn
    $ runSelectReturningList
    $ select
      $ limit_ n
      $ orderBy_ (desc_ . jobId)
      $ filter_ (\j -> jobParent j ==. val_ (JobKey Nothing))
      $ all_ (dbJobs db)

jobTree :: Connection -> Job -> IO (Tree Job)
jobTree conn p = do
  children <- beam conn
    $ runSelectReturningList
    $ select
      $ orderBy_ (asc_ . jobId)
    $ filter_ (\j -> jobParent j ==. val_ (JobKey (Just (jobId p))))
      $ all_ (dbJobs db)
  childForest <- mapM (jobTree conn) children
  return (Node p childForest)

renderJobs :: Forest Job -> Html ()
renderJobs jobs =
  div_ [class_ "bg-secondary p-3"] $
    mapM_ renderJobTree (sortDesc jobs)
 where
  sortDesc = List.sortOn (Down . jobId . rootLabel)

renderJobTree :: Tree Job -> Html ()
renderJobTree (Node job' children) = do
  let ty = case jobParent job' of
            JobKey (Just _) -> "card p-1 my-0"
            JobKey Nothing  -> "card p-2 my-3"
  div_ [class_ ty] $ do
    renderJob job'
    div_ [class_ "ml-3"] $
      mapM_ renderJobTree (sortAsc children)
 where
  sortAsc = List.sortOn (jobId . rootLabel)

renderJob :: Job -> Html ()
renderJob Job{..} =
  div_ [class_ "card-body p-0"] $
    div_ [class_ "row m-0 p-1"] $ do
      case unDbEnum jobStatus of
        Job.Queued     -> div_ [class_ "col-1 badge badge-secondary"]   "Queued"
        Job.InProgress -> div_ [class_ "col-1 badge badge-primary"] "In Progress"
        Job.Completed  ->
          case unDbEnum <$> jobConclusion of
            Just Job.Success   -> div_ [class_ "col-1 badge badge-success"]   "Success"
            Just Job.Failure   -> div_ [class_ "col-1 badge badge-danger"]    "Failure"
            Just Job.Cancelled -> div_ [class_ "col-1 badge badge-warning"]   "Cancelled"
            Just Job.Neutral   -> div_ [class_ "col-1 badge badge-info"]      "Neutral"
            _                  -> div_ [class_ "col-1 badge badge-warning"]   "Complete"
      div_ [class_ "col-1 card-text"] (toHtml jobName)
      div_ [class_ "col-4 card-text"] (toHtml $ jobSrcOwner <> " / " <> jobSrcRepo <> " / " <> fromMaybe jobSrcSha jobSrcBranch)
      div_ [class_ "col-1 card-text"] $
        a_ [href_ ("/job/" <> cs (show jobId))] (toHtml $ show jobId)
      div_ [class_ "col-1 card-text"] (toHtml $ fromMaybe "" jobSrcCommitter)
      div_ [class_ "col-3 card-text"] (toHtml $ fromMaybe "" jobSrcMessage)
      case fromJSON jobAction of
        Success (Job.Build False) -> div_ [class_ "col-1 card-text"] "Build"
        Success (Job.Build True)  -> div_ [class_ "col-1 card-text"] "Build and Check"
        Success (Job.Check x)     -> div_ [class_ "col-1 card-text"] (toHtml $ "Check " <> x)
        _                         -> div_ [class_ "col-1 card-text"] ""

job :: Int -> AppM (Html ())
job _ = return $
  return ()