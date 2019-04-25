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
import           Data.Int                      (Int32)
import qualified Data.List                     as List
import           Data.Maybe
import           Data.Ord
import           Data.String.Conversions
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
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
      deferredScript "/static/job.js"
      body_ $
        div_ [class_ "container-fluid"]
          content

home :: AppM (Html ())
home = do
  jobs <- runDb $ recentJobs 10
  return $ lpage "Jobs" $
    div_ [id_ "jobs"] $
      renderJobs jobs

job :: Int32 -> AppM (Html ())
job jobid = do
  j <- runDb $ lookupJob jobid
  return $ lpage "Job" $ do
    div_ [id_ "job", data_ "job-id" (viaShow jobid)] $
      maybe (p_ "Job not found") renderJob j
    div_ [id_ "logs", data_ "job-id" (viaShow jobid)] ""

viaShow :: Show a => a -> Text
viaShow = Text.pack . show

lookupJob :: Int32 -> Pg (Maybe Job)
lookupJob jobid =
  runSelectReturningOne
    $ select
      $ filter_ (\j -> jobId j ==. val_ jobid)
      $ all_ (dbJobs db)

recentJobs :: Integer -> Pg (Forest Job)
recentJobs n = do
  roots <- recentRoots n
  mapM jobTree roots

recentRoots :: Integer -> Pg [Job]
recentRoots n =
  runSelectReturningList
    $ select
      $ limit_ n
      $ orderBy_ (desc_ . jobId)
      $ filter_ (\j -> jobParent j ==. val_ (JobKey Nothing))
      $ all_ (dbJobs db)

jobTree :: Job -> Pg (Tree Job)
jobTree p = do
  children <- runSelectReturningList
                $ select
                  $ orderBy_ (asc_ . jobId)
                $ filter_ (\j -> jobParent j ==. val_ (JobKey (Just (jobId p))))
                  $ all_ (dbJobs db)
  childForest <- mapM jobTree children
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
      -- div_ [class_ "col-4 card-text"] (toHtml $ jobSrcOwner <> " / " <> jobSrcRepo <> " / " <> fromMaybe jobSrcSha jobSrcBranch)
      div_ [class_ "col-4 card-text"] (toHtml $ show jobCommit)
      div_ [class_ "col-1 card-text"] $
        a_ [href_ ("/job/" <> cs (show jobId))] (toHtml $ show jobId)
      -- div_ [class_ "col-1 card-text"] (toHtml $ fromMaybe "" jobSrcCommitter)
      -- div_ [class_ "col-3 card-text"] (toHtml $ fromMaybe "" jobSrcMessage)
      div_ [class_ "col-1 card-text"] "committer"
      div_ [class_ "col-3 card-text"] "message"
      case fromJSON jobAction of
        Success (Job.Build False) -> div_ [class_ "col-1 card-text"] "Build"
        Success (Job.Build True)  -> div_ [class_ "col-1 card-text"] "Build and Check"
        Success (Job.Check x)     -> div_ [class_ "col-1 card-text"] (toHtml $ "Check " <> x)
        _                         -> div_ [class_ "col-1 card-text"] ""
