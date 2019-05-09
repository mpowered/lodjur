{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module Job where

import qualified Data.Aeson                    as A
import qualified Data.Char                     as C
import           Data.Int                       ( Int32 )
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           GHC.Generics
import           Lucid

import           Lodjur.Database         hiding ( div_ )
import           Lodjur.Database.Enum
import           Lodjur.Job

data Job'
  = Job'
    { job'Id                    :: Int32
    , job'Name                  :: Text
    , job'Action                :: Action
    , job'Status                :: Status
    , job'Conclusion            :: Maybe Conclusion
    , job'CreatedAt             :: UTCTime
    , job'StartedAt             :: Maybe UTCTime
    , job'CompletedAt           :: Maybe UTCTime
    , job'Parent                :: Maybe Int32
    , job'CommitSha             :: Text
    , job'CommitOwner           :: Text
    , job'CommitRepo            :: Text
    , job'CommitBranch          :: Maybe Text
    , job'CommitMessage         :: Maybe Text
    , job'CommitAuthor          :: Maybe Text
    , job'CommitAuthorEmail     :: Maybe Text
    , job'CommitCommitter       :: Maybe Text
    , job'CommitCommitterEmail  :: Maybe Text
    , job'CommitTimestamp       :: Maybe UTCTime
    }
  deriving (Show, Generic)

instance A.ToJSON Job' where
  toJSON = A.genericToJSON options

instance A.FromJSON Job' where
  parseJSON = A.genericParseJSON options

data LogLine
  = LogLine
    { log'Text                  :: Text
    , log'Timestamp             :: UTCTime
    }
  deriving (Show, Generic)

instance A.ToJSON LogLine where
  toJSON = A.genericToJSON options

instance A.FromJSON LogLine where
  parseJSON = A.genericParseJSON options

options :: A.Options
options = A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_' . dropWhile (not . C.isUpper) }

instance ToHtml Job' where
  toHtmlRaw = toHtml
  toHtml Job'{..} =
    div_ [class_ "card-body p-0"] $
      div_ [class_ "row m-0 p-1"] $ do
        case job'Status of
          Queued     -> div_ [class_ "col-1 badge badge-secondary"]   "Queued"
          InProgress -> div_ [class_ "col-1 badge badge-primary"] "In Progress"
          Completed  ->
            case job'Conclusion of
              Just Success   -> div_ [class_ "col-1 badge badge-success"]   "Success"
              Just Failure   -> div_ [class_ "col-1 badge badge-danger"]    "Failure"
              Just Cancelled -> div_ [class_ "col-1 badge badge-warning"]   "Cancelled"
              Just Neutral   -> div_ [class_ "col-1 badge badge-info"]      "Neutral"
              _              -> div_ [class_ "col-1 badge badge-warning"]   "Complete"
        div_ [class_ "col-1 card-text"] (toHtml job'Name)
        div_ [class_ "col-4 card-text"] (toHtml $ job'CommitOwner <> " / " <> job'CommitRepo <> " / " <> fromMaybe job'CommitSha job'CommitBranch)
        div_ [class_ "col-1 card-text"] $
          a_ [href_ ("/job/" <> cs (show job'Id))] (toHtml $ show job'Id)
        div_ [class_ "col-1 card-text"] (toHtml $ fromMaybe "" job'CommitAuthor)
        div_ [class_ "col-3 card-text"] (toHtml $ fromMaybe "" job'CommitMessage)
        case job'Action of
          Build False -> div_ [class_ "col-1 card-text"] "Build"
          Build True  -> div_ [class_ "col-1 card-text"] "Build and Check"
          Check x     -> div_ [class_ "col-1 card-text"] (toHtml $ "Check " <> x)
          Deploy x    -> div_ [class_ "col-1 card-text"] (toHtml $ "Deploy " <> x)

instance ToHtml [Job'] where
  toHtmlRaw = toHtml
  toHtml jobs =
    div_ $
      mapM_ toHtml jobs

instance ToHtml (Maybe Job') where
  toHtmlRaw = toHtml
  toHtml (Just job) = toHtml job
  toHtml Nothing = div_ ""

job' :: Job -> Commit -> Job'
job' Job{..} Commit{..} =
  Job'
    { job'Id                    = jobId
    , job'Name                  = jobName
    , job'Action                = fromJSON undefined jobAction
    , job'Status                = unDbEnum jobStatus
    , job'Conclusion            = unDbEnum <$> jobConclusion
    , job'CreatedAt             = jobCreatedAt
    , job'StartedAt             = jobStartedAt
    , job'CompletedAt           = jobCompletedAt
    , job'Parent                = getJobId jobParent
    , job'CommitSha             = commitSha
    , job'CommitOwner           = commitOwner
    , job'CommitRepo            = commitRepo
    , job'CommitBranch          = commitBranch
    , job'CommitMessage         = commitMessage
    , job'CommitAuthor          = commitAuthor
    , job'CommitAuthorEmail     = commitAuthorEmail
    , job'CommitCommitter       = commitCommitter
    , job'CommitCommitterEmail  = commitCommitterEmail
    , job'CommitTimestamp       = commitTimestamp
    }
 where
  fromJSON d v =
    case A.fromJSON v of
      A.Success r -> r
      A.Error _   -> d
  getJobId (JobKey i) = i

instance ToHtml LogLine where
  toHtmlRaw = toHtml
  toHtml LogLine{..} = div_ $ toHtml log'Text

instance ToHtml [LogLine] where
  toHtmlRaw = toHtml
  toHtml ls =
    div_ $
      mapM_ toHtml ls

recentRoots :: Integer -> Pg [Job']
recentRoots n = do
  ps <- runSelectReturningList $
    select $ do
      j <- limit_ n $ orderBy_ (desc_ . jobId) $ filter_ (\j -> jobParent j ==. val_ (JobKey Nothing)) $ all_ (dbJobs db)
      c <- all_ (dbCommits db)
      guard_ (jobCommit j `references_` c)
      pure (j, c)
  return (uncurry job' <$> ps)

lookupJob :: Int32 -> Pg (Maybe Job')
lookupJob jobid = do
  p <- runSelectReturningOne $
    select $ do
      j <- filter_ (\j -> jobId j ==. val_ jobid) $ all_ (dbJobs db)
      c <- all_ (dbCommits db)
      guard_ (jobCommit j `references_` c)
      pure (j, c)
  return (uncurry job' <$> p)

jobChildren :: Int32 -> Pg [Job']
jobChildren jobid = do
  ps <- runSelectReturningList $
    select $ do
      j <- filter_ (\j -> jobParent j ==. val_ (JobKey (Just jobid))) $ all_ (dbJobs db)
      c <- all_ (dbCommits db)
      guard_ (jobCommit j `references_` c)
      pure (j, c)
  return (uncurry job' <$> ps)

jobLogsTail :: Int32 -> Pg [LogLine]
jobLogsTail jobid = do
  ls <- runSelectReturningList $
    select $
    limit_ 20 $
    orderBy_ (desc_ . logCreatedAt) $
    filter_ (\l -> logJob l ==. val_ (JobKey jobid)) $
    all_ (dbLogs db)
  return $ reverse $ map (\Log{..} -> LogLine logText logCreatedAt) ls
