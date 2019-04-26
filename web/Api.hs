{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Api where

import qualified Data.Aeson                    as A
import           Data.Int (Int32)
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text (Text)
import qualified Data.Text.Lazy as Text
import           Data.Time.Clock                ( UTCTime )
import           Lucid
import           Servant
import           Servant.HTML.Lucid
import           Servant.JS

import           Lodjur.Database               hiding (div_)
import           Lodjur.Database.Enum
import           Lodjur.Job
import           Types

type Api = "api" :>
 (    "recent-jobs" :> Get '[JSON] [Int32]
 :<|> "job" :> Capture "jobId" Int32 :> Get '[JSON, HTML] LJob
 )

apijs :: AppM Text
apijs = return $ jsForAPI (Proxy :: Proxy Api) jquery

api :: ServerT Api AppM
api
    = recentJobs
 :<|> job

recentJobs :: AppM [Int32]
recentJobs = do
  js <- runDb $ recentRoots 20
  return $ map jobId js

job :: Int32 -> AppM LJob
job jobid = do
  j <- runDb $ lookupJob jobid
  case j of
    Just (Job{..}, Commit{..}) -> return
      LJob
        { ljId                     = jobId
        , ljName                   = jobName
        , ljAction                 = fromJSON undefined jobAction
        , ljStatus                 = unDbEnum jobStatus
        , ljConclusion             = unDbEnum <$> jobConclusion
        , ljCreatedAt              = jobCreatedAt
        , ljStartedAt              = jobStartedAt
        , ljCompletedAt            = jobCompletedAt
        , ljParent                 = getJobId jobParent
        , ljCommitSha              = commitSha
        , ljCommitOwner            = commitOwner
        , ljCommitRepo             = commitRepo
        , ljCommitBranch           = commitBranch
        , ljCommitMessage          = commitMessage
        , ljCommitAuthor           = commitAuthor
        , ljCommitAuthorEmail      = commitAuthorEmail
        , ljCommitCommitter        = commitCommitter
        , ljCommitCommitterEmail   = commitCommitterEmail
        , ljCommitTimestamp        = commitTimestamp
        }
    Nothing -> throwError err404
 where
  fromJSON d v =
    case A.fromJSON v of
      A.Success r -> r
      A.Error _   -> d
  getJobId (JobKey i) = i

data LJob
  = LJob
    { ljId                     :: Int32
    , ljName                   :: Text
    , ljAction                 :: Action
    , ljStatus                 :: Status
    , ljConclusion             :: Maybe Conclusion
    , ljCreatedAt              :: UTCTime
    , ljStartedAt              :: Maybe UTCTime
    , ljCompletedAt            :: Maybe UTCTime
    , ljParent                 :: Maybe Int32
    , ljCommitSha              :: Text
    , ljCommitOwner            :: Text
    , ljCommitRepo             :: Text
    , ljCommitBranch           :: Maybe Text
    , ljCommitMessage          :: Maybe Text
    , ljCommitAuthor           :: Maybe Text
    , ljCommitAuthorEmail      :: Maybe Text
    , ljCommitCommitter        :: Maybe Text
    , ljCommitCommitterEmail   :: Maybe Text
    , ljCommitTimestamp        :: Maybe UTCTime
    }
  deriving (Show, Generic, A.ToJSON, A.FromJSON)

instance ToHtml LJob where
  toHtmlRaw = toHtml
  toHtml LJob{..} =
    div_ [class_ "card-body p-0"] $
      div_ [class_ "row m-0 p-1"] $ do
        case ljStatus of
          Queued     -> div_ [class_ "col-1 badge badge-secondary"]   "Queued"
          InProgress -> div_ [class_ "col-1 badge badge-primary"] "In Progress"
          Completed  ->
            case ljConclusion of
              Just Success   -> div_ [class_ "col-1 badge badge-success"]   "Success"
              Just Failure   -> div_ [class_ "col-1 badge badge-danger"]    "Failure"
              Just Cancelled -> div_ [class_ "col-1 badge badge-warning"]   "Cancelled"
              Just Neutral   -> div_ [class_ "col-1 badge badge-info"]      "Neutral"
              _                  -> div_ [class_ "col-1 badge badge-warning"]   "Complete"
        div_ [class_ "col-1 card-text"] (toHtml ljName)
        div_ [class_ "col-4 card-text"] (toHtml $ ljCommitOwner <> " / " <> ljCommitRepo <> " / " <> fromMaybe ljCommitSha ljCommitBranch)
        div_ [class_ "col-1 card-text"] $
          a_ [href_ ("/job/" <> cs (show ljId))] (toHtml $ show ljId)
        div_ [class_ "col-1 card-text"] (toHtml $ fromMaybe "" ljCommitAuthor)
        div_ [class_ "col-3 card-text"] (toHtml $ fromMaybe "" ljCommitMessage)
        case ljAction of
          Build False -> div_ [class_ "col-1 card-text"] "Build"
          Build True  -> div_ [class_ "col-1 card-text"] "Build and Check"
          Check x     -> div_ [class_ "col-1 card-text"] (toHtml $ "Check " <> x)
          Deploy x    -> div_ [class_ "col-1 card-text"] (toHtml $ "Deploy " <> x)

lookupJob :: Int32 -> Pg (Maybe (Job, Commit))
lookupJob jobid =
  runSelectReturningOne $ select $ do
      j <- filter_ (\j -> jobId j ==. val_ jobid) $ all_ (dbJobs db)
      c <- all_ (dbCommits db)
      guard_ (jobCommit j `references_` c)
      pure (j, c)

recentRoots :: Integer -> Pg [Job]
recentRoots n =
  runSelectReturningList
    $ select
      $ limit_ n
      $ orderBy_ (desc_ . jobId)
      $ filter_ (\j -> jobParent j ==. val_ (JobKey Nothing))
      $ all_ (dbJobs db)

htmlText :: Html a -> Text
htmlText = Text.toStrict . renderText
