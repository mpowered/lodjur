{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE RecursiveDo            #-}

module Job where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                , genericToJSON
                                                , genericParseJSON
                                                )
import qualified Data.Aeson                    as A
import qualified Data.Char                     as C
import           Data.Int                       ( Int32 )
import           Data.List                      ( partition, sortOn )
import           Data.String.Conversions
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time
import           Data.Tree
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

instance ToJSON Job' where
  toJSON = genericToJSON options

instance FromJSON Job' where
  parseJSON = genericParseJSON options

data LogLine
  = LogLine
    { log'Text                  :: Text
    }
  deriving (Show, Generic)

instance ToJSON LogLine where
  toJSON = genericToJSON options

instance FromJSON LogLine where
  parseJSON = genericParseJSON options

options :: A.Options
options = A.defaultOptions
  { A.fieldLabelModifier = A.camelTo2 '_' . dropWhile (not . C.isUpper)
  }

newtype Outline a = Outline a

instance ToJSON a => ToJSON (Outline a) where
  toJSON (Outline a) = toJSON a

instance ToHtml (Outline Job') where
  toHtmlRaw = toHtml
  toHtml (Outline Job' {..}) = div_ [class_ "job", data_ "job-id" (cs $ show job'Id)] $ div_ $ do
    statusIcon job'Status job'Conclusion
    toHtml job'Name

instance ToHtml (Outline (Tree Job')) where
  toHtmlRaw = toHtml
  toHtml (Outline (Node node children)) = div_ [class_ "job-tree"] $ do
    div_ [class_ "job-item"] $ do
      if null children
        then span_ [class_ "outline-symbol fa-fw"] ""
        else span_ [class_ "outline-symbol far fa-fw fa-chevron-down"] ""
      toHtml (Outline node)
    toHtml (Outline children)

instance ToHtml (Outline (Forest Job')) where
  toHtmlRaw = toHtml
  toHtml (Outline jobs) = mapM_ (toHtml . Outline) jobs

icon :: Monad m => Text -> HtmlT m ()
icon a = span_ [ class_ a ] ""

clock_ :: Monad m => HtmlT m ()
clock_ = icon "far fa-fw fa-clock"

stopwatch_ :: Monad m => HtmlT m ()
stopwatch_ = icon "far fa-fw fa-stopwatch"

spinner_ :: Monad m => HtmlT m ()
spinner_ = icon "far fa-fw fa-pulse fa-spinner"

check_ :: Monad m => HtmlT m ()
check_ = icon "success fas fa-fw fa-check"

times_ :: Monad m => HtmlT m ()
times_ = icon "failure fas fa-fw fa-times"

ban_ :: Monad m => HtmlT m ()
ban_ = icon "fas fa-fw fa-ban"

question_ :: Monad m => HtmlT m ()
question_ = icon "fas fa-fw fa-question"

exclamation_ :: Monad m => HtmlT m ()
exclamation_ = icon "fas fa-fw fa-exclamation"

codebranch_ :: Monad m => HtmlT m ()
codebranch_ = icon "far fa-fw fa-code-branch"

codecommit_ :: Monad m => HtmlT m ()
codecommit_ = icon "far fa-fw fa-code-commit"

hashtag_ :: Monad m => HtmlT m ()
hashtag_ = icon "far fa-fw fa-hashtag"

linki_ :: Monad m => HtmlT m ()
linki_ = icon "far fa-fw fa-link"

user_ :: Monad m => HtmlT m ()
user_ = icon "far fa-fw fa-user"

github_ :: Monad m => HtmlT m ()
github_ = icon "fab fa-fw fa-github"

statusIcon :: Monad m => Status -> Maybe Conclusion -> HtmlT m ()
statusIcon status conclusion = case status of
  Queued     -> clock_
  InProgress -> spinner_
  Completed  -> case conclusion of
    Just Success   -> check_
    Just Failure   -> times_
    Just Cancelled -> ban_
    Just Neutral   -> question_
    _              -> exclamation_

statusClass :: Status -> Maybe Conclusion -> Text
statusClass status conclusion = case status of
  Queued     -> "queued"
  InProgress -> "inprogress"
  Completed  -> case conclusion of
    Just Success   -> "success"
    Just Failure   -> "failure"
    Just Cancelled -> "cancelled"
    Just Neutral   -> "neutral"
    _              -> "other"

attr :: (Monad m, ToHtml a) => HtmlT m () -> a -> HtmlT m ()
attr f v = do
  div_ [ class_ "attr-field" ] f
  div_ [ class_ "attr-val" ] (toHtml v)

attrMaybe :: (Monad m, ToHtml a) => HtmlT m () -> Maybe a -> HtmlT m ()
attrMaybe f (Just v) = attr f v
attrMaybe _ Nothing = do
  div_ [ class_ "attr-field" ] ""
  div_ [ class_ "attr-val" ] ""

job' :: Job -> Commit -> Job'
job' Job {..} Commit {..} = Job'
  { job'Id                   = jobId
  , job'Name                 = jobName
  , job'Action               = fromJSON undefined jobAction
  , job'Status               = unDbEnum jobStatus
  , job'Conclusion           = unDbEnum <$> jobConclusion
  , job'CreatedAt            = jobCreatedAt
  , job'StartedAt            = jobStartedAt
  , job'CompletedAt          = jobCompletedAt
  , job'Parent               = getJobId jobParent
  , job'CommitSha            = commitSha
  , job'CommitOwner          = commitOwner
  , job'CommitRepo           = commitRepo
  , job'CommitBranch         = commitBranch
  , job'CommitMessage        = commitMessage
  , job'CommitAuthor         = commitAuthor
  , job'CommitAuthorEmail    = commitAuthorEmail
  , job'CommitCommitter      = commitCommitter
  , job'CommitCommitterEmail = commitCommitterEmail
  , job'CommitTimestamp      = commitTimestamp
  }
 where
  fromJSON d v = case A.fromJSON v of
    A.Success r -> r
    A.Error   _ -> d
  getJobId (JobKey i) = i

instance ToHtml LogLine where
  toHtmlRaw = toHtml
  toHtml LogLine {..} = div_ (toHtml log'Text)

instance ToHtml [LogLine] where
  toHtmlRaw = toHtml
  toHtml ls = mapM_ toHtml ls

recentRoots :: Integer -> Pg [Job']
recentRoots n = do
  ps <- runSelectReturningList $ select $ do
    j <-
      limit_ n
      $ orderBy_ (desc_ . jobId)
      $ filter_ (\j -> jobParent j ==. val_ nothing_)
      $ all_ (dbJobs db)
    c <- related_ (dbCommits db) (jobCommit j)
    pure (j, c)
  return (uncurry job' <$> ps)

recentJobsForest :: Integer -> Pg (Forest Job')
recentJobsForest n = do
  ps <- runSelectReturningList $ selectWith $ do
    rec prev <-
          selecting
          $        ( limit_ n
                   $ orderBy_ (desc_ . jobId)
                   $ filter_ (\j -> jobParent j ==. val_ nothing_)
                   $ all_ (dbJobs db)
                   )
          `union_` (do
                     parent <- reuse prev
                     relatedBy_ (dbJobs db)
                                (\j -> jobParent j ==. just_ (pk parent))
                   )
    pure $ do
      j <- reuse prev
      c <- related_ (dbCommits db) (jobCommit j)
      pure (j, c)
  return $ reverse $ buildForest Nothing (uncurry job' <$> ps)
 where
  buildForest _ [] = []
  buildForest p js =
    let (roots, js') = partition (\j -> job'Parent j == p) js
        sorted = sortOn job'Id roots
    in  map (\j -> Node j (buildForest (Just (job'Id j)) js')) sorted

lookupJob :: Int32 -> Pg (Maybe Job')
lookupJob jobid = do
  p <- runSelectReturningOne $ select $ do
    j <- filter_ (\j -> jobId j ==. val_ jobid) $ all_ (dbJobs db)
    c <- all_ (dbCommits db)
    guard_ (jobCommit j `references_` c)
    pure (j, c)
  return (uncurry job' <$> p)

jobChildren :: Int32 -> Pg [Job']
jobChildren jobid = do
  ps <- runSelectReturningList $ select $ do
    j <- filter_ (\j -> jobParent j ==. val_ (JobKey (Just jobid)))
      $ all_ (dbJobs db)
    c <- all_ (dbCommits db)
    guard_ (jobCommit j `references_` c)
    pure (j, c)
  return (uncurry job' <$> ps)

jobLogLines :: Int32 -> Pg [LogLine]
jobLogLines jobid = do
  ls <-
    runSelectReturningList
    $ select
    $ orderBy_ (asc_ . logCreatedAt)
    $ filter_ (\l -> logJob l ==. val_ (JobKey jobid))
    $ all_ (dbLogs db)
  return $ map (\Log {..} -> LogLine logText) ls

data CardSize = LargeCard | SmallCard

data Card a = Card CardSize a

instance ToJSON a => ToJSON (Card a) where
  toJSON (Card _ a) = toJSON a

instance ToHtml (Card Job') where
  toHtmlRaw = toHtml
  toHtml (Card LargeCard Job' {..}) =
    div_ [class_ "card", data_ "job-id" (cs $ show job'Id) ] $ do
      div_ [ class_ ("card-trim left status-background " <> statusClass job'Status job'Conclusion) ] ""
      div_ [ class_ "card-col narrow" ] $ do
        div_ [ class_ "card-row" ] $
          attr (statusIcon job'Status job'Conclusion) job'Name
        div_ [ class_ "card-row" ] $
          attr linki_ (show job'Id)
        div_ [ class_ "card-row" ] $
          attrMaybe codebranch_ job'CommitBranch
        div_ [ class_ "card-row" ] $
          attr codecommit_ (Text.take 10 job'CommitSha)
      div_ [ class_ "card-col wide" ] $ do
        div_ [ class_ "card-row" ] $
          attrMaybe clock_ (prettyTime <$> job'StartedAt)
        div_ [ class_ "card-row" ] $
          attrMaybe stopwatch_ (prettyDuration <$> job'StartedAt <*> pure job'CompletedAt)
        div_ [ class_ "card-row" ] $
          attr github_ (job'CommitOwner <> "/" <> job'CommitRepo)
        div_ [ class_ "card-row" ] $
          attrMaybe user_ (job'CommitCommitter <|> job'CommitCommitterEmail)
      div_ [ class_ "card-trim right" ] ""

  toHtml (Card SmallCard Job' {..}) =
    div_ [class_ "card",  data_ "job-id" (cs $ show job'Id) ] $ do
      div_ [ class_ ("card-trim left status-background " <> statusClass job'Status job'Conclusion) ] ""
      div_ [ class_ "card-col narrow" ] $ do
        div_ [ class_ "card-row" ] $
          attr (statusIcon job'Status job'Conclusion) job'Name
        div_ [ class_ "card-row" ] $
          attr linki_ (show job'Id)
      div_ [ class_ "card-col" ] $ do
        div_ [ class_ "card-row" ] $
          attrMaybe clock_ (prettyTime <$> job'StartedAt)
        div_ [ class_ "card-row" ] $
          attrMaybe stopwatch_ (prettyDuration <$> job'StartedAt <*> pure job'CompletedAt)
      div_ [ class_ "card-trim right" ] ""

instance ToHtml (Card (Tree Job')) where
  toHtmlRaw = toHtml
  toHtml (Card sz (Node job children)) = do
    if null children
      then
        div_ [ class_ "card-item" ] $
          toHtml (Card sz job)
      else do
        div_ [ class_ "card-item" ] $ do
          toHtml (Card sz job)
          div_ [class_ "card-sublist"] $
            mapM_ (toHtml . Card SmallCard . rootLabel) children

instance ToHtml (Card (Forest Job')) where
  toHtmlRaw = toHtml
  toHtml (Card sz jobs) = mapM_ (toHtml . Card sz) jobs

prettyTime :: Monad m => UTCTime -> HtmlT m ()
prettyTime t =
  div_ [class_ "time"] $ do
    div_ [class_ "utctime"] $
      toHtml (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t)
    div_ [class_ "time-pretty"] $
      toHtml (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t)

prettyDuration :: Monad m => UTCTime -> Maybe UTCTime -> HtmlT m ()
prettyDuration start end = do
  div_ [class_ "duration"] $ do
    div_ [class_ "duration-start"] $ 
      toHtml (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" start)
    forM_ end $ \t ->
      div_ [class_ "duration-end"] $ 
        toHtml (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t)
    let d = maybe "running" show (diffUTCTime <$> end <*> pure start)
    div_ [class_ "duration-pretty"] $ 
      toHtml d
