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
import           Data.Int                       ( Int32, Int64 )
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

data RSpec'
  = RSpec'
    { rspec'Id                  :: Int32
    , rspec'Duration            :: Double
    , rspec'ExampleCount        :: Int
    , rspec'FailureCount        :: Int
    , rspec'PendingCount        :: Int
    , rspec'Tests               :: [RSpecTest']
    }
  deriving (Show, Generic)

instance ToJSON RSpec' where
  toJSON = genericToJSON options

instance FromJSON RSpec' where
  parseJSON = genericParseJSON options

data RSpecTest'
  = RSpecTest'
    { rspectest'Id                  :: Int64
    , rspectest'Description         :: Text
    , rspectest'FullDescription     :: Text
    , rspectest'Status              :: Text
    , rspectest'FilePath            :: Text
    , rspectest'LineNumber          :: Int
    , rspectest'ExceptionClass      :: Maybe Text
    , rspectest'ExceptionMessage    :: Maybe Text
    , rspectest'ExceptionBacktrace  :: Maybe Text
    }
  deriving (Show, Generic)

instance ToHtml RSpec' where
  toHtmlRaw = toHtml
  toHtml RSpec' {..} = do
    div_ $ do
      div_ $ do
        div_ "Duration"
        div_ $ toHtml (show rspec'Duration)
      div_ $ do
        div_ "Example Count"
        div_ $ toHtml (show rspec'ExampleCount)
      div_ $ do
        div_ "Failure Count"
        div_ $ toHtml (show rspec'FailureCount)
      div_ $ do
        div_ "Pending Count"
        div_ $ toHtml (show rspec'PendingCount)
    div_ [ class_ "rspec-tests" ] $ do
      div_ [ class_ "rspec-test" ] $ do
        div_ [ class_ "wide"]        "Description"
        div_ [ class_ "wide"]        "Full Description"
        div_ [ class_ "narrow"]      "Status"
        div_ [ class_ "wide"]        "File Path"
        div_ [ class_ "narrow"]      "Line Number"
        div_ [ class_ "wide"]        "Exception Class"
        div_ [ class_ "wide"]        "Exception Message"
        div_ [ class_ "scroll"]      "Exception Backtrace"
      mapM_ toHtml rspec'Tests

instance ToHtml RSpecTest' where
  toHtmlRaw = toHtml
  toHtml RSpecTest' {..} =
    div_ [ class_ "rspec-test" ] $ do
      div_ [ class_ "wide" ]        $ toHtml rspectest'Description
      div_ [ class_ "wide" ]        $ toHtml rspectest'FullDescription
      div_ [ class_ "narrow" ]      $ toHtml rspectest'Status
      div_ [ class_ "wide" ]        $ toHtml rspectest'FilePath
      div_ [ class_ "narrow" ]      $ toHtml (show rspectest'LineNumber)
      div_ [ class_ "wide" ]        $ maybe "" toHtml rspectest'ExceptionClass
      div_ [ class_ "wide" ]        $ maybe "" toHtml rspectest'ExceptionMessage
      div_ [ class_ "scroll" ]      $ maybe "" toHtml rspectest'ExceptionBacktrace

instance ToJSON RSpecTest' where
  toJSON = genericToJSON options

instance FromJSON RSpecTest' where
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
    jobStatusIcon job'Status job'Conclusion
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

file_ :: Monad m => HtmlT m ()
file_ = icon "far fa-fw fa-file"

bars_ :: Monad m => HtmlT m ()
bars_ = icon "far fa-fw fa-bars"

comment_ :: Monad m => HtmlT m ()
comment_ = icon "far fa-fw fa-comment"

curly_ :: Monad m => HtmlT m ()
curly_ = icon "far fa-fw fa-brackets-curly"

circle_ :: Monad m => HtmlT m ()
circle_ = icon "far fa-fw fa-circle"

dotcircle_ :: Monad m => HtmlT m ()
dotcircle_ = icon "far fa-fw fa-dot-circle"

infocircle_ :: Monad m => HtmlT m ()
infocircle_ = icon "far fa-fw fa-info-circle"

github_ :: Monad m => HtmlT m ()
github_ = icon "fab fa-fw fa-github"

jobStatusIcon :: Monad m => Status -> Maybe Conclusion -> HtmlT m ()
jobStatusIcon status conclusion = case status of
  Queued     -> clock_
  InProgress -> spinner_
  Completed  -> case conclusion of
    Just Success   -> check_
    Just Failure   -> times_
    Just Cancelled -> ban_
    Just Neutral   -> question_
    _              -> exclamation_

jobStatusClass :: Status -> Maybe Conclusion -> Text
jobStatusClass status conclusion = case status of
  Queued     -> "queued"
  InProgress -> "inprogress"
  Completed  -> case conclusion of
    Just Success   -> "success"
    Just Failure   -> "failure"
    Just Cancelled -> "cancelled"
    Just Neutral   -> "neutral"
    _              -> "other"

testStatusClass :: Text -> Text
testStatusClass status = case status of
  "passed"  -> "success"
  "failed"  -> "failure"
  "pending" -> "cancelled"
  _         -> "other"

testStatusIcon :: Monad m => Text -> HtmlT m ()
testStatusIcon status = case status of
  "passed"  -> check_
  "failed"  -> times_
  "pending" -> question_
  _         -> exclamation_

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
  toHtml = mapM_ toHtml

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

lookupRSpec :: Int32 -> Pg (Maybe RSpec')
lookupRSpec jobid = do
  r <- runSelectReturningOne $ select $
    filter_ (\r -> rspecJob r ==. val_ (JobKey jobid)) $ all_ (dbRspecs db)
  case r of
    Just r' -> do
      ts <- runSelectReturningList $ select
        $ orderBy_ (\t -> (asc_ $ rspectestFilePath t, asc_ $ rspectestLineNumber t))
        $ filter_ (\t -> rspectestStatus t ==. val_ "failed")
        $ oneToMany_ (dbRspecTests db) rspectestRSpec (val_ r')
      return $ Just (rspec' r' ts)
    Nothing ->
      return Nothing

rspec' :: RSpec -> [RSpecTest] -> RSpec'
rspec' RSpec {..} tests = RSpec'
    { rspec'Id                  = rspecId
    , rspec'Duration            = rspecDuration
    , rspec'ExampleCount        = rspecExampleCount
    , rspec'FailureCount        = rspecFailureCount
    , rspec'PendingCount        = rspecPendingCount
    , rspec'Tests               = map rspectest' tests
    }

rspectest' :: RSpecTest -> RSpecTest'
rspectest' RSpecTest {..} = RSpecTest'
    { rspectest'Id                  = rspectestId
    , rspectest'Description         = rspectestDescription
    , rspectest'FullDescription     = rspectestFullDescription
    , rspectest'Status              = rspectestStatus
    , rspectest'FilePath            = rspectestFilePath
    , rspectest'LineNumber          = rspectestLineNumber
    , rspectest'ExceptionClass      = rspectestExceptionClass
    , rspectest'ExceptionMessage    = rspectestExceptionMessage
    , rspectest'ExceptionBacktrace  = rspectestExceptionBacktrace
    }

newtype Card a = Card a

newtype SmallCard a = SmallCard a

instance ToJSON a => ToJSON (Card a) where
  toJSON (Card a) = toJSON a

instance ToHtml (Card Job') where
  toHtmlRaw = toHtml
  toHtml (Card Job' {..}) =
    div_ [class_ "card job", data_ "job-id" (cs $ show job'Id) ] $ do
      div_ [ class_ ("card-trim left status-background " <> jobStatusClass job'Status job'Conclusion) ] ""
      div_ [ class_ "card-col narrow" ] $ do
        div_ [ class_ "card-row" ] $
          attr (jobStatusIcon job'Status job'Conclusion) job'Name
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

instance ToHtml (SmallCard Job') where
  toHtmlRaw = toHtml
  toHtml (SmallCard Job' {..}) =
    div_ [class_ "card job",  data_ "job-id" (cs $ show job'Id) ] $ do
      div_ [ class_ ("card-trim left status-background " <> jobStatusClass job'Status job'Conclusion) ] ""
      div_ [ class_ "card-col narrow" ] $ do
        div_ [ class_ "card-row" ] $
          attr (jobStatusIcon job'Status job'Conclusion) job'Name
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
  toHtml (Card (Node job children)) = do
    if null children
      then
        div_ [ class_ "card-item" ] $
          toHtml (Card job)
      else do
        div_ [ class_ "card-item" ] $ do
          toHtml (Card job)
          div_ [class_ "card-sublist"] $
            mapM_ (toHtml . SmallCard . rootLabel) children

instance ToHtml (Card (Forest Job')) where
  toHtmlRaw = toHtml
  toHtml (Card jobs) = mapM_ (toHtml . Card) jobs

instance ToHtml [Card RSpecTest'] where
  toHtmlRaw = toHtml
  toHtml = mapM_ toHtml

instance ToHtml (Card RSpecTest') where
  toHtmlRaw = toHtml
  toHtml (Card RSpecTest' {..}) =
    div_ [class_ "card rspectest", data_ "rspec-test-id" (cs $ show rspectest'Id) ] $ do
      div_ [ class_ ("card-trim left status-background " <> testStatusClass rspectest'Status) ] ""
      div_ [ class_ "card-col extrawide" ] $ do
        div_ [ class_ "card-row" ] $
          attr (testStatusIcon rspectest'Status) rspectest'Description
        div_ [ class_ "card-row" ] $
          attr bars_ rspectest'FullDescription
        div_ [ class_ "card-row" ] $
          attr file_ (rspectest'FilePath <> " " <> cs (show rspectest'LineNumber))
        div_ [ class_ "card-row" ] $
          attrMaybe curly_ rspectest'ExceptionClass
        div_ [ class_ "card-row" ] $
          attrMaybe comment_ rspectest'ExceptionMessage
      div_ [ class_ "card-col fullwidth" ] $ do
        div_ [ class_ "card-row" ] $
          attrMaybe infocircle_ rspectest'ExceptionBacktrace
      div_ [ class_ "card-trim right" ] ""

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
