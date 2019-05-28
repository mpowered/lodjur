{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE RecursiveDo            #-}

module Job where

import           Control.Applicative
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
    , log'Timestamp             :: UTCTime
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

clock :: Monad m => HtmlT m ()
clock = icon "far fa-fw fa-clock"

stopwatch :: Monad m => HtmlT m ()
stopwatch = icon "far fa-fw fa-stopwatch"

spinner :: Monad m => HtmlT m ()
spinner = icon "far fa-fw fa-pulse fa-spinner"

check :: Monad m => HtmlT m ()
check = icon "success fas fa-fw fa-check"

times :: Monad m => HtmlT m ()
times = icon "failure fas fa-fw fa-times"

ban :: Monad m => HtmlT m ()
ban = icon "fas fa-fw fa-ban"

question :: Monad m => HtmlT m ()
question = icon "fas fa-fw fa-question"

exclamation :: Monad m => HtmlT m ()
exclamation = icon "fas fa-fw fa-exclamation"

codebranch :: Monad m => HtmlT m ()
codebranch = icon "far fa-fw fa-code-branch"

statusIcon :: Monad m => Status -> Maybe Conclusion -> HtmlT m ()
statusIcon status conclusion = case status of
  Queued     -> clock
  InProgress -> spinner
  Completed  -> case conclusion of
    Just Success   -> check
    Just Failure   -> times
    Just Cancelled -> ban
    Just Neutral   -> question
    _              -> exclamation

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
  toHtml ls = div_ $ mapM_ toHtml ls

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

jobLogsTail :: Int32 -> Pg [LogLine]
jobLogsTail jobid = do
  ls <-
    runSelectReturningList
    $ select
    $ limit_ 100
    $ orderBy_ (desc_ . logCreatedAt)
    $ filter_ (\l -> logJob l ==. val_ (JobKey jobid))
    $ all_ (dbLogs db)
  return $ reverse $ map (\Log {..} -> LogLine logText logCreatedAt) ls

data Card a = Card UTCTime a

instance ToJSON a => ToJSON (Card a) where
  toJSON (Card _ a) = toJSON a

instance ToHtml (Card Job') where
  toHtmlRaw = toHtml
  toHtml (Card now Job' {..}) =
    div_ [class_ "card"] $ do
      div_ [ class_ "card-col" ] $ do
        div_ [ class_ "card-row" ] $
          attr (statusIcon job'Status job'Conclusion) job'Name
        div_ [ class_ "card-row" ] $
          attrMaybe codebranch job'CommitBranch
      div_ [ class_ "card-col" ] $ do
        div_ [ class_ "card-row" ] $
          attrMaybe clock (prettyTime now <$> job'StartedAt)
        div_ [ class_ "card-row" ] $
          attrMaybe stopwatch (prettyDuration <$> (diffUTCTime <$> (job'CompletedAt <|> pure now) <*> job'StartedAt))

instance ToHtml (Card (Tree Job')) where
  toHtmlRaw = toHtml
  toHtml (Card now (Node job children)) = do
    if null children
      then
        div_ [ class_ "card-item" ] $
          toHtml (Card now job)
      else do
        div_ [ class_ "card-item" ] $ do
          toHtml (Card now job)
          div_ [class_ "card-sublist"] $
            mapM_ (toHtml . Card now . rootLabel) children

instance ToHtml (Card (Forest Job')) where
  toHtmlRaw = toHtml
  toHtml (Card now jobs) = mapM_ (toHtml . Card now) jobs

prettyTime :: Monad m => UTCTime -> UTCTime -> HtmlT m ()
prettyTime _now t =
  toHtml $ formatTime defaultTimeLocale "%F %r" t

prettyDuration :: Monad m => NominalDiffTime -> HtmlT m ()
prettyDuration d =
  toHtml $ show d
