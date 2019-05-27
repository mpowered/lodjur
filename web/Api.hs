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

module Api where

import           Control.Applicative
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson
import           Data.Int                       ( Int32 )
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time
import           Data.Tree
import           Lucid
import           Servant
import           Servant.HTML.Lucid
import           Servant.JS

import           Job
import           Types

type Api = "api" :>
 (    "jobs" :> "outline" :> Get '[HTML, JSON] (Outline (Forest Job'))
 :<|> "job" :> Capture "jobId" Int32 :> "detail" :> Get '[HTML, JSON] (Detail Job')
 :<|> "job" :> Capture "jobId" Int32 :> "logs" :> Get '[HTML, JSON] [LogLine]
 )

apiAsJS :: Text
apiAsJS = jsForAPI (Proxy :: Proxy Api) jquery

api :: ServerT Api AppM
api = jobsOutline
 :<|> jobDetail
 :<|> jobLogs

jobsOutline :: AppM (Outline (Forest Job'))
jobsOutline =
  Outline <$> runDb (recentJobsForest 20)

data Detail a = Detail UTCTime a

instance ToJSON (Detail Job') where
  toJSON (Detail _ j) = toJSON j

instance ToHtml (Detail Job') where
  toHtmlRaw = toHtml
  toHtml (Detail now Job' {..}) = do
    div_ [ class_ "job-head" ] $ do
      div_ $ statusIcon job'Status job'Conclusion
      div_ $ toHtml job'Name
    div_ [ class_ "job-commit commit-left" ] $ do
      jobAttr "fab fa-fw fa-github" (job'CommitOwner <> "/" <> job'CommitRepo)
      jobAttr "far fa-fw fa-code-branch" (fromMaybe "" job'CommitBranch)
      jobAttr "far fa-fw fa-code-commit" job'CommitSha
      jobAttr "far fa-fw fa-comment-alt" (fromMaybe "" job'CommitMessage)
    div_ [ class_ "job-commit commit-right" ] $ do
      jobAttr "far fa-fw fa-at" committer
      jobAttr "far fa-fw fa-clock" (maybe "" (prettyTime now) job'StartedAt)
      jobAttr "far fa-fw fa-stopwatch" (maybe "" prettyDuration (diffUTCTime <$> (job'CompletedAt <|> pure now) <*> job'StartedAt))
    where
      committer = Text.unwords $ catMaybes
        [ job'CommitCommitter
        , (\email -> "<" <> email <> ">") <$> job'CommitAuthorEmail
        ]

      jobAttr :: (Monad m, ToHtml a) => Text -> a -> HtmlT m ()
      jobAttr ico val = do
        div_ [ class_ "job-commit-attr" ] $ do
          div_ $ span_ [ class_ ico ] ""
          div_ $ toHtml val

      prettyTime :: Monad m => UTCTime -> UTCTime -> HtmlT m ()
      prettyTime _now t =
        toHtml $ formatTime defaultTimeLocale "%F %r" t

      prettyDuration :: Monad m => NominalDiffTime -> HtmlT m ()
      prettyDuration d =
        toHtml $ show d

jobDetail :: Int32 -> AppM (Detail Job')
jobDetail jobid = do
  now <- liftIO getCurrentTime
  j <- runDb $ lookupJob jobid
  maybe (throwError err404) (return . Detail now) j

jobLogs :: Int32 -> AppM [LogLine]
jobLogs jobid = runDb $ jobLogsTail jobid