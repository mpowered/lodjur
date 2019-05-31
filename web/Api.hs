{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Api where

import           Data.Int                       ( Int32 )
import           Data.Text                      ( Text )
import           Data.Tree
import           Servant
import           Servant.HTML.Lucid
import           Servant.JS

import           Job
import           Types

type Api = "api" :>
 (    "jobs" :> "cards" :> Get '[HTML, JSON] (Card (Forest Job'))
 :<|> "job" :> Capture "jobId" Int32 :> "card" :> Get '[HTML, JSON] (Card Job')
 :<|> "job" :> Capture "jobId" Int32 :> "logs" :> Get '[HTML, JSON] [LogLine]
 :<|> "job" :> Capture "jobId" Int32 :> "rspec" :> Get '[HTML, JSON] RSpec'
 )

apiAsJS :: Text
apiAsJS = jsForAPI (Proxy :: Proxy Api) jquery

api :: ServerT Api AppM
api = jobsCards
 :<|> jobCard
 :<|> jobLogs
 :<|> jobRspec

jobsCards :: AppM (Card (Forest Job'))
jobsCards =
  Card LargeCard <$> runDb (recentJobsForest 100)

jobCard :: Int32 -> AppM (Card Job')
jobCard jobid = do
  j <- runDb $ lookupJob jobid
  maybe (throwError err404) (return . Card LargeCard) j

jobLogs :: Int32 -> AppM [LogLine]
jobLogs jobid = runDb $ jobLogLines jobid

jobRspec :: Int32 -> AppM RSpec'
jobRspec jobid = do
  r <- runDb $ lookupRSpec jobid
  maybe (throwError err404) return r
