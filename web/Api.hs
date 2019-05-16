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
 (    "recent-jobs" :> Get '[HTML, JSON] [Job']
 :<|> "jobs-outline" :> Get '[HTML, JSON] (Outline (Forest Job'))
 :<|> "job" :> Capture "jobId" Int32 :> Get '[HTML, JSON] Job'
 )

apiAsJS :: Text
apiAsJS = jsForAPI (Proxy :: Proxy Api) jquery

api :: ServerT Api AppM
api = recentJobs
 :<|> jobsOutline
 :<|> job

recentJobs :: AppM [Job']
recentJobs =
  runDb $ recentRoots 20

jobsOutline :: AppM (Outline (Forest Job'))
jobsOutline =
  Outline <$> runDb (recentJobsForest 20)

job :: Int32 -> AppM Job'
job jobid = do
  j <- runDb $ lookupJob jobid
  maybe (throwError err404) return j