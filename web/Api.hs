{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Api where

import           Data.Int (Int32)
import           Data.Text (Text)
import           Servant
import           Servant.HTML.Lucid
import           Servant.JS

import           Job
import           Types

type Api = "api" :>
 (    "recent-jobs" :> Get '[JSON] [Job']
 :<|> "job" :> Capture "jobId" Int32 :> Get '[JSON, HTML] Job'
 )

apijs :: AppM Text
apijs = return $ jsForAPI (Proxy :: Proxy Api) jquery

api :: ServerT Api AppM
api
    = recentJobs
 :<|> job

recentJobs :: AppM [Job']
recentJobs =
  runDb $ recentRoots 20

job :: Int32 -> AppM Job'
job jobid = do
  j <- runDb $ lookupJob jobid
  maybe (throwError err404) return j
