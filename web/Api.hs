{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Api where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Aeson
import qualified Data.Binary.Builder        as Builder
import           Data.ByteString.Lazy       (ByteString)
import           Data.Int (Int32)
import           Data.Text (Text)
import           Network.Wai.EventSource
import           Servant
import           Servant.EventStream
import           Servant.HTML.Lucid
import           Servant.JS
import           Servant.Types.SourceT

import           Lodjur.Core                as Core
import           Lodjur.Database
import           Job
import           Types

type Api = JsApi :<|> StreamApi

type JsApi = "api" :>
 (    "recent-jobs" :> Get '[JSON, HTML] [Job']
 :<|> "job" :> Capture "jobId" Int32 :> Get '[JSON, HTML] Job'
 )

type StreamApi = "api" :>
      "watch-jobs" :> StreamGet NoFraming EventStream E

apijs :: AppM Text
apijs = return $ jsForAPI (Proxy :: Proxy JsApi) jquery

api :: ServerT Api AppM
api = jsapi :<|> sapi
 where
  jsapi = recentJobs
     :<|> job
  sapi  = watchJobs

-- Text should be utf8 safe
textEvent :: ByteString -> ServerEvent
textEvent x = ServerEvent { eventId = Nothing, eventName = Nothing, eventData = [Builder.fromLazyByteString x] }

jsonEvent :: ToJSON a => a -> ServerEvent
jsonEvent = textEvent . encode

recentJobs :: AppM [Job']
recentJobs =
  runDb $ recentRoots 20

job :: Int32 -> AppM Job'
job jobid = do
  j <- runDb $ lookupJob jobid
  maybe (throwError err404) return j

newtype E = E { runEvent :: IO () }

instance ToSourceIO ServerEvent Text where


watchJobs :: AppM E
watchJobs = do
  pool <- getEnv Types.envDbPool
  core <- getEnv Types.envCore
  chan <- liftIO $ Core.subscribe core

  let
    go = do
      jobs <- withConnection pool $ \conn -> beam conn (recentRoots 20)
      return (jsonEvent jobs)

    loop = do
      event <- atomically $ readTChan chan
      case event of
        Core.JobSubmitted -> Just <$> go
        Core.JobUpdated   -> Just <$> go
        _                 -> return Nothing


  return (fromStepT (lift go))
  -- return (fromStepT ((liftIO . lift) go) <> eventSourceMaybe loop)
