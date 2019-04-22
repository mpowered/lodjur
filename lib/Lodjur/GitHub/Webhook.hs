{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Lodjur.GitHub.Webhook
  ( RepoWebhookCheckEvent (..)
  , GitHubCheckEvent
  , matchCheckEvent
  , module Servant.GitHub.Webhook
  , GitHubKey(..)
  , gitHubKey
  ) where

import           Data.Aeson
import qualified Data.ByteString        as BS
import           Data.ByteString.Lazy     (toStrict)
import           Data.List                (intercalate)
import           Data.Maybe               (mapMaybe)
import           Data.String.Conversions  (cs)
import           Network.Wai              (requestHeaders)
import           Servant
import           Servant.GitHub.Webhook hiding (GitHubKey, gitHubKey)
import qualified Servant.GitHub.Webhook as Webhook
import           Servant.Server.Internal

data RepoWebhookCheckEvent
  = WebhookCheckSuiteEvent
  | WebhookCheckRunEvent

type instance Demote' ('KProxy :: KProxy RepoWebhookCheckEvent) = RepoWebhookCheckEvent

instance Webhook.Reflect 'WebhookCheckSuiteEvent where
  reflect _ = WebhookCheckSuiteEvent

instance Reflect 'WebhookCheckRunEvent where
  reflect _ = WebhookCheckRunEvent

instance FromJSON RepoWebhookCheckEvent where
  parseJSON (String "check_suite") = pure WebhookCheckSuiteEvent
  parseJSON (String "check_run") = pure WebhookCheckRunEvent
  parseJSON _ = fail "Could not build a Webhook event"

instance ToJSON RepoWebhookCheckEvent where
  toJSON WebhookCheckSuiteEvent = String "check_suite"
  toJSON WebhookCheckRunEvent = String "check_run"

data GitHubCheckEvent (events :: [RepoWebhookCheckEvent])

instance
  (Reflect events, HasServer sublayout context)
  => HasServer (GitHubCheckEvent events :> sublayout) context where

  type ServerT (GitHubCheckEvent events :> sublayout) m
    = RepoWebhookCheckEvent -> ServerT sublayout m

  hoistServerWithContext _ _ f s = \p -> hoistServerWithContext p1 p2 f (s p) where
    p1 = Proxy :: Proxy sublayout
    p2 = Proxy :: Proxy context

  route
    :: forall env. Proxy (GitHubCheckEvent events :> sublayout)
    -> Context context
    -> Delayed env (RepoWebhookCheckEvent -> Server sublayout)
    -> Router env
  route Proxy context subserver
    = route
      (Proxy :: Proxy sublayout)
      context
      (addAuthCheck subserver go)
    where
      lookupGHEvent = lookup "X-Github-Event"

      events :: [RepoWebhookCheckEvent]
      events = reflect (Proxy :: Proxy events)

      eventNames :: String
      eventNames = intercalate ", " $ (cs . encode) <$> events

      go :: DelayedIO RepoWebhookCheckEvent
      go = withRequest $ \req ->
        case lookupGHEvent (requestHeaders req) of
          Nothing -> delayedFail err401
          Just h ->
            case mapMaybe (`matchCheckEvent` h) events of
              [] -> delayedFail err404
                { errBody = cs $ "supported events: " <> eventNames }
              (event:_) -> pure event

matchCheckEvent :: RepoWebhookCheckEvent -> BS.ByteString -> Maybe RepoWebhookCheckEvent
matchCheckEvent e name
  | toStrict (encode e) == name' = Just e
  | otherwise = Nothing
  where name' = "\"" <> name <> "\""

-- Workaround for singlekey no longer working
-- see: https://github.com/tsani/servant-github-webhook/issues/13
newtype GitHubKey = GitHubKey (forall result. Webhook.GitHubKey result)

gitHubKey :: IO BS.ByteString -> GitHubKey
gitHubKey k = GitHubKey (Webhook.gitHubKey k)

instance HasContextEntry '[GitHubKey] (Webhook.GitHubKey result) where
  getContextEntry (GitHubKey x :. _) = x
