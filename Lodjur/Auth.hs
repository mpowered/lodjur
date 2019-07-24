module Lodjur.Auth where

import Data.Text (Text)

data TeamAuthConfig = TeamAuthConfig
  { githubAuthTeam      :: Text
  , githubTestTeam      :: Text
  , githubAuthOrg       :: Text
  }
