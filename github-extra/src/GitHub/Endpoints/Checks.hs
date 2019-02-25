-- The Github Users API, as described at
-- <http://developer.github.com/v3/checks/>.
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.Endpoints.Checks (
    createCheckRun,
    createCheckRunR,
    module GitHub.Data,
    module GitHub.Extra,
    ) where

import GitHub.Auth
import GitHub.Data
import GitHub.Extra
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

createCheckRun :: Auth -> Name Owner -> Name Repo -> NewCheckRun -> IO (Either Error CheckRun)
createCheckRun auth owner repo = executeRequest auth . createCheckRunR owner repo

createCheckRunR :: Name Owner -> Name Repo -> NewCheckRun -> GenRequest 'MtAntiopePreview 'RW CheckRun
createCheckRunR owner repo =
  Command Post ["repos", toPathPart owner, toPathPart repo, "check-runs"] . encode

