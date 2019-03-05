-- The Github Users API, as described at
-- <http://developer.github.com/v3/checks/>.
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.Endpoints.Checks (
    createCheckRun,
    createCheckRunR,
    updateCheckRun,
    updateCheckRunR,
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

updateCheckRun :: Auth -> Name Owner -> Name Repo -> Id CheckRun -> UpdateCheckRun -> IO (Either Error CheckRun)
updateCheckRun auth owner repo crId = executeRequest auth . updateCheckRunR owner repo crId

updateCheckRunR :: Name Owner -> Name Repo -> Id CheckRun -> UpdateCheckRun -> GenRequest 'MtAntiopePreview 'RW CheckRun
updateCheckRunR owner repo crId =
  Command Patch ["repos", toPathPart owner, toPathPart repo, "check-runs", toPathPart crId] . encode
