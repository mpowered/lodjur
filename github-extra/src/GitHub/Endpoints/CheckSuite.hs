-- The Github Users API, as described at
-- <http://developer.github.com/v3/checks/suites/>.
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.Endpoints.CheckSuite (
    createCheckSuite,
    createCheckSuiteR,
    module GitHub.Data,
    module GitHub.Extra,
    ) where

import GitHub.Auth
import GitHub.Data
import GitHub.Extra
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

createCheckSuite :: Auth -> Name Owner -> Name Repo -> NewCheckSuite -> IO (Either Error CheckSuite)
createCheckSuite auth owner repo = executeRequest auth . createCheckSuiteR owner repo

createCheckSuiteR :: Name Owner -> Name Repo -> NewCheckSuite -> GenRequest 'MtAntiopePreview 'RW CheckSuite
createCheckSuiteR owner repo =
  Command Post ["repos", toPathPart owner, toPathPart repo, "check-suites"] . encode