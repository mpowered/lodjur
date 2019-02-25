-- The Github Users API, as described at
-- <http://developer.github.com/v3/apps/>.
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.Endpoints.Apps (
    app,
    appR,
    installationsForApp,
    installationsForAppR,
    installation,
    installationR,
    installationsForUser,
    installationsForUserR,
    createInstallationToken,
    createInstallationTokenR,
    installationForOrg,
    installationForOrgR,
    installationForRepo,
    installationForRepoR,
    installationForUser,
    installationForUserR,
    module GitHub.Data,
    ) where

import GitHub.Auth
import GitHub.Data
import GitHub.Extra
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | Returns the GitHub App associated with the authentication credentials used.
-- You must use a JWT to access this endpoint.
app :: Auth -> IO (Either Error App)
app auth = executeRequest auth appR

-- | Get the authenticated GitHub App.
-- See <https://developer.github.com/v3/apps/#get-the-authenticated-github-app>
appR :: GenRequest 'MtMachineManPreview 'RO App
appR =
  Query ["app"] []

installationsForApp :: Auth -> IO (Either Error (Vector Installation))
installationsForApp auth = executeRequest auth $ installationsForAppR FetchAll

installationsForAppR :: FetchCount -> GenRequest 'MtMachineManPreview 'RO (Vector Installation)
installationsForAppR =
  PagedQuery ["app", "installations"] []

installation :: Auth -> Id Installation -> IO (Either Error Installation)
installation auth instId = executeRequest auth $ installationR instId

installationR :: Id Installation -> GenRequest 'MtMachineManPreview 'RO Installation
installationR instId =
  Query ["app", "installations", toPathPart instId] []

installationsForUser :: Auth -> IO (Either Error (Vector Installations))
installationsForUser auth = executeRequest auth $ installationsForUserR FetchAll

installationsForUserR :: FetchCount -> GenRequest 'MtMachineManPreview 'RO (Vector Installations)
installationsForUserR =
  PagedQuery ["user", "installations"] []

-- | Create a new installation token.
-- Requires a Bearer JWT token
createInstallationToken :: Auth -> Id Installation -> IO (Either Error AccessToken)
createInstallationToken auth = executeRequest auth . createInstallationTokenR

-- | Create a new installation token.
-- See <https://developer.github.com/v3/apps/##create-a-new-installation-token>
createInstallationTokenR :: Id Installation -> GenRequest 'MtMachineManPreview 'RW AccessToken
createInstallationTokenR instId =
  Command Post ["app", "installations", toPathPart instId, "access_tokens"] ""

installationForOrg :: Auth -> Name Organization -> IO (Either Error Installation)
installationForOrg auth org = executeRequest auth $ installationForOrgR org

installationForOrgR :: Name Organization -> GenRequest 'MtMachineManPreview 'RO Installation
installationForOrgR org =
  Query ["orgs", toPathPart org, "installation"] []

installationForRepo :: Auth -> Name Owner -> Name Repo -> IO (Either Error Installation)
installationForRepo auth user repo = executeRequest auth $ installationForRepoR user repo

installationForRepoR :: Name Owner -> Name Repo -> GenRequest 'MtMachineManPreview 'RO Installation
installationForRepoR user repo  =
  Query ["repos", toPathPart user, toPathPart repo, "installation"] []

installationForUser :: Auth -> Name User -> IO (Either Error Installation)
installationForUser auth user = executeRequest auth $ installationForUserR user

installationForUserR :: Name User -> GenRequest 'MtMachineManPreview 'RO Installation
installationForUserR user =
  Query ["users", toPathPart user, "installation"] []

