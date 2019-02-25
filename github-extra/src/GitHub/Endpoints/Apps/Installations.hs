-- The Github Users API, as described at
-- <https://developer.github.com/v3/apps/installations/>.
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.Endpoints.Apps.Installations (
    repositoriesForInstallation,
    repositoriesForInstallationR,
    module GitHub.Data,
    ) where

import GitHub.Auth
import GitHub.Data
import GitHub.Extra
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

repositoriesForInstallation :: Auth -> IO (Either Error InstallationRepositories)
repositoriesForInstallation auth = executeRequest auth repositoriesForInstallationR

repositoriesForInstallationR :: GenRequest 'MtMachineManPreview 'RO InstallationRepositories
repositoriesForInstallationR =
  Query ["installation", "repositories"] []

