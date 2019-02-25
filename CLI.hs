{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.ByteString.Char8        as BS
import           Data.Foldable
import           Data.Semigroup               ((<>))
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Time.Clock.POSIX
import qualified GitHub                       as GH
import qualified GitHub.Data.Id               as GH
import qualified GitHub.Data.Name             as GH
import qualified GitHub.Extra                 as GH
import qualified GitHub.Endpoints.Apps        as GH
import qualified GitHub.Endpoints.Apps.Installations as GH
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Options.Applicative
import qualified Web.JWT                      as JWT

import           Config

data Options = Options
  { optConfigFile   :: FilePath
  , optCommand      :: Command
  }

data Command
  = App AppOptions
  | Events EventsOptions
  | Installations InstallationsOptions

options :: Parser Options
options = Options
  <$> strOption
    (  long "config-file"
    <> metavar "PATH"
    <> short 'c'
    <> value "lodjur.toml"
    <> help "Path to Lodjur configuration file"
    )
  <*> hsubparser
    ( command "app" (info (App <$> appOptions) (progDesc "App"))
   <> command "events" (info (Events <$> eventsOptions) (progDesc "Events"))
   <> command "installations" (info (Installations <$> installationsOptions) (progDesc "Installations"))
    )

data AppOptions = AppOptions
  { appCommand      :: AppCommand
  }

data AppCommand
  = AppInfo
  | AppInstallation
  | AppInstallations

appOptions :: Parser AppOptions
appOptions = AppOptions
  <$> hsubparser
    ( command "info" (info (pure AppInfo) (progDesc "Get app info"))
   <> command "installation" (info (pure AppInstallation) (progDesc "Get installation info"))
   <> command "installations" (info (pure AppInstallations) (progDesc "Find installations"))
    )

data EventsOptions = EventsOptions
  { eventsCommand   :: EventsCommand
  }

data EventsCommand
  = EventsList EventsListOptions

eventsOptions :: Parser EventsOptions
eventsOptions = EventsOptions
  <$> hsubparser
    ( command "list" (info (EventsList <$> eventsListOptions) (progDesc "List events"))
    )

data EventsListOptions = EventsListOptions
  { eventsListOwner :: Text.Text
  , eventsListRepo  :: Text.Text
  }

eventsListOptions :: Parser EventsListOptions
eventsListOptions = EventsListOptions
  <$> strOption
    (  long "owner"
    <> metavar "OWNER"
    <> short 'o'
    <> help "Owner"
    )
  <*> strOption
    (  long "repository"
    <> metavar "REPO"
    <> short 'r'
    <> help "Repository name"
    )

data InstallationsOptions = InstallationsOptions
  { installationsCommand    :: InstallationsCommand
  }

data InstallationsCommand
  = InstallationsCreateJWT
  | InstallationsCreateToken
  | InstallationsRepositories

installationsOptions :: Parser InstallationsOptions
installationsOptions = InstallationsOptions
  <$> hsubparser
    ( command "create-jwt" (info (pure InstallationsCreateJWT) (progDesc "Create a new JWT"))
   <> command "create-token" (info (pure InstallationsCreateToken) (progDesc "Create a new installation access token"))
   <> command "repositories" (info (pure InstallationsRepositories) (progDesc "List repositories that an installation can access"))
    )

main :: IO ()
main = start =<< customExecParser p opts
 where
  p = prefs showHelpOnEmpty
  opts = info
    (options <**> helper)
    (fullDesc <> progDesc "Lodjur CLI" <> header
      "Command-line interface into Lodjur"
    )

start :: Options -> IO ()
start Options {..} = do
  cfg <- readConfiguration optConfigFile
  mgr <- newManager tlsManagerSettings

  case optCommand of
    App opts -> app cfg mgr opts
    Events opts -> events cfg mgr opts
    Installations opts -> installations cfg mgr opts

app :: Config -> Manager -> AppOptions -> IO ()
app cfg mgr AppOptions{..} =
  case appCommand of
    AppInfo -> appInfo cfg mgr
    AppInstallation -> appInstallation cfg mgr
    AppInstallations -> appInstallations cfg mgr

appInfo :: Config -> Manager -> IO ()
appInfo cfg mgr = do
  tok <- createInstallationJWT cfg
  result <- GH.executeRequestWithMgr mgr (GH.Bearer tok) GH.appR
  case result of
    Left err ->
      error $ "Unable to fetch app info: " <> show err
    Right a ->
      print a

appInstallation :: Config -> Manager -> IO ()
appInstallation cfg@Config{..} mgr = do
  tok <- createInstallationJWT cfg
  result <- GH.executeRequestWithMgr mgr (GH.Bearer tok) $
              GH.installationR (GH.Id githubInstallationId)
  case result of
    Left err ->
      error $ "Unable to fetch installation info: " <> show err
    Right a ->
      print a

appInstallations :: Config -> Manager -> IO ()
appInstallations cfg mgr = do
  tok <- createInstallationJWT cfg
  result <- GH.executeRequestWithMgr mgr (GH.Bearer tok) $
              GH.installationsForAppR GH.FetchAll
  case result of
    Left err ->
      error $ "Unable to find app installations: " <> show err
    Right a ->
      print a

events :: Config -> Manager -> EventsOptions -> IO ()
events cfg mgr EventsOptions{..} =
  case eventsCommand of
    EventsList opts -> eventsList cfg mgr opts

eventsList :: Config -> Manager -> EventsListOptions -> IO ()
eventsList cfg mgr EventsListOptions{..} = do
  GH.AccessToken{..} <- getInstallationAccessToken cfg mgr
  result <- GH.executeRequestWithMgr mgr (GH.OAuth accessToken) $
              GH.repositoryEventsR
                (GH.N eventsListOwner)
                (GH.N eventsListRepo)
                GH.FetchAll
  print result

installations :: Config -> Manager -> InstallationsOptions -> IO ()
installations cfg mgr InstallationsOptions{..} =
  case installationsCommand of
    InstallationsCreateJWT -> installationsCreateJWT cfg mgr
    InstallationsCreateToken -> installationsCreateToken cfg mgr
    InstallationsRepositories -> installationRepositories cfg mgr

installationsCreateJWT :: Config -> Manager -> IO ()
installationsCreateJWT cfg _mgr = do
  tok <- createInstallationJWT cfg
  BS.putStrLn tok

installationsCreateToken :: Config -> Manager -> IO ()
installationsCreateToken cfg mgr = do
  GH.AccessToken{..} <- getInstallationAccessToken cfg mgr
  BS.putStrLn accessToken
  traverse_ (\t -> putStrLn ("Expires at " <> show t)) accessTokenExpiresAt

installationRepositories :: Config -> Manager -> IO ()
installationRepositories cfg mgr = do
  GH.AccessToken{..} <- getInstallationAccessToken cfg mgr
  result <- GH.executeRequestWithMgr mgr (GH.OAuth accessToken) $
              GH.repositoriesForInstallationR
  print result

createInstallationJWT :: Config -> IO GH.Token
createInstallationJWT Config{..} = do
  now <- getPOSIXTime
  let claims = mempty { JWT.iss = JWT.stringOrURI (Text.pack $ show githubAppId)
                      , JWT.iat = JWT.numericDate now
                      , JWT.exp = JWT.numericDate (now + 600)
                      }
      jwt = JWT.encodeSigned githubAppSigner claims
  return $ Text.encodeUtf8 jwt

getInstallationAccessToken :: Config -> Manager -> IO GH.AccessToken
getInstallationAccessToken cfg@Config{..} mgr = do
  tok <- createInstallationJWT cfg
  result <- GH.executeRequestWithMgr mgr (GH.Bearer tok) $
              GH.createInstallationTokenR (GH.Id githubInstallationId)
  case result of
    Left err ->
      error $ "Unable to fetch installation access token: " <> show err
    Right token ->
      return token
