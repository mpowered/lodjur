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
import qualified GitHub.Data.Name             as GH
import qualified GitHub.Extra                 as GH
import qualified GitHub.Endpoints.Apps        as GH
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
  = Events EventsOptions
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
    ( command "events" (info (Events <$> eventsOptions) (progDesc "Events"))
   <> command "installations" (info (Installations <$> installationsOptions) (progDesc "Installations"))
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

installationsOptions :: Parser InstallationsOptions
installationsOptions = InstallationsOptions
  <$> hsubparser
    ( command "create-jwt" (info (pure InstallationsCreateJWT) (progDesc "Create a new JWT"))
   <> command "create-token" (info (pure InstallationsCreateToken) (progDesc "Create a new installation access token"))
    )

main :: IO ()
main = start =<< execParser opts
 where
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
    Events opts -> events cfg mgr opts
    Installations opts -> installations cfg mgr opts

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

installationsCreateJWT :: Config -> Manager -> IO ()
installationsCreateJWT cfg _mgr = do
  tok <- createInstallationJWT cfg
  BS.putStrLn tok

installationsCreateToken :: Config -> Manager -> IO ()
installationsCreateToken cfg mgr = do
  GH.AccessToken{..} <- getInstallationAccessToken cfg mgr
  BS.putStrLn accessToken
  traverse_ (\t -> putStrLn ("Expires at " <> show t)) accessTokenExpiresAt

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
              GH.createInstallationTokenR (GH.mkId GH.Installation githubInstallationId)
  case result of
    Left err ->
      error $ "Unable to fetch installation access token: " <> show err
    Right token -> return token
