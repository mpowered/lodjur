{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Monad.Trans.Maybe
import qualified Data.Aeson               as Aeson
import           Data.Pool
import           Data.Time.Clock
import qualified Data.UUID.V4             as UUID
import           Data.UUID                (UUID)
import           Lodjur.DB
import           Database.Beam
import qualified Database.Beam.Postgres   as Pg

main :: IO ()
main = do
  now <- getCurrentTime
  pool <- createPool (Pg.connectPostgreSQL "") Pg.close 1 120 1
  bid <- UUID.nextRandom
  did <- UUID.nextRandom
  cid <- UUID.nextRandom
  let rev = Revision "abc" now
      b   = Build (Job bid "build" (pk rev) now now JobRunning "shaun")
      d   = Deploy (Job did "deploy" (pk rev) now now JobRunning "shaun") "target"
      c   = Check (Job cid "check testapp" (pk rev) now now JobRunning "shaun") 1 0 0 0.5
  withResource pool $ \conn ->
    Pg.runBeamPostgresDebug putStrLn conn $ do
      runDelete $
        delete (dbRevisions db) (const $ val_ True)
      runInsert $
        insert (dbRevisions db) $
        insertValues [ rev ]
      runInsert $
        insert (dbBuilds db) $
        insertValues [ b ]
      runInsert $
        insert (dbDeploys db) $
        insertValues [ d ]
      runInsert $
        insert (dbChecks db) $
        insertValues [ c ]
      runInsert $
        insert (dbCheckExamples db) $
        insertExpressions
          [ CheckExample
              default_
              (val_ (pk c))
              (val_ "example 1")
              (val_ "elephants")
              (val_ "pass")
              (val_ "eli.rb")
              (val_ 1)
              (val_ (Aeson.String ""))
          ]

  rows <- withResource pool $ \conn ->
    Pg.runBeamPostgresDebug putStrLn conn $
      runSelectReturningOne $
        select $ do
          r <- all_ (dbRevisions db)
          j <- oneToOne_ (dbBuilds db) (jobRevision . buildJob) r
          pure (r, j)

  print rows

{-
  job' <- withResource pool $ \conn ->
    getJob' conn jobid
  print job'

data Job'
  = BuildJob'
    { buildJob'     :: Job
    }
  | DeployJob'
    { deployJob'    :: Job
    , deployDeploy  :: Deploy
    }
  | CheckJob'
    { checkJob'     :: Job
    , checkChecks   :: [Check]
    }
  deriving (Show)

getJob' :: Pg.Connection -> UUID -> IO (Maybe Job')
getJob' conn uuid = runMaybeT $ do
  job <- MaybeT $ getJob conn uuid
  case jobType job of
    BuildJob -> return $ BuildJob' job
    DeployJob -> do
      deploy <- MaybeT $ getDeploy conn job
      return $ DeployJob' job deploy
    CheckJob -> do
      checks <- liftIO $ getChecks conn job
      return $ CheckJob' job checks

getJob :: Pg.Connection -> UUID -> IO (Maybe Job)
getJob conn uuid =
  Pg.runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $
      select $
        filter_ (\j -> jobId j ==. val_ uuid) $
          all_ (dbJobs db)

getDeploy :: Pg.Connection -> Job -> IO (Maybe Deploy)
getDeploy conn job =
  Pg.runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $
      select $
        filter_ (\d -> deployJob d ==. val_ (pk job)) $
          all_ (dbDeploys db)

getChecks :: Pg.Connection -> Job -> IO [Check]
getChecks conn job =
  Pg.runBeamPostgresDebug putStrLn conn $
    runSelectReturningList $
      select $
        filter_ (\c -> checkJob c ==. val_ (pk job)) $
          all_ (dbChecks db)

-}
