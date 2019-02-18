{-# LANGUAGE OverloadedStrings     #-}

module Main where

-- import           Control.Monad.Trans.Maybe
import qualified Data.Aeson               as Aeson
import           Data.Pool
import           Data.Time.Clock
import qualified Data.UUID.V4             as UUID
-- import           Data.UUID                (UUID)
import           Lodjur.DB
import           Database.Beam
import qualified Database.Beam.Postgres   as Pg
import           Lodjur.Git               (Hash)

main :: IO ()
main = do
  now <- getCurrentTime
  pool <- createPool (Pg.connectPostgreSQL "") Pg.close 1 120 1
  bid <- UUID.nextRandom
  did <- UUID.nextRandom
  cid <- UUID.nextRandom
  let rev = Revision "abc" now
      b   = Build bid (pk rev) now now JobRunning "shaun"
      d   = Deploy did (pk rev) now now JobRunning "shaun" "target"
      c   = Check cid (pk rev) now now JobRunning "shaun" "testapp" 1 0 0 0.5
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

{-
  rows <- withResource pool $ \conn ->
    Pg.runBeamPostgresDebug putStrLn conn $
      runSelectReturningOne $
        select $ do
          r <- all_ (dbRevisions db)
          j <- oneToOne_ (dbBuilds db) buildRevision r
          pure (r, j)

  print rows
-}

  jobs <- withResource pool $ \conn ->
    getJobs conn rev
  print jobs

data Job
  = BuildJob Build
  | DeployJob Deploy
  | CheckJob Check
  deriving (Show)

lookupRevision :: Pg.Connection -> Hash -> IO (Maybe Revision)
lookupRevision conn revid =
  Pg.runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $
      select $
        filter_ (\a -> revId a ==. val_ revid) $
          all_ (dbRevisions db)

getJobs :: Pg.Connection -> Revision -> IO [Job]
getJobs conn rev =
  Pg.runBeamPostgresDebug putStrLn conn $ do
    builds <-
      runSelectReturningList $
        select $
          filter_ (\a -> buildRevision a ==. val_ (pk rev)) $
            all_ (dbBuilds db)
    deploys <-
      runSelectReturningList $
        select $
          filter_ (\a -> deployRevision a ==. val_ (pk rev)) $
            all_ (dbDeploys db)
    checks <-
      runSelectReturningList $
        select $
          filter_ (\a -> checkRevision a ==. val_ (pk rev)) $
            all_ (dbChecks db)
    return
      $  map BuildJob builds
      ++ map DeployJob deploys
      ++ map CheckJob checks

{-
getBuilds :: Pg.Connection -> UUID -> IO (Maybe Build)
getBuild conn uuid =
  Pg.runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $
      select $
        filter_ (\j -> jobId j ==. val_ uuid) $
          all_ (dbBuilds db)

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
