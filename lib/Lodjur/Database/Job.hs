{-# LANGUAGE RankNTypes #-}

module Lodjur.Database.Job where

import           Lodjur.Database
import           Database.Beam
import qualified Database.Beam.Postgres        as Pg

data Job
  = BuildJob Build
  | DeployJob Deploy
  | CheckJob Check
  deriving (Show)

getJobs :: Pg.Connection -> Revision -> IO [Job]
getJobs conn rev = do
  builds  <- getBuilds conn rev
  deploys <- getDeploys conn rev
  checks  <- getChecks conn rev
  return $ map BuildJob builds
        ++ map DeployJob deploys
        ++ map CheckJob checks

insertBuilds :: Pg.Connection -> [Build] -> IO ()
insertBuilds conn bs =
  Pg.runBeamPostgresDebug putStrLn conn
    $ runInsert
    $ insert (dbBuilds db)
    $ insertValues bs

insertDeploys :: Pg.Connection -> [Deploy] -> IO ()
insertDeploys conn ds =
  Pg.runBeamPostgresDebug putStrLn conn
    $ runInsert
    $ insert (dbDeploys db)
    $ insertValues ds

insertChecks :: Pg.Connection -> [Check] -> IO ()
insertChecks conn cs =
  Pg.runBeamPostgresDebug putStrLn conn
    $ runInsert
    $ insert (dbChecks db)
    $ insertValues cs

insertCheckExamples :: Pg.Connection -> (forall a . [CheckExampleT a]) -> IO ()
insertCheckExamples conn ces =
  Pg.runBeamPostgresDebug putStrLn conn
    $ runInsert
    $ insert (dbCheckExamples db)
    $ insertExpressions ces

getBuilds :: Pg.Connection -> Revision -> IO [Build]
getBuilds conn rev =
  Pg.runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
    $ select
    $ filter_ (\a -> buildRevision a ==. val_ (pk rev))
    $ all_ (dbBuilds db)

getDeploys :: Pg.Connection -> Revision -> IO [Deploy]
getDeploys conn rev =
  Pg.runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
    $ select
    $ filter_ (\a -> deployRevision a ==. val_ (pk rev))
    $ all_ (dbDeploys db)

getChecks :: Pg.Connection -> Revision -> IO [Check]
getChecks conn rev =
  Pg.runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
    $ select
    $ filter_ (\a -> checkRevision a ==. val_ (pk rev))
    $ all_ (dbChecks db)
