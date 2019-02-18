{-# LANGUAGE RankNTypes #-}

module Lodjur.Database.Revision where

import           Lodjur.DB
import           Lodjur.Git                     ( Hash )
import           Database.Beam
import qualified Database.Beam.Postgres        as Pg

insertRevisions :: Pg.Connection -> [Revision] -> IO ()
insertRevisions conn rs =
  Pg.runBeamPostgresDebug putStrLn conn
    $ runInsert
    $ insert (dbRevisions db)
    $ insertValues rs

lookupRevision :: Pg.Connection -> Hash -> IO (Maybe Revision)
lookupRevision conn revid =
  Pg.runBeamPostgresDebug putStrLn conn
    $ runSelectReturningOne
    $ select
    $ filter_ (\a -> revId a ==. val_ revid)
    $ all_ (dbRevisions db)

getRecentRevisions :: Pg.Connection -> Integer -> IO [Revision]
getRecentRevisions conn n =
  Pg.runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
    $ select
    $ limit_ n
    $ orderBy_ (desc_ . revTime)
    $ all_ (dbRevisions db)
