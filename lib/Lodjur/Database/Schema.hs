{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lodjur.Database.Schema where

import           Data.Aeson                     ( Value )
import           Data.Int                       ( Int32
                                                , Int64
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Database.Beam
import           Database.Beam.Postgres

import           Lodjur.Database.Enum
import           Lodjur.Job

beam :: Connection -> Pg a -> IO a
beam = runBeamPostgres
    -- runBeamPostgresDebug putStrLn

data DB f = DB
  { jobs        :: f (TableEntity JobT)
  , rspecs      :: f (TableEntity RSpecT)
  , rspecsTests :: f (TableEntity RSpecTestT)
  } deriving Generic

instance Database be DB

db :: DatabaseSettings be DB
db = defaultDbSettings

-- Jobs

data JobT f = JobT
  { jobId               :: C f Int32
  , jobName             :: C f Text
  , jobSrcSha           :: C f Text
  , jobSrcBranch        :: C f (Maybe Text)
  , jobSrcOwner         :: C f Text
  , jobSrcRepo          :: C f Text
  , jobSrcMessage       :: C f (Maybe Text)
  , jobSrcCommitter     :: C f (Maybe Text)
  , jobAction           :: C f Value
  , jobStatus           :: C f (DbEnum Status)
  , jobConclusion       :: C f (Maybe (DbEnum Conclusion))
  , jobCreatedAt        :: C f UTCTime
  , jobStartedAt        :: C f (Maybe UTCTime)
  , jobCompletedAt      :: C f (Maybe UTCTime)
  , jobParent           :: PrimaryKey JobT (Nullable f)
  } deriving (Generic, Beamable)

instance Table JobT where
  data PrimaryKey JobT f = JobKey (C f Int32) deriving (Generic, Beamable)
  primaryKey = JobKey <$> jobId

type Job = JobT Identity
deriving instance Show Job
deriving instance Show (PrimaryKey JobT Identity)
deriving instance Show (PrimaryKey JobT (Nullable Identity))

-- RSpec Results

data RSpecT f = RSpec
  { rspecId             :: C f Int32
  , rspecDuration       :: C f Double
  , rspecExampleCount   :: C f Int
  , rspecFailureCount   :: C f Int
  , rspecPendingCount   :: C f Int
  } deriving (Generic, Beamable)

instance Table RSpecT where
  data PrimaryKey RSpecT f = RSpecKey (C f Int32) deriving (Generic, Beamable)
  primaryKey = RSpecKey <$> rspecId

type RSpec = RSpecT Identity
deriving instance Show RSpec
deriving instance Show (PrimaryKey RSpecT Identity)

data RSpecTestT f = RSpecTest
  { rspectestId                :: C f Int64
  , rspectestRSpec             :: PrimaryKey RSpecT f
  , rspectestDescription       :: C f Text
  , rspectestFullDescription   :: C f Text
  , rspectestStatus            :: C f Text
  , rspectestFilePath          :: C f Text
  , rspectestLineNumber        :: C f Int
  , rspectestException         :: C f Value
  } deriving (Generic, Beamable)

instance Table RSpecTestT where
  data PrimaryKey RSpecTestT f = RSpecTestKey (C f Int64) deriving (Generic, Beamable)
  primaryKey = RSpecTestKey <$> rspectestId

type RSpecTest = RSpecTestT Identity
deriving instance Show RSpecTest
