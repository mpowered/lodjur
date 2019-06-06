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
beam =
  runBeamPostgres
  -- runBeamPostgresDebug putStrLn

data DB f = DB
  { dbCommits    :: f (TableEntity CommitT)
  , dbJobs       :: f (TableEntity JobT)
  , dbRspecs     :: f (TableEntity RSpecT)
  , dbRspecTests :: f (TableEntity RSpecTestT)
  , dbLogs       :: f (TableEntity LogT)
  , dbUsers      :: f (TableEntity UserT)
  } deriving Generic

instance Database be DB

db :: DatabaseSettings be DB
db = defaultDbSettings

-- Commits

data CommitT f = Commit
  { commitId                :: C f Int32
  , commitSha               :: C f Text
  , commitOwner             :: C f Text
  , commitRepo              :: C f Text
  , commitBranch            :: C f (Maybe Text)
  , commitMessage           :: C f (Maybe Text)
  , commitAuthor            :: C f (Maybe Text)
  , commitAuthorEmail       :: C f (Maybe Text)
  , commitCommitter         :: C f (Maybe Text)
  , commitCommitterEmail    :: C f (Maybe Text)
  , commitTimestamp         :: C f (Maybe UTCTime)
  } deriving (Generic, Beamable)

instance Table CommitT where
  data PrimaryKey CommitT f = CommitKey (C f Int32) deriving (Generic, Beamable)
  primaryKey = CommitKey <$> commitId

type Commit = CommitT Identity
deriving instance Show Commit
deriving instance Show (PrimaryKey CommitT Identity)

-- Jobs

data JobT f = Job
  { jobId               :: C f Int32
  , jobName             :: C f Text
  , jobCommit           :: PrimaryKey CommitT f
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
  , rspecJob            :: PrimaryKey JobT f
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
  { rspectestId                 :: C f Int64
  , rspectestRSpec              :: PrimaryKey RSpecT f
  , rspectestDescription        :: C f Text
  , rspectestFullDescription    :: C f Text
  , rspectestStatus             :: C f Text
  , rspectestFilePath           :: C f Text
  , rspectestLineNumber         :: C f Int
  , rspectestExceptionClass     :: C f (Maybe Text)
  , rspectestExceptionMessage   :: C f (Maybe Text)
  , rspectestExceptionBacktrace :: C f (Maybe Text)
  } deriving (Generic, Beamable)

instance Table RSpecTestT where
  data PrimaryKey RSpecTestT f = RSpecTestKey (C f Int64) deriving (Generic, Beamable)
  primaryKey = RSpecTestKey <$> rspectestId

type RSpecTest = RSpecTestT Identity
deriving instance Show RSpecTest

-- Log output

data LogT f = Log
  { logId            :: C f Int64
  , logJob           :: PrimaryKey JobT f
  , logCreatedAt     :: C f UTCTime
  , logText          :: C f Text
  } deriving (Generic, Beamable)

instance Table LogT where
  data PrimaryKey LogT f = LogKey (C f Int64) deriving (Generic, Beamable)
  primaryKey = LogKey <$> logId

type Log = LogT Identity
deriving instance Show Log
deriving instance Show (PrimaryKey LogT Identity)

-- GitHub Users

data UserT f = User
  { userId           :: C f Int64
  , userLogin        :: C f Text
  , userName         :: C f (Maybe Text)
  , userEmail        :: C f (Maybe Text)
  , userCompany      :: C f (Maybe Text)
  , userLocation     :: C f (Maybe Text)
  , userAvatarUrl    :: C f (Maybe Text)
  , userAccessToken  :: C f (Maybe Text)
  , userCreatedAt    :: C f UTCTime
  , userUpdatedAt    :: C f UTCTime
  , userLastLogin    :: C f UTCTime
  } deriving (Generic, Beamable)

instance Table UserT where
  data PrimaryKey UserT f = UserKey (C f Int64) deriving (Generic, Beamable)
  primaryKey = UserKey <$> userId

type User = UserT Identity
deriving instance Show User
deriving instance Show (PrimaryKey UserT Identity)
