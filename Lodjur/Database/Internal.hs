{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lodjur.Database.Internal where

import           Data.Aeson                             (Value)
import           Data.Int                               (Int32)
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import           Data.Time.Clock                        (UTCTime)
import           Data.UUID                              (UUID)
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Lodjur.Git                             (Hash)

class DbEnum a where
  enumToText :: a -> Text
  enumFromText :: Text -> Either Text a

toDbEnumField :: DbEnum a => a -> Action
toDbEnumField = toField . enumToText

fromDbEnumField :: (Typeable a, DbEnum a) => FieldParser a
fromDbEnumField f mdata =
  enumFromText <$> fromField f mdata >>= \case
    Left bad -> returnError ConversionFailed f (Text.unpack bad)
    Right ok -> return ok

data DB f = DB
  { dbRevisions     :: f (TableEntity RevisionT)
  , dbBuilds        :: f (TableEntity BuildT)
  , dbDeploys       :: f (TableEntity DeployT)
  , dbChecks        :: f (TableEntity CheckT)
  , dbCheckExamples :: f (TableEntity CheckExampleT)
  } deriving Generic

instance Database be DB

db :: DatabaseSettings be DB
db = defaultDbSettings

-- Revisions

data RevisionT f = Revision
  { revId           :: C f Hash
  , revTime         :: C f UTCTime
  } deriving (Generic, Beamable)

instance Table RevisionT where
  data PrimaryKey RevisionT f = RevisionKey (C f Hash) deriving (Generic, Beamable)
  primaryKey = RevisionKey <$> revId

type Revision = RevisionT Identity
deriving instance Show Revision
deriving instance Show (PrimaryKey RevisionT Identity)

-- Jobs

data JobStatus
  = JobRunning
  | JobSuccess
  | JobFailure
  deriving (Show, Read, Eq, Ord, Enum)

instance DbEnum JobStatus where
  enumFromText "running" = Right JobRunning
  enumFromText "success" = Right JobSuccess
  enumFromText "fail"    = Right JobFailure
  enumFromText bad       = Left bad

  enumToText JobRunning  = "running"
  enumToText JobSuccess  = "success"
  enumToText JobFailure  = "fail"

instance FromField JobStatus where
  fromField = fromDbEnumField

instance ToField JobStatus where
  toField = toDbEnumField

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be JobStatus where
  sqlValueSyntax = sqlValueSyntax . enumToText

instance (BackendFromField be JobStatus, BeamBackend be) => FromBackendRow be JobStatus

data BuildT f = Build
  { buildId             :: C f UUID
  , buildRevision       :: PrimaryKey RevisionT f
  , buildStartTime      :: C f UTCTime
  , buildFinishTime     :: C f UTCTime
  , buildStatus         :: C f JobStatus
  , buildStartedBy      :: C f Text
  } deriving (Generic, Beamable)

instance Table BuildT where
  data PrimaryKey BuildT f = BuildKey (C f UUID) deriving (Generic, Beamable)
  primaryKey = BuildKey <$> buildId

type Build = BuildT Identity
deriving instance Show Build

data DeployT f = Deploy
  { deployId            :: C f UUID
  , deployRevision      :: PrimaryKey RevisionT f
  , deployStartTime     :: C f UTCTime
  , deployFinishTime    :: C f UTCTime
  , deployStatus        :: C f JobStatus
  , deployStartedBy     :: C f Text
  , deployTarget        :: C f Text
  } deriving (Generic, Beamable)

instance Table DeployT where
  data PrimaryKey DeployT f = DeployKey (C f UUID) deriving (Generic, Beamable)
  primaryKey = DeployKey <$> deployId

type Deploy = DeployT Identity
deriving instance Show Deploy

data CheckT f = Check
  { checkId             :: C f UUID
  , checkRevision       :: PrimaryKey RevisionT f
  , checkStartTime      :: C f UTCTime
  , checkFinishTime     :: C f UTCTime
  , checkStatus         :: C f JobStatus
  , checkStartedBy      :: C f Text
  , checkApplication    :: C f Text
  , checkExamples       :: C f Int32
  , checkFailed         :: C f Int32
  , checkPending        :: C f Int32
  , checkDuration       :: C f Double
  } deriving (Generic, Beamable)

instance Table CheckT where
  data PrimaryKey CheckT f = CheckKey (C f UUID) deriving (Generic, Beamable)
  primaryKey = CheckKey <$> checkId

type Check = CheckT Identity
deriving instance Show Check
deriving instance Show (PrimaryKey CheckT Identity)

data CheckExampleT f = CheckExample
  { egId                :: C f Int32
  , egCheck             :: PrimaryKey CheckT f
  , egDescription       :: C f Text
  , egFullDescription   :: C f Text
  , egStatus            :: C f Text
  , egFilePath          :: C f Text
  , egLineNumber        :: C f Int32
  , egException         :: C f Value
  } deriving (Generic, Beamable)

instance Table CheckExampleT where
  data PrimaryKey CheckExampleT f = CheckExampleKey (C f Int32) deriving (Generic, Beamable)
  primaryKey = CheckExampleKey <$> egId

type CheckExample = CheckExampleT Identity
deriving instance Show CheckExample
