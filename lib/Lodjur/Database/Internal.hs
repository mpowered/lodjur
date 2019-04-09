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

module Lodjur.Database.Internal
  ( module Lodjur.Database.Internal
  , module Database.Beam
  , Connection
  )
where

import           Data.Aeson                             (Value)
import           Data.Int                               (Int64)
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import           Data.Time.Clock                        (UTCTime)
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField

import qualified GitHub.Extra                  as GH

beam :: Connection -> Pg a -> IO a
beam = runBeamPostgres
--runBeamPostgres = runBeamPostgresDebug putStrLn

class TextEnum a where
  enumToText :: a -> Text
  enumFromText :: Text -> Either Text a

instance TextEnum GH.CheckStatus where
  enumToText GH.Queued       = "queued"
  enumToText GH.InProgress   = "in_progress"
  enumToText GH.Completed    = "completed"

  enumFromText "queued"      = return GH.Queued
  enumFromText "in_progress" = return GH.InProgress
  enumFromText "completed"   = return GH.Completed
  enumFromText bad           = Left ("Bad CheckStatus '" <> bad <> "'")

instance TextEnum GH.Conclusion where
  enumToText GH.Success          = "success"
  enumToText GH.Failure          = "failure"
  enumToText GH.Neutral          = "neutral"
  enumToText GH.Cancelled        = "cancelled"
  enumToText GH.TimedOut         = "timed_out"
  enumToText GH.ActionRequired   = "action_required"

  enumFromText "success"         = return GH.Success
  enumFromText "failure"         = return GH.Failure
  enumFromText "neutral"         = return GH.Neutral
  enumFromText "cancelled"       = return GH.Cancelled
  enumFromText "timed_out"       = return GH.TimedOut
  enumFromText "action_required" = return GH.ActionRequired
  enumFromText bad               = Left ("Bad Conclusion '" <> bad <> "'")

toDbEnumField :: TextEnum a => a -> Action
toDbEnumField = toField . enumToText

fromDbEnumField :: (Typeable a, TextEnum a) => FieldParser a
fromDbEnumField f mdata =
  enumFromText <$> fromField f mdata >>= \case
    Left bad -> returnError ConversionFailed f (Text.unpack bad)
    Right ok -> return ok

newtype DbEnum a = DbEnum { unDbEnum :: a }
  deriving (Eq, Ord, Show)

instance (Typeable a, TextEnum a) => FromField (DbEnum a) where
  fromField f mdata = DbEnum <$> fromDbEnumField f mdata

instance TextEnum a => ToField (DbEnum a) where
  toField = toDbEnumField . unDbEnum

instance (TextEnum a, HasSqlValueSyntax be Text) => HasSqlValueSyntax be (DbEnum a) where
  sqlValueSyntax = sqlValueSyntax . enumToText . unDbEnum

instance (BackendFromField be (DbEnum a), BeamBackend be) => FromBackendRow be (DbEnum a)

data DB f = DB
  { dbEvents        :: f (TableEntity EventT)
  , dbCheckRuns     :: f (TableEntity CheckRunT)
  , dbCheckSuites   :: f (TableEntity CheckSuiteT)
  } deriving Generic

instance Database be DB

db :: DatabaseSettings be DB
db = defaultDbSettings

-- Events

data EventT f = Event
  { eventId         :: C f Int64
  , eventSource     :: C f Text
  , eventDelivery   :: C f (Maybe Text)
  , eventType       :: C f Text
  , eventCreatedAt  :: C f UTCTime
  , eventData       :: C f Value
  } deriving (Generic, Beamable)

instance Table EventT where
  data PrimaryKey EventT f = EventKey (C f Int64) deriving (Generic, Beamable)
  primaryKey = EventKey <$> eventId

type Event = EventT Identity
deriving instance Show Event
deriving instance Show (PrimaryKey EventT Identity)

-- Check Suites

data CheckSuiteT f = CheckSuite
  { checksuiteId                :: C f Int
  , checksuiteRepositoryOwner   :: C f Text
  , checksuiteRepositoryName    :: C f Text
  , checksuiteHeadSha           :: C f Text
  , checksuiteStatus            :: C f (DbEnum GH.CheckStatus)
  , checksuiteConclusion        :: C f (Maybe (DbEnum GH.Conclusion))
  , checksuiteStartedAt         :: C f (Maybe UTCTime)
  , checksuiteCompletedAt       :: C f (Maybe UTCTime)
  } deriving (Generic, Beamable)

instance Table CheckSuiteT where
  data PrimaryKey CheckSuiteT f = CheckSuiteKey (C f Int) deriving (Generic, Beamable)
  primaryKey = CheckSuiteKey <$> checksuiteId

type CheckSuite = CheckSuiteT Identity
deriving instance Show CheckSuite
deriving instance Show (PrimaryKey CheckSuiteT Identity)

-- Check Runs

data CheckRunT f = CheckRun
  { checkrunId                :: C f Int
  , checkrunCheckSuite        :: PrimaryKey CheckSuiteT f
  , checkrunName              :: C f Text
  , checkrunStatus            :: C f (DbEnum GH.CheckStatus)
  , checkrunConclusion        :: C f (Maybe (DbEnum GH.Conclusion))
  , checkrunStartedAt         :: C f (Maybe UTCTime)
  , checkrunCompletedAt       :: C f (Maybe UTCTime)
  } deriving (Generic, Beamable)

instance Table CheckRunT where
  data PrimaryKey CheckRunT f = CheckRunKey (C f Int) deriving (Generic, Beamable)
  primaryKey = CheckRunKey <$> checkrunId

type CheckRun = CheckRunT Identity
deriving instance Show CheckRun
deriving instance Show (PrimaryKey CheckRunT Identity)

{-
  , checkSuiteApplication    :: C f Text
  , checkSuiteExamples       :: C f Int32
  , checkSuiteFailed         :: C f Int32
  , checkSuitePending        :: C f Int32
  , checkSuiteDuration       :: C f Double
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
-}
