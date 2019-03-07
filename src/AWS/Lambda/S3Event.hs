{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AWS.Lambda.S3Event
  (
    S3Event(..)
  ) where

import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import GHC.Generics

data S3 a =
  S3 {
    s3SchemaVersion :: Text,
    configurationId :: Text,
    bucket          :: Bucket a,
    s3Object        :: S3Object a
  } deriving (Show)

instance FromJSON a => FromJSON (S3 a) where
  parseJSON (Object v) = S3
    <$> v .: "s3SchemaVersion"
    <*> v .: "configurationId"
    <*> v .: "bucket"
    <*> ((v .: "object") >>= parseJSON)

instance ToJSON a => ToJSON (S3 a) where
  toJSON S3{..} =
    object [
        "s3SchemaVersion" .= s3SchemaVersion
      , "configurationId" .= configurationId
      , "bucket" .= bucket
      , "object" .= s3Object
    ]

data Bucket a =
  Bucket {
    name          :: Text,
    arn           :: Text,
    ownerIdentity :: Maybe (HashMap Text Text)
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (Bucket a)
instance ToJSON a => ToJSON (Bucket a)

data S3Object a =
  S3Object {
    key       :: Text,
    size      :: Maybe (Integer),
    eTag      :: Maybe (Text),
    versionId :: Text,
    sequencer :: Text
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (S3Object a)
instance ToJSON a => ToJSON (S3Object a)

data Record a =
  Record {
    eventVersion      :: Text,
    eventSource       :: Text,
    awsRegion         :: Text,
    eventTime         :: Text,
    eventName         :: Text,
    s3                :: S3 a,
    userIdentity      :: Maybe (HashMap Text Text),
    requestParameters :: Maybe (HashMap Text Text),
    responseElements  :: Maybe (HashMap Text Text),
    glacierEventData  :: Maybe (HashMap Text Value)
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (Record a)
instance ToJSON a => ToJSON (Record a)

data S3Event a =
  S3Event {
    records :: [Record a]
  }

instance FromJSON a => FromJSON (S3Event a) where
  parseJSON = withObject "S3Event" $
    \v -> S3Event
      <$> ((v .: "Records") >>= parseJSONList)

instance ToJSON a => ToJSON (S3Event a) where
  toJSON (S3Event{..}) =
    object ["records" .= records]
