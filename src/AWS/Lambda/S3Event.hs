{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AWS.Lambda.S3Event
  (
    S3Event(..),
    S3(..),
    S3Object(..),
    Bucket(..),
    Record(..)
  ) where

import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import GHC.Generics

data S3 =
  S3 {
    s3SchemaVersion :: Text,
    configurationId :: Text,
    bucket          :: Bucket,
    s3Object        :: S3Object
  } deriving (Show)

instance FromJSON S3 where
  parseJSON (Object v) = S3
    <$> v .: "s3SchemaVersion"
    <*> v .: "configurationId"
    <*> v .: "bucket"
    <*> ((v .: "object") >>= parseJSON)

instance ToJSON S3 where
  toJSON S3{..} =
    object [
        "s3SchemaVersion" .= s3SchemaVersion
      , "configurationId" .= configurationId
      , "bucket" .= bucket
      , "object" .= s3Object
    ]

data Bucket =
  Bucket {
    name          :: Text,
    arn           :: Text,
    ownerIdentity :: Maybe (HashMap Text Text)
  } deriving (Show, Generic)

instance FromJSON Bucket
instance ToJSON Bucket

data S3Object =
  S3Object {
    key       :: Text,
    size      :: Maybe Integer,
    eTag      :: Maybe Text,
    versionId :: Maybe Text,
    sequencer :: Text
  } deriving (Show, Generic)

instance FromJSON S3Object
instance ToJSON S3Object

data Record =
  Record {
    eventVersion      :: Text,
    eventSource       :: Text,
    awsRegion         :: Text,
    eventTime         :: Text,
    eventName         :: Text,
    s3                :: S3,
    userIdentity      :: Maybe (HashMap Text Text),
    requestParameters :: Maybe (HashMap Text Text),
    responseElements  :: Maybe (HashMap Text Text),
    glacierEventData  :: Maybe (HashMap Text Value)
  } deriving (Show, Generic)

instance FromJSON Record
instance ToJSON Record

data S3Event =
  S3Event {
    records :: [Record]
  }

instance FromJSON S3Event where
  parseJSON = withObject "S3Event" $
    \v -> S3Event
      <$> ((v .: "Records") >>= parseJSONList)

instance ToJSON S3Event where
  toJSON (S3Event{..}) =
    object ["Records" .= records]
