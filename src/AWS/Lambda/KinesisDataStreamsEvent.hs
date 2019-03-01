{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AWS.Lambda.KinesisDataStreamsEvent
  (
    KinesisDataStreamsEvent(..)
  ) where

import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import GHC.Generics

data Kinesis a =
  Kinesis {
    partitionKey         :: Text,
    payload              :: a,
    kinesisSchemaVersion :: Text,
    sequenceNumber       :: Text
  } deriving (Show)

instance FromJSON a => FromJSON (Kinesis a) where
  parseJSON (Object v) = Kinesis
    <$> v .: "partitionKey"
    <*> v .: "data"
    <*> v .: "kinesisSchemaVersion"
    <*> v .: "sequenceNumber"

instance ToJSON a => ToJSON (Kinesis a) where
  toJSON Kinesis{..} =
    object [
        "partitionKey" .= partitionKey
      , "data" .= payload
      , "kinesisSchemaVersion" .= kinesisSchemaVersion
      , "sequenceNumber" .= sequenceNumber
    ]

data Record a =
  Record {
    eventID           :: Text,
    eventVersion      :: Text,
    kinesis           :: Kinesis a,
    invokeIdentityArn :: Text,
    eventName         :: Text,
    eventSourceARN    :: Text,
    eventSource       :: Text,
    awsRegion         :: Text
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (Record a)
instance ToJSON a => ToJSON (Record a)

data KinesisDataStreamsEvent a =
  KinesisDataStreamsEvent {
    records :: [Record a]
  }

instance FromJSON a => FromJSON (KinesisDataStreamsEvent a) where
  parseJSON = withObject "KinesisDataStreamsEvent" $
    \v -> KinesisDataStreamsEvent
      <$> ((v .: "Records") >>= parseJSONList)

instance ToJSON a => ToJSON (KinesisDataStreamsEvent a) where
  toJSON (KinesisDataStreamsEvent{..}) =
    object ["records" .= records]

