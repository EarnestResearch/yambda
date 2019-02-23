{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module AWS.Lambda.KinesisDataStreamsEvent
  (
    KinesisDataStreamsEvent(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.HashMap.Strict
import GHC.Generics

data Kinesis =
  Kinesis {
    partitionKey :: Text,
    payload :: Text,
    kinesisSchemaVersion :: Text,
    sequenceNumber :: Text
  } deriving (Show)

instance FromJSON Kinesis where
  parseJSON (Object v) = Kinesis
    <$> v .: "partitionKey"
    <*> v .: "data"
    <*> v .: "kinesisSchemaVersion"
    <*> v .: "sequenceNumber"

instance ToJSON Kinesis where
  toJSON Kinesis{..} =
    object [
        "partitionKey" .= partitionKey
      , "data" .= payload
      , "kinesisSchemaVersion" .= kinesisSchemaVersion
      , "sequenceNumber" .= sequenceNumber
    ]

data Record =
  Record {
    eventID :: Text,
    eventVersion :: Text,
    kinesis :: Kinesis,
    invokeIdentityArn :: Text,
    eventName :: Text,
    eventSourceARN :: Text,
    eventSource :: Text,
    awsRegion :: Text
  } deriving (Show, Generic)

instance FromJSON Record
instance ToJSON Record

data KinesisDataStreamsEvent = 
  KinesisDataStreamsEvent { 
    records :: [Record] 
  }

instance FromJSON KinesisDataStreamsEvent where
  parseJSON = withObject "KinesisDataStreamsEvent" $ 
    \v -> KinesisDataStreamsEvent 
      <$> ((v .: "Records") >>= parseJSONList)

instance ToJSON KinesisDataStreamsEvent where
  toJSON (KinesisDataStreamsEvent{..}) = 
    object ["records" .= records]

