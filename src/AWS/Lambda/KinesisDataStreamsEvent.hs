{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}

module AWS.Lambda.KinesisDataStreamsEvent where

import Control.Lens
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data Kinesis = Kinesis
  { _partitionKey         :: Text
  , _payload              :: Text
  , _kinesisSchemaVersion :: Text
  , _sequenceNumber       :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON Kinesis where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modify }

instance FromJSON Kinesis where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modify }

modify :: String -> String
modify "_payload" = "data"
modify k          = drop 1 k


data Record = Record
  { _eventID           :: Text
  , _eventVersion      :: Text
  , _kinesis           :: Kinesis
  , _invokeIdentityArn :: Text
  , _eventName         :: Text
  , _eventSourceARN    :: Text
  , _eventSource       :: Text
  , _awsRegion         :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON Record where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Record where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


newtype KinesisDataStreamsEvent = KinesisDataStreamsEvent { _records :: [Record] }
  deriving (Eq, Generic, Show)
  deriving LambdaEncode via (LambdaFromJSON KinesisDataStreamsEvent)
  deriving LambdaDecode via (LambdaToJSON KinesisDataStreamsEvent)

instance ToJSON KinesisDataStreamsEvent where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modify' }

instance FromJSON KinesisDataStreamsEvent where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modify' }

modify' :: String -> String
modify' "_records" = "Records"
modify' k          = drop 1 k


makeLenses ''Kinesis
makeLenses ''Record
makeLenses ''KinesisDataStreamsEvent
