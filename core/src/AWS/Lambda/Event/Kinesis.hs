{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module AWS.Lambda.Event.Kinesis where

import AWS.Lambda.Encoding
import AWS.Lambda.Event.JSON
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


instance HasEventJSONOptions Kinesis where
  getEventJsonOptions = defaultOptions
    { fieldLabelModifier =
        \case "_payload" -> "data"
              k          -> drop 1 k
    }

deriving via EventJSON Kinesis instance ToJSON Kinesis
deriving via EventJSON Kinesis instance FromJSON Kinesis


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

instance HasEventJSONOptions Record
deriving via EventJSON Record instance ToJSON Record
deriving via EventJSON Record instance FromJSON Record


newtype KinesisDataStreamsEvent = KinesisDataStreamsEvent { _records :: [Record] }
  deriving (Eq, Generic, Show)
  deriving LambdaDecode via (LambdaFromJSON KinesisDataStreamsEvent)
  deriving LambdaEncode via (LambdaToJSON KinesisDataStreamsEvent)

instance HasEventJSONOptions KinesisDataStreamsEvent where
  getEventJsonOptions = defaultOptions
    { fieldLabelModifier =
        \case "_records" -> "Records"
              k          -> drop 1 k
    }

deriving via EventJSON KinesisDataStreamsEvent instance ToJSON KinesisDataStreamsEvent
deriving via EventJSON KinesisDataStreamsEvent instance FromJSON KinesisDataStreamsEvent


makeLenses ''Kinesis
makeLenses ''Record
makeLenses ''KinesisDataStreamsEvent
