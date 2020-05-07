{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module AWS.Lambda.Event.S3 where

import AWS.Lambda.Encoding
import AWS.Lambda.Event.JSON
import Control.Lens
import Data.Aeson
import Data.HashMap.Strict
import Data.Text hiding (drop)
import GHC.Generics

newtype S3Event = S3Event { _records :: [Record] }
  deriving (Eq, Generic, Show)
  deriving LambdaDecode via (LambdaFromJSON S3Event)
  deriving LambdaEncode via (LambdaToJSON S3Event)

instance HasEventJSONOptions S3Event where
  getEventJsonOptions = defaultOptions
    { fieldLabelModifier =
        \case "_records" -> "Records"
              k          -> drop 1 k
    }

deriving via EventJSON S3Event instance ToJSON S3Event
deriving via EventJSON S3Event instance FromJSON S3Event


data Record = Record
  { _eventVersion      :: Text
  , _eventSource       :: Text
  , _awsRegion         :: Text
  , _eventTime         :: Text
  , _eventName         :: Text
  , _s3                :: S3
  , _userIdentity      :: Maybe (HashMap Text Text)
  , _requestParameters :: Maybe (HashMap Text Text)
  , _responseElements  :: Maybe (HashMap Text Text)
  , _glacierEventData  :: Maybe (HashMap Text Value)
  } deriving (Eq, Generic, Show)

instance HasEventJSONOptions Record
deriving via EventJSON Record instance ToJSON Record
deriving via EventJSON Record instance FromJSON Record


data S3 = S3
  { _s3SchemaVersion :: Text
  , _configurationId :: Text
  , _bucket          :: Bucket
  , _s3Object        :: S3Object
  } deriving (Eq, Generic, Show)

instance HasEventJSONOptions S3 where
  getEventJsonOptions = defaultOptions
    { fieldLabelModifier =
        \case "_s3Object" -> "object"
              k           -> drop 1 k
    }

deriving via EventJSON S3 instance ToJSON S3
deriving via EventJSON S3 instance FromJSON S3


data Bucket = Bucket
  { _name          :: Text
  , _arn           :: Text
  , _ownerIdentity :: Maybe (HashMap Text Text)
  } deriving (Eq, Generic, Show)

instance HasEventJSONOptions Bucket
deriving via EventJSON Bucket instance ToJSON Bucket
deriving via EventJSON Bucket instance FromJSON Bucket


data S3Object = S3Object
  { _key       :: Text
  , _size      :: Maybe Integer
  , _eTag      :: Maybe Text
  , _versionId :: Maybe Text
  , _sequencer :: Text
  } deriving (Eq, Generic, Show)

instance HasEventJSONOptions S3Object
deriving via EventJSON S3Object instance ToJSON S3Object
deriving via EventJSON S3Object instance FromJSON S3Object


makeLenses ''S3Event
makeLenses ''Record
makeLenses ''S3
makeLenses ''Bucket
makeLenses ''S3Object
