{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module AWS.Lambda.S3Event where

import Control.Lens
import Data.Aeson
import Data.HashMap.Strict
import Data.Text hiding (drop)
import GHC.Generics


newtype S3Event = S3Event { _records :: [Record] }
  deriving (Eq, Generic, Show)

instance ToJSON S3Event where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modify }

instance FromJSON S3Event where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modify }

modify :: String -> String
modify "_records" = "Records"
modify k          = drop 1 k


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

instance ToJSON Record where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Record where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


data S3 = S3
  { _s3SchemaVersion :: Text
  , _configurationId :: Text
  , _bucket          :: Bucket
  , _s3Object        :: S3Object
  } deriving (Eq, Generic, Show)

instance ToJSON S3 where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modify' }

instance FromJSON S3 where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modify' }

modify':: String -> String
modify' "_s3Object" = "object"
modify' k           = drop 1 k


data Bucket = Bucket
  { _name          :: Text
  , _arn           :: Text
  , _ownerIdentity :: Maybe (HashMap Text Text)
  } deriving (Eq, Generic, Show)

instance ToJSON Bucket where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Bucket where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


data S3Object = S3Object
  { _key       :: Text
  , _size      :: Maybe Integer
  , _eTag      :: Maybe Text
  , _versionId :: Maybe Text
  , _sequencer :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON S3Object where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON S3Object where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


makeLenses ''S3Event
makeLenses ''Record
makeLenses ''S3
makeLenses ''Bucket
makeLenses ''S3Object
