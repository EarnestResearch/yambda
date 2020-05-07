{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}

module AWS.Lambda.Event.SNS where

import AWS.Lambda.Encoding
import AWS.Lambda.Event.JSON
import Control.Lens
import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import GHC.Generics


newtype SNSEvent = SNSEvent { _records :: [Record] }
  deriving (Eq, Generic, Show)
  deriving LambdaDecode via (LambdaFromJSON SNSEvent)
  deriving LambdaEncode via (LambdaToJSON SNSEvent)

instance HasEventJSONOptions SNSEvent where
  getEventJsonOptions = defaultOptions
    { fieldLabelModifier = capitalize . drop 1 }

deriving via EventJSON SNSEvent instance ToJSON SNSEvent
deriving via EventJSON SNSEvent instance FromJSON SNSEvent


data Record = Record
  { _eventVersion         :: Text
  , _eventSource          :: Text
  , _eventSubscriptionArn :: Text
  , _sns                  :: SNS
  } deriving (Eq, Generic, Show)

instance HasEventJSONOptions Record where
  getEventJsonOptions = defaultOptions
    { fieldLabelModifier = capitalize . drop 1 }

deriving via EventJSON Record instance ToJSON Record
deriving via EventJSON Record instance FromJSON Record


data SNS = SNS
  { _signatureVersion  :: Text
  , _timestamp         :: Text
  , _signature         :: Text
  , _signingCertUrl    :: Text
  , _messageId         :: Text
  , _message           :: Text
  , _messageAttributes :: Maybe (HashMap Text MessageAttribute)
  , _snsType           :: Text
  , _unsubscribeUrl    :: Text
  , _topicArn          :: Text
  , _subject           :: Maybe Text
  } deriving (Eq, Generic, Show)

instance HasEventJSONOptions SNS where
  getEventJsonOptions = defaultOptions
    { fieldLabelModifier =
      \case "_snsType"   -> "Type"
            v            -> capitalize . drop 1 $ v
    }

deriving via EventJSON SNS instance ToJSON SNS
deriving via EventJSON SNS instance FromJSON SNS


data MessageAttribute = MessageAttribute
  { _attributeType :: Text
  , _value         :: Text
  } deriving (Eq, Generic, Show)

instance HasEventJSONOptions MessageAttribute where
  getEventJsonOptions = defaultOptions
    { fieldLabelModifier =
      \case "_attributeType" -> "Type"
            v                -> capitalize . drop 1 $ v
    }

deriving via EventJSON MessageAttribute instance ToJSON MessageAttribute
deriving via EventJSON MessageAttribute instance FromJSON MessageAttribute


makeLenses ''SNSEvent
makeLenses ''Record
makeLenses ''SNS
makeLenses ''MessageAttribute
