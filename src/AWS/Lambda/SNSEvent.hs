{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module AWS.Lambda.SNSEvent where

import           Control.Lens
import           Data.Aeson
import qualified Data.Char as C
import           Data.HashMap.Strict
import           Data.Text hiding (drop)
import           GHC.Generics

newtype SNSEvent = SNSEvent { _records :: [Record] }
  deriving (Eq, Generic, Show)

instance ToJSON SNSEvent where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modify }

instance FromJSON SNSEvent where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modify }

modify :: String -> String
modify "_snsType"       = "Type"
modify "_attributeType" = "Type"
modify v                = capitalize . drop 1 $ v

capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = C.toUpper x : xs

data Record = Record
  { _eventVersion         :: Text
  , _eventSource          :: Text
  , _eventSubscriptionArn :: Text
  , _sns                  :: SNS
  } deriving (Eq, Generic, Show)

instance ToJSON Record where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modify }

instance FromJSON Record where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modify }

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

instance ToJSON SNS where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modify }

instance FromJSON SNS where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modify }

data MessageAttribute = MessageAttribute
  { _attributeType :: Text
  , _value         :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON MessageAttribute where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modify }

instance FromJSON MessageAttribute where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modify }

makeLenses ''SNSEvent
makeLenses ''Record
makeLenses ''SNS
makeLenses ''MessageAttribute
