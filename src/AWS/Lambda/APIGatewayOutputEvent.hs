{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

module AWS.Lambda.APIGatewayOutputEvent where

import Control.Lens
import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import GHC.Generics
import AWS.Lambda.Encoding

apiGatewayOutputEvent :: Text -> APIGatewayOutputEvent
apiGatewayOutputEvent = APIGatewayOutputEvent False 200 Nothing Nothing

data APIGatewayOutputEvent = APIGatewayOutputEvent
  { _isBase64Encoded   :: Bool
  , _statusCode        :: Int
  , _headers           :: Maybe (HashMap Text Text)
  , _multiValueHeaders :: Maybe (HashMap Text [Text])
  , _body              :: Text
  } deriving (Eq, Generic, Show)
    deriving LambdaDecode via (LambdaFromJSON APIGatewayOutputEvent)
    deriving LambdaEncode via (LambdaToJSON APIGatewayOutputEvent)

instance ToJSON APIGatewayOutputEvent where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON APIGatewayOutputEvent where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

makeLenses ''APIGatewayOutputEvent
