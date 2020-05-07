{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}

module AWS.Lambda.Event.APIGateway where

import AWS.Lambda.Encoding
import AWS.Lambda.Event.JSON
import Control.Lens
import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import GHC.Generics


data APIGatewayInputEvent = APIGatewayInputEvent
  { _resource                        :: Text
  , _path                            :: Text
  , _httpMethod                      :: Text
  , _headers                         :: Maybe (HashMap Text Text)
  , _multiValueHeaders               :: Maybe (HashMap Text [Text])
  , _queryStringParameters           :: Maybe (HashMap Text Text)
  , _multiValueQueryStringParameters :: Maybe (HashMap Text [Text])
  , _pathParameters                  :: Maybe (HashMap Text Text)
  , _stageVariables                  :: Maybe (HashMap Text Text)
  , _requestContext                  :: HashMap Text Value
  , _body                            :: Text
  , _isBase64Encoded                 :: Bool
  }
  deriving (Eq, Generic, Show)
  deriving LambdaDecode via (LambdaFromJSON APIGatewayInputEvent)
  deriving LambdaEncode via (LambdaToJSON APIGatewayInputEvent)

instance HasEventJSONOptions APIGatewayInputEvent
deriving via EventJSON APIGatewayInputEvent instance ToJSON APIGatewayInputEvent
deriving via EventJSON APIGatewayInputEvent instance FromJSON APIGatewayInputEvent


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

instance HasEventJSONOptions APIGatewayOutputEvent
deriving via EventJSON APIGatewayOutputEvent instance ToJSON APIGatewayOutputEvent
deriving via EventJSON APIGatewayOutputEvent instance FromJSON APIGatewayOutputEvent


makeFieldsNoPrefix ''APIGatewayInputEvent
makeFieldsNoPrefix ''APIGatewayOutputEvent
