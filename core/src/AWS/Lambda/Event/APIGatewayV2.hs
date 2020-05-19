{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}


{- | AWS API Gateway V2 models.

     Not a formal definition but close enough:
     https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-lambda.html

     Go models to cut/paste from:
     https://github.com/aws/aws-lambda-go/blob/master/events/apigw.go
-}
module AWS.Lambda.Event.APIGatewayV2 where

import           AWS.Lambda.Encoding
import           AWS.Lambda.Event.JSON
import           Control.Lens
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LB
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Base64 as TB64
import           Data.Word
import           GHC.Generics


{- AWS multi-value note:

  Format 2.0 doesn't have multiValueHeaders or multiValueQueryStringParameters
  fields. Duplicate headers are combined with commas and included in the headers
  field. Duplicate query strings are combined with commas and included in the
  queryStringParameters field.
-}

{- Binary parameter values:

   Can't be properly supported with the encoding chosen by AWS.
   E.g. query string represented as a JSON map only allows for UTF8-compatible
   values, any binary encoding will have to be external then.
-}

-- | Request sent by API GW
data HTTPRequest = HTTPRequest
    { _version               :: Text
    , _routeKey              :: Text
    , _rawPath               :: Text
    , _rawQueryString        :: Text
    , _cookies               :: Maybe [Text]
    , _headers               :: Map Text Text
    , _queryStringParameters :: Maybe (Map Text Text)
    , _pathParameters        :: Maybe (Map Text Text)
    , _requestContext        :: HTTPRequestContext
    , _stageVariables        :: Maybe (Map Text Text)
    , _body                  :: Maybe Text
    , _isBase64Encoded       :: Bool
    }
    deriving (Generic, Eq, Show)

instance HasEventJSONOptions HTTPRequest

deriving via EventJSON HTTPRequest
  instance ToJSON HTTPRequest

deriving via EventJSON HTTPRequest
  instance FromJSON HTTPRequest

deriving via LambdaFromJSON HTTPRequest
  instance LambdaDecode HTTPRequest

deriving via LambdaToJSON HTTPRequest
  instance LambdaEncode HTTPRequest


-- | Try to get request body as Text (base64 decodes if needed)
requestBodyText
  :: HTTPRequest
  -> Either Text Text
requestBodyText HTTPRequest{..} = do
  b <- maybe (Left "No body") Right _body
  if _isBase64Encoded
    then TB64.decodeBase64 b
    else Right b


-- | Try to get request body as bytes (base64 decodes if needed)
requestBodyBytes
  :: HTTPRequest
  -> Either Text B.ByteString
requestBodyBytes HTTPRequest{..} = do
  b <- maybe (Left "No body") Right _body
  if _isBase64Encoded
    then B64.decodeBase64 $ TE.encodeUtf8 b
    else Right $ TE.encodeUtf8 b


requestBodyJSON
  :: FromJSON a
  => HTTPRequest
  -> Either Text a
requestBodyJSON r@HTTPRequest{..} = do
  b <- requestBodyBytes r
  first T.pack $ eitherDecode $ LB.fromStrict b


data HTTPRequestContext = HTTPRequestContext
    { _routeKey     :: Text
    , _stage        :: Text
    , _accountId    :: Text
    , _requestId    :: Text
    , _authorizer   :: Maybe HTTPRequestContextAuthorizerDescription
    , _apiId        :: Text
    , _domainName   :: Text
    , _domainPrefix :: Text
    , _time         :: Text
    , _timeEpoch    :: Word64
    , _http         :: HTTPRequestContextHTTPDescription
    }
    deriving (Generic, Eq, Show)

instance HasEventJSONOptions HTTPRequestContext

deriving via EventJSON HTTPRequestContext
  instance FromJSON HTTPRequestContext

deriving via EventJSON HTTPRequestContext
  instance ToJSON HTTPRequestContext


newtype HTTPRequestContextAuthorizerDescription =
  HTTPRequestContextAuthorizerDescription
    { _jwt :: HTTPRequestContextAuthorizerJWTDescription
    }
    deriving (Generic, Eq, Show)

instance HasEventJSONOptions HTTPRequestContextAuthorizerDescription

deriving via EventJSON HTTPRequestContextAuthorizerDescription
  instance FromJSON HTTPRequestContextAuthorizerDescription

deriving via EventJSON HTTPRequestContextAuthorizerDescription
  instance ToJSON HTTPRequestContextAuthorizerDescription


data HTTPRequestContextAuthorizerJWTDescription
  = HTTPRequestContextAuthorizerJWTDescription
    { _claims :: Map Text Text
    , _scopes :: [Text]
    }
    deriving (Generic, Eq, Show)

instance HasEventJSONOptions HTTPRequestContextAuthorizerJWTDescription

deriving via EventJSON HTTPRequestContextAuthorizerJWTDescription
  instance FromJSON HTTPRequestContextAuthorizerJWTDescription

deriving via EventJSON HTTPRequestContextAuthorizerJWTDescription
  instance ToJSON HTTPRequestContextAuthorizerJWTDescription

data HTTPRequestContextHTTPDescription = HTTPRequestContextHTTPDescription
    { _method    :: Text
    , _path      :: Text
    , _protocol  :: Text
    , _sourceIp  :: Text
    , _userAgent :: Text
    }
    deriving (Generic, Eq, Show)

instance HasEventJSONOptions HTTPRequestContextHTTPDescription

deriving via EventJSON HTTPRequestContextHTTPDescription
  instance FromJSON HTTPRequestContextHTTPDescription

deriving via EventJSON HTTPRequestContextHTTPDescription
  instance ToJSON HTTPRequestContextHTTPDescription


-- | Lambda Response.
--   Note that API GW spec allows for simple 200 "some kind of JSON" responses,
--   so in general you don't have to wrap everything up into a response type
--   if that's all you need.
data HTTPResponse = HTTPResponse
    { _statusCode      :: Int
    , _headers         :: Maybe (Map Text Text)
    , _body            :: Maybe Text
    , _isBase64Encoded :: Maybe Bool
    , _cookies         :: Maybe [Text]
    }
    deriving (Generic, Eq, Show)

httpResponse
  :: Int
  -> HTTPResponse
httpResponse sc = HTTPResponse sc
  Nothing
  Nothing
  Nothing
  Nothing


instance HasEventJSONOptions HTTPResponse

deriving via EventJSON HTTPResponse
  instance ToJSON HTTPResponse

deriving via EventJSON HTTPResponse
  instance FromJSON HTTPResponse

deriving via LambdaFromJSON HTTPResponse
  instance LambdaDecode HTTPResponse

deriving via LambdaToJSON HTTPResponse
  instance LambdaEncode HTTPResponse


-- Generate TH lenses
makeFieldsNoPrefix ''HTTPRequest
makeFieldsNoPrefix ''HTTPRequestContext
makeFieldsNoPrefix ''HTTPRequestContextAuthorizerJWTDescription
makeFieldsNoPrefix ''HTTPRequestContextHTTPDescription
makeFieldsNoPrefix ''HTTPResponse
