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

import           AWS.Lambda.Event.JSON
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LB
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
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
data HTTPRequest b = HTTPRequest
    { _version               :: Text
    , _routeKey              :: Text
    , _rawPath               :: Text
    , _rawQueryString        :: Text
    , _cookies               :: [Text]
    , _headers               :: Map Text Text
    , _queryStringParameters :: Map Text Text
    , _pathParameters        :: Map Text Text
    , _requestContext        :: HTTPRequestContext
    , _stageVariables        :: Map Text Text
      -- | Some JSON-representable body.
      --   You can use any custom JSON-encoded type if you know
      --   you'll be getting JSON.
      --   Use 'Base64EncodedBody' if you expect a base64 encoded binary.
      --   If encoding can't be known upfront and you need to use
      --   the value of '_isBase64Encoded' flag use 'Text' and add
      --   custom conditional decoding.
    , _body                  :: b
    , _isBase64Encoded       :: Bool
    }
    deriving (Generic, Eq, Show)

instance HasEventJSONOptions (HTTPRequest b)

deriving via EventJSON (HTTPRequest b)
  instance (ToJSON b => ToJSON (HTTPRequest b))

deriving via EventJSON (HTTPRequest b)
  instance (FromJSON b => FromJSON (HTTPRequest b))


-- | Request body passed as a base64 encoded string
newtype Base64EncodedBody
  = Base64EncodedBody LB.ByteString
  deriving (Generic, Eq, Show)

instance ToJSON Base64EncodedBody where
  toEncoding (Base64EncodedBody b)
    = toEncoding
    $ B64.encodeBase64 (LB.toStrict b)

  toJSON (Base64EncodedBody b)
    = toJSON
    $ B64.encodeBase64 (LB.toStrict b)

instance FromJSON Base64EncodedBody where
  parseJSON v = do
    jVal <- parseJSON v
    let err = typeMismatch "base64 encoded string" v

    case jVal
      of String txt ->
          case B64.decodeBase64 (TE.encodeUtf8 txt)
            of Left _  -> err
               Right b -> pure $ Base64EncodedBody (LB.fromStrict b)
         _ -> err


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
    , _timeEpoch    :: Int
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
data HTTPResponse b = HTTPResponse
    { _statusCode        :: Int
    , _headers           :: Maybe (Map Text Text)
    , _multiValueHeaders :: Maybe (Map Text [Text])
    -- NOTE: AWS docs don't mention empty body but it may be needed for 204
    , _body              :: Maybe b
    , _isBase64Encoded   :: Maybe Bool
    , _cookies           :: Maybe [Text]
    }
    deriving (Generic, Eq, Show)

instance HasEventJSONOptions (HTTPResponse b)

deriving via EventJSON (HTTPResponse b)
  instance (ToJSON b => ToJSON (HTTPResponse b))

deriving via EventJSON (HTTPResponse b)
  instance (FromJSON b => FromJSON (HTTPResponse b))


-- Generate TH lenses
makeFieldsNoPrefix ''HTTPRequest
makeFieldsNoPrefix ''HTTPRequestContext
makeFieldsNoPrefix ''HTTPRequestContextAuthorizerJWTDescription
makeFieldsNoPrefix ''HTTPRequestContextHTTPDescription
makeFieldsNoPrefix ''HTTPResponse
