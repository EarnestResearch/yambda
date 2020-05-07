{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module AWS.Lambda.Event.APIGatewayV2Spec where

import           AWS.Lambda.Event.APIGatewayV2
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           GHC.Generics
import qualified Paths_aws_lambda_haskell_runtime_client as Paths
import           Test.Hspec


data MockBody = MockBody
  { bodykey1 :: Text
  , bodykey2 :: Int
  } deriving (Generic, Eq, Show, FromJSON, ToJSON)


spec :: Spec
spec = describe "JSON decoding" $ do
  -- note that we focus on decoding since Aeson API makes that
  -- symmetric with encoding

  it "decode request from file, text body" testDecodeTextBodyRequestFromFile
  it "decode request from file, JSON body" testDecodeJsonBodyRequestFromFile
  it "decode request from file, base64 body" testDecodeBase64BodyRequestFromFile

  it "decode response from file, text body" testDecodeTextBodyResponseFromFile
  it "decode response from file, JSON body" testDecodeJsonBodyResponseFromFile
  it "decode response from file, base64 body" testDecodeBase64BodyResponseFromFile


testDecodeTextBodyRequestFromFile :: Expectation
testDecodeTextBodyRequestFromFile = do
  fname <- Paths.getDataFileName "api-gateway-v2-request-text-body.json"
  reqBytes <- LB.readFile fname
  let Just req = decode @(HTTPRequest Text) reqBytes
  req `shouldBe` mockRequest "Hello from Lambda"


testDecodeJsonBodyRequestFromFile :: Expectation
testDecodeJsonBodyRequestFromFile = do
  fname <- Paths.getDataFileName "api-gateway-v2-request-json-body.json"
  reqBytes <- LB.readFile fname
  let Just req = decode @(HTTPRequest MockBody) reqBytes
  req `shouldBe` mockRequest (MockBody "bodyval" 123)


testDecodeBase64BodyRequestFromFile :: Expectation
testDecodeBase64BodyRequestFromFile = do
  fname <- Paths.getDataFileName "api-gateway-v2-request-base64-body.json"
  reqBytes <- LB.readFile fname
  let Just req = decode @(HTTPRequest Base64EncodedBody) reqBytes
  req `shouldBe` (mockRequest (Base64EncodedBody "base64val") & isBase64Encoded .~ True)


testDecodeTextBodyResponseFromFile :: Expectation
testDecodeTextBodyResponseFromFile = do
  fname <- Paths.getDataFileName "api-gateway-v2-response-text-body.json"
  reqBytes <- LB.readFile fname
  let Just req = decode @(HTTPResponse Text) reqBytes
  req `shouldBe` mockResponse "Hello from Lambda!"


testDecodeJsonBodyResponseFromFile :: Expectation
testDecodeJsonBodyResponseFromFile = do
  fname <- Paths.getDataFileName "api-gateway-v2-response-json-body.json"
  reqBytes <- LB.readFile fname
  let Just req = decode @(HTTPResponse MockBody) reqBytes
  req `shouldBe` mockResponse (MockBody "bodyval" 123)


testDecodeBase64BodyResponseFromFile :: Expectation
testDecodeBase64BodyResponseFromFile = do
  fname <- Paths.getDataFileName "api-gateway-v2-response-base64-body.json"
  reqBytes <- LB.readFile fname
  let Just req = decode @(HTTPResponse Base64EncodedBody) reqBytes
  req `shouldBe` (mockResponse (Base64EncodedBody "base64val") & isBase64Encoded ?~ True)


mockRequest
  :: b
  -> HTTPRequest b
mockRequest b = HTTPRequest
  { _version = "2.0"
  , _routeKey = "$default"
  , _rawPath = "/my/path"
  , _rawQueryString = "parameter1=value1&parameter1=value2&parameter2=value"
  , _cookies = ["cookie1","cookie2"]
  , _headers = Map.fromList [("Header1","value1"),("Header2","value2")]
  , _queryStringParameters = Map.fromList [("parameter1","value1,value2"),("parameter2","value")]
  , _pathParameters = Map.fromList [("parameter1","value1")]
  , _requestContext = HTTPRequestContext
    { _routeKey = "$default"
    , _stage = "$default"
    , _accountId = "123456789012"
    , _requestId = "id"
    , _authorizer = Just (HTTPRequestContextAuthorizerDescription
      { _jwt = HTTPRequestContextAuthorizerJWTDescription
        { _claims = Map.fromList [("claim1", "value1"), ("claim2", "value2")]
        , _scopes = ["scope1", "scope2"]
        }
      })
    , _apiId = "api-id"
    , _domainName = "id.execute-api.us-east-1.amazonaws.com"
    , _domainPrefix = "id"
    , _time = "12/Mar/2020:19:03:58 +0000"
    , _timeEpoch = 1583348638390
    , _http = HTTPRequestContextHTTPDescription
      { _method = "POST"
      , _path = "/my/path"
      , _protocol = "HTTP/1.1"
      , _sourceIp = "IP"
      , _userAgent = "agent"
      }
    }
  , _stageVariables = Map.fromList [("stageVariable1","value1"), ("stageVariable2","value2")]
  , _body = b
  , _isBase64Encoded = False
  }


mockResponse
  :: b
  -> HTTPResponse b
mockResponse b = HTTPResponse
  { _statusCode = 200
  , _headers = Just $ Map.fromList [ ("header1", "headerval1"), ("header2", "headerval2") ]
  , _multiValueHeaders = Just $ Map.fromList [ ("mvheader1", ["mvh1", "mvh2"]) ]
  , _body = Just b
  , _isBase64Encoded = Just False
  , _cookies = Just ["cookie1", "cookie2"]
  }
