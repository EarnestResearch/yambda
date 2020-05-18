{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module AWS.Lambda.Event.S3Spec where


import           AWS.Lambda.Event.S3
import           Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as Map
import qualified Paths_yambda as Paths
import           Test.Hspec


spec :: Spec
spec = describe "JSON encoding" $
  it "parse S3 delete event" testS3Delete


testS3Delete :: Expectation
testS3Delete = do
  fname <- Paths.getDataFileName "s3-delete.json"
  reqBytes <- LB.readFile fname
  decode @S3Event reqBytes `shouldBe` Just mockDeleteEvent


mockDeleteEvent :: S3Event
mockDeleteEvent = S3Event [
  Record
  { _eventVersion = "2.0"
  , _eventSource = "aws:s3"
  , _awsRegion = "us-east-1"
  , _eventTime = "1970-01-01T00:00:00.000Z"
  , _eventName = "ObjectRemoved:Delete"
  , _s3 = S3 {_s3SchemaVersion = "1.0"
  , _configurationId = "testConfigRule"
  , _bucket = Bucket {_name = "example-bucket"
  , _arn = "arn:aws:s3:::example-bucket"
  , _ownerIdentity = Just (Map.fromList [("principalId","EXAMPLE")])}
  , _s3Object = S3Object {_key = "test/key"
  , _size = Nothing
  , _eTag = Nothing
  , _versionId = Nothing
  , _sequencer = "0A1B2C3D4E5F678901"}}
  , _userIdentity = Just (Map.fromList [("principalId","EXAMPLE")])
  , _requestParameters = Just (Map.fromList [("sourceIPAddress","127.0.0.1")])
  , _responseElements = Just (Map.fromList [("x-amz-id-2","EXAMPLE123/5678abcdefghijklambdaisawesome/mnopqrstuvwxyzABCDEFGH"),("x-amz-request-id","EXAMPLE123456789")])
  , _glacierEventData = Nothing
  }
  ]
