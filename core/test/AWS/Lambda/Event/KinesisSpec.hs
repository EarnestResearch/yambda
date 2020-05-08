{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module AWS.Lambda.Event.KinesisSpec where

import           AWS.Lambda.Event.Kinesis
import           Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Paths_aws_lambda_haskell_runtime_client as Paths
import           Test.Hspec


spec :: Spec
spec = describe "JSON decoding" $
  it "parse kinesis records event" testKinesisGetRecords


testKinesisGetRecords :: Expectation
testKinesisGetRecords = do
  fname <- Paths.getDataFileName "kinesis-get-records.json"
  reqBytes <- LB.readFile fname
  decode @KinesisDataStreamsEvent reqBytes `shouldBe` Just mockGetRecordsEvent


mockGetRecordsEvent :: KinesisDataStreamsEvent
mockGetRecordsEvent = KinesisDataStreamsEvent
  [ Record
    { _eventID = "shardId-000000000000:49545115243490985018280067714973144582180062593244200961"
    , _eventVersion = "1.0"
    , _kinesis = Kinesis {_partitionKey = "partitionKey-03"
    , _payload = "SGVsbG8sIHRoaXMgaXMgYSB0ZXN0IDEyMy4="
    , _kinesisSchemaVersion = "1.0"
    , _sequenceNumber = "49545115243490985018280067714973144582180062593244200961"}
    , _invokeIdentityArn = "arn:aws:iam::EXAMPLE"
    , _eventName = "aws:kinesis:record"
    , _eventSourceARN = "arn:aws:kinesis:EXAMPLE"
    , _eventSource = "aws:kinesis"
    , _awsRegion = "us-east-1"
    }
  ]
