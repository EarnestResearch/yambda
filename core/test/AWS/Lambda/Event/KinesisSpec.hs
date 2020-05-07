{-# LANGUAGE OverloadedStrings #-}
module AWS.Lambda.Event.KinesisSpec where

import AWS.Lambda.Event.Kinesis
import Control.Lens
import Data.Aeson
import Data.HashMap.Strict as H
import Test.Hspec

spec :: Spec
spec = describe "JSON encoding decoding" $ do
  it "encode/decode Kinesis converting 'payload' <-> 'data'" testCodecKinesis
  it "encode/decode KinesisDataStreamsEvent converting 'records' <-> 'Records'" testCodecKinesisDataStreamsEvent


testCodecKinesisDataStreamsEvent :: Expectation
testCodecKinesisDataStreamsEvent = do
  let k = Kinesis "partiionKey-V" "payload-V" "kinesisSchemaVersion-V" "sequenceNumber-V"
      r = Record "eId" "eVersion" k "invIdArn" "eName" "eSrcArn" "eSrc" "region"
      s = KinesisDataStreamsEvent [r]
      b = encode s
      j = decode b :: Maybe Object
      s' = decode b :: Maybe KinesisDataStreamsEvent
  case j of
    Just hmap -> do
      let ks = H.keys hmap
      elem "Records" ks `shouldBe` True
    Nothing -> j `shouldNotBe` Nothing
  case s' of
    Just s'' -> s'' `shouldBe` s
    Nothing  -> s' `shouldNotBe` Nothing


testCodecKinesis :: Expectation
testCodecKinesis = do
  let k = Kinesis "partiionKey-V" "payload-V" "kinesisSchemaVersion-V" "sequenceNumber-V"
      b = encode k
      j = decode b :: Maybe Object   -- json object
      k' = decode b :: Maybe Kinesis --
  case j of
    Just hmap -> do
      let ks = H.keys hmap
      length ks `shouldBe` 4
      elem "data" ks `shouldBe` True
      elem "kinesisSchemaVersion" ks `shouldBe` True
      elem "partitionKey" ks `shouldBe` True
      elem "sequenceNumber" ks `shouldBe` True
    Nothing -> j `shouldNotBe` Nothing
  case k' of
    Just kin -> do
      kin ^. payload `shouldBe` "payload-V"
      kin `shouldBe` k
    Nothing -> k' `shouldNotBe` Nothing
