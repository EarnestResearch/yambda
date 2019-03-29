{-# LANGUAGE OverloadedStrings #-}
module AWS.Lambda.KinesisSpec where

import AWS.Lambda.KinesisDataStreamsEvent
import Control.Lens
import Data.Aeson
import Data.HashMap.Strict as H
import Test.Hspec

spec :: Spec
spec = describe "JSON encoding decoding" $
  it "encode/decode Kinesis converting 'payload' <-> 'data'" testCodecKinesis

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
      kin ^. payload `shouldBe` "payload-V" -- lens
      _payload kin `shouldBe` "payload-V"   -- default/regular accessor
      kin `shouldBe` k
    Nothing -> k' `shouldNotBe` Nothing
