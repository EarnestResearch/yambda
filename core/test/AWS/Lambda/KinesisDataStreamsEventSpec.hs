{-# LANGUAGE OverloadedStrings #-}
module AWS.Lambda.KinesisDataStreamsEventSpec where

import AWS.Lambda.KinesisDataStreamsEvent
import Data.Aeson
import Data.HashMap.Strict as H
import Test.Hspec
-- import Test.HUnit

spec :: Spec
spec = describe "JSON encoding decoding" $
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
