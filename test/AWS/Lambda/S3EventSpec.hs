{-# language OverloadedStrings #-}
module AWS.Lambda.S3EventSpec where

import AWS.Lambda.S3Event
import Data.HashMap.Strict as H
import Data.Aeson
import Test.Hspec

spec :: Spec
spec = describe "JSON encoding decoding" $
  it "encode/decode S3Event converting 'records' <-> 'Records'" testCodecS3Event

testCodecS3Event :: Expectation
testCodecS3Event = do
  let bckt = Bucket "bucket-v" "arn-v" Nothing
      s3obj = S3Object "key-v" (Just 123) Nothing Nothing "seq"
      s3' = S3 "version-v" "confId-v" bckt s3obj
      r = Record "eId" "eSrc" "region" "eTime" "eName" s3' Nothing Nothing Nothing Nothing
      s = S3Event [r]
      b = encode s
      j = decode b :: Maybe Object
      s' = decode b :: Maybe S3Event
  case j of
    Just hmap -> do
      let ks = H.keys hmap
      elem "Records" ks `shouldBe` True
    Nothing -> j `shouldNotBe` Nothing
  case s' of
    Just s'' -> s'' `shouldBe` s
    Nothing -> s' `shouldNotBe` Nothing
