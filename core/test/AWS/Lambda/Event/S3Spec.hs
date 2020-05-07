{-# LANGUAGE OverloadedStrings #-}

module AWS.Lambda.Event.S3Spec where

import AWS.Lambda.Event.S3
import Data.Aeson
import Data.HashMap.Strict as H
import Test.Hspec

spec :: Spec
spec = describe "JSON encoding decoding" $ do
  it "encode/decode S3Event converting 'records' <-> 'Records'" testCodecS3Event
  it "encode/decode S3 converting 's3Oject' <-> 'object'" testCodecS3


mockBucket :: Bucket
mockBucket = Bucket "bucket-v" "arn-v" Nothing


mockS3Obj :: S3Object
mockS3Obj = S3Object "key-v" (Just 123) Nothing Nothing "seq"


mockS3 :: S3
mockS3 = S3 "version-v" "confId-v" mockBucket mockS3Obj


testCodecS3Event :: Expectation
testCodecS3Event = do
  let r = Record "eId" "eSrc" "region" "eTime" "eName" mockS3 Nothing Nothing Nothing Nothing
      s = S3Event [r]
      b = encode s
      j = decode b :: Maybe Object
      s' = decode b :: Maybe S3Event
  case j of
    Just hmap -> do
      let ks = H.keys hmap
      elem "Records" ks `shouldBe` True
    Nothing -> j `shouldNotBe` Nothing

  s' `shouldBe` Just s


testCodecS3 :: Expectation
testCodecS3 = do
  -- test s3 _mockS3Object <-> object
  let b' = encode mockS3
      j' = decode b' :: Maybe Object
      s3' = decode b' :: Maybe S3
  case j' of
    Just hmap -> do
      let ks = H.keys hmap
      elem "object" ks `shouldBe` True
    Nothing -> j' `shouldNotBe` Nothing

  s3' `shouldBe` Just mockS3
