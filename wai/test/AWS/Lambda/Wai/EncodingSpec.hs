{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module AWS.Lambda.Wai.EncodingSpec where

import           AWS.Lambda.Event.APIGatewayV2
import           AWS.Lambda.Wai.Encoding
import           Control.Lens
import           Data.Aeson
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import qualified Paths_yambda_wai as Paths
import           Test.Hspec


spec :: Spec
spec = describe "GW / WAI conversions" $ do
  it "gwReqToWai 01" testGwReqToWai01
  it "gwReqToWai 02" testGwReqToWai02

  it "waiResToGw 01" testWaiResToGw01
  it "waiResToGw 02" testWaiResToGw02
  it "waiResToGw 03" testWaiResToGw03


testGwReqToWai01 :: Expectation
testGwReqToWai01 = do
  r <- loadFile "request01.json"
  W.requestMethod r `shouldBe` H.methodPost
  W.rawPathInfo r `shouldBe` "/echo/foo/bar/meah"
  W.pathInfo r `shouldBe` ["foo", "bar", "meah"]

  W.queryString r `shouldBe`
    [ ("foo", Just "bar,")
    , ("foo", Just "b,a,z")
    , ("foo", Just "b,l,a,h")
    ]

  W.requestHeaderHost r `shouldBe`
    Just "aabbccddee.execute-api.us-east-1.amazonaws.com"

  W.requestHeaderUserAgent r `shouldBe`
    Just "curl/7.70.0"

  List.sort (W.requestHeaders r) `shouldBe` List.sort
    [ ("accept", "*/*")
    , ("content-length", "43")
    , ("content-type", "application/x-www-form-urlencoded")
    , ("host", "aabbccddee.execute-api.us-east-1.amazonaws.com")
    , ("test", "test1,test2,test3")
    , ("user-agent", "curl/7.70.0")
    , ("x-amzn-trace-id", "Root=1-5ebed252-c759dc36b13aabe2e3fbeba8")
    , ("x-forwarded-for", "33.33.33.33")
    , ("x-forwarded-port", "443")
    , ("x-forwarded-proto", "https")
    ]

  b <- W.strictRequestBody r
  b `shouldBe` "param1={\"foo\":1,\"bar\":\"blah\"}&param2=foobar"


testGwReqToWai02 :: Expectation
testGwReqToWai02 = do
  r <- loadFile "request02.json"
  W.requestMethod r `shouldBe` H.methodPut
  W.rawPathInfo r `shouldBe` "/api/foo"
  W.pathInfo r `shouldBe` ["api", "foo"]

  W.queryString r `shouldBe`
    [ ("foo", Just "bar")
    , ("bar", Just "baz meah")
    ]

  W.requestHeaderHost r `shouldBe`
    Just "aabbccddee.execute-api.us-east-1.amazonaws.com"

  W.requestHeaderUserAgent r `shouldBe`
    Just "curl/7.70.0"

  List.sort (W.requestHeaders r) `shouldBe` List.sort
    [ ("accept", "*/*")
    , ("content-length", "35")
    , ("content-type", "application/json")
    , ("host", "aabbccddee.execute-api.us-east-1.amazonaws.com")
    , ("user-agent", "curl/7.70.0")
    , ("x-amzn-trace-id", "Root=1-5ebf0749-57baecc80db7e2181f709f90")
    , ("x-forwarded-for", "33.33.33.33")
    , ("x-forwarded-port", "443")
    , ("x-forwarded-proto", "https")
    ]

  b <- W.strictRequestBody r
  b `shouldBe` "{\"username\":\"xyz\",\"password\":\"xyz\"}"


testWaiResToGw01 :: Expectation
testWaiResToGw01 = do
  r <- waiResToGw $ W.responseLBS H.status204 [] "useless content"
  r `shouldBe` httpResponse 204


testWaiResToGw02 :: Expectation
testWaiResToGw02 = do
  r <- waiResToGw $ W.responseLBS H.status200
         [ (H.hContentType, "text/plain")
         , ("Set-Cookie", "sessionId=e8bb43229de9; Domain=foo.example.com")
         , ("Set-Cookie", "foo=bar; Domain=foo.example.com")
         ]
         "some text"

  r `shouldBe`
    ( httpResponse 200
    & isBase64Encoded ?~ False
    & body ?~ "some text"
    & headers ?~ Map.fromList [("Content-Type", "text/plain")]
    & cookies ?~
        [ "sessionId=e8bb43229de9; Domain=foo.example.com"
        , "foo=bar; Domain=foo.example.com"
        ]
    )


testWaiResToGw03 :: Expectation
testWaiResToGw03 = do
  r <- waiResToGw $ W.responseLBS H.status200
         [ (H.hContentType, "application/octet-stream") ]
         "some binary data"

  r `shouldBe`
    ( httpResponse 200
    & isBase64Encoded ?~ True
    & body ?~ "c29tZSBiaW5hcnkgZGF0YQ=="
    & headers ?~ Map.fromList [("Content-Type", "application/octet-stream")]
    )


loadFile
  :: FilePath
  -> IO W.Request
loadFile fname = do
  fp <- Paths.getDataFileName fname
  Just gwReq <- decodeFileStrict fp
  gwReqToWai gwReq
