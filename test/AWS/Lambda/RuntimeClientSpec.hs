{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications   #-}

module AWS.Lambda.RuntimeClientSpec where

import AWS.Lambda.HttpClient
import AWS.Lambda.RuntimeClient
import AWS.Lambda.TestHttpClient
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson hiding (Error)
import Data.ByteString.Char8 (pack)
import Data.HashMap.Strict as MAP
import Data.Key
import Data.Text (Text)
import System.Environment
import Test.Hspec

spec :: Spec
spec =
  beforeAll_ clearTraceId $
  around_ withRuntimeEnvVars $
  describe "the runtime client" $ do
    it "sets aws xray trace ID environment variable"               testSetsTraceId
    it "returns an event ID obtained from an http response header" testReturnsEventId
    it "returns an event deserialized from the response body"      testReturnsEvent
    it "uses the correct url to retrieve the next event"           testGetUrl
    it "uses the correct url to post a response"                   testPostResponseUrl
    it "uses the correct url to post an error"                     testPostErrorUrl
    it "uses the correct url to post an init error"                testPostInitErrorUrl
    it "sends a response"                                          testPostResponseBody
    it "sends an error"                                            testPostErrorBody
    it "sends an initialization error"                             testPostInitErrorBody
  where
    clearTraceId = unsetEnv "_X_AMZN_TRACE_ID"

withRuntimeEnvVars :: IO () -> IO ()
withRuntimeEnvVars = bracket_ setEnvVars unsetEnvVars
  where
    envVars =
      MAP.fromList
        [ ("AWS_LAMBDA_RUNTIME_API", "example.com")
        ]
    setEnvVars   = mapWithKeyM_ setEnv envVars
    unsetEnvVars = mapM_ unsetEnv (keys envVars)

testSetsTraceId :: Expectation
testSetsTraceId = runNoLoggingT $ do
  RuntimeClient{..} <- runtimeClientWith' eitherDecode httpClient
  Event{..} <- getNextEvent
  traceId <- liftIO $ lookupEnv "_X_AMZN_TRACE_ID"
  liftIO $ traceId `shouldBe` Just "67890"
  where
    runtimeClientWith' = runtimeClientWith @_ @_ @Text @Text
    httpClient = defaultTestHttpClient ("Hello, world" :: Text)

testReturnsEventId :: Expectation
testReturnsEventId = runNoLoggingT $ do
  RuntimeClient{..} <- runtimeClientWith' eitherDecode httpClient
  Event{..} <- getNextEvent
  liftIO $ show eventID `shouldBe` "EventID \"12345\""
  where
    runtimeClientWith' = runtimeClientWith @_ @_ @Text @Text
    httpClient = defaultTestHttpClient ("Hello, world" :: Text)

testReturnsEvent :: Expectation
testReturnsEvent = runNoLoggingT $ do
  RuntimeClient{..} <- runtimeClientWith' eitherDecode httpClient
  Event{..} <- getNextEvent
  liftIO $ eventBody `shouldBe` Right "Hello, world"
  where
    runtimeClientWith' = runtimeClientWith @_ @_ @Text @Text
    httpClient = defaultTestHttpClient ("Hello, world" :: Text)

withGetUrl :: HttpClient a -> String -> HttpClient a
withGetUrl (HttpClient get' post') url =
  HttpClient (\x -> shouldBe x url >> get' x) post'

testGetUrl :: Expectation
testGetUrl = runNoLoggingT $ do
  RuntimeClient{..} <- runtimeClientWith' eitherDecode httpClient
  void getNextEvent
  where
    runtimeClientWith' = runtimeClientWith @_ @_ @Text @Text
    httpClient =
      defaultTestHttpClient Null
        `withGetUrl` "http://example.com/2018-06-01/runtime/invocation/next"

withPostUrl :: HttpClient a -> String -> HttpClient a
withPostUrl (HttpClient get' post') url = HttpClient get' (\x y -> shouldBe x url >> post' x y)

testPostResponseUrl :: Expectation
testPostResponseUrl = runNoLoggingT $ do
  RuntimeClient{..} <- runtimeClientWith' eitherDecode httpClient
  Event{..} <- getNextEvent
  void $ postResponse eventID ""
  where
    runtimeClientWith' = runtimeClientWith @_ @_ @Text @Text
    httpClient =
      defaultTestHttpClient Null
        `withPostUrl` "http://example.com/2018-06-01/runtime/invocation/12345/response"

testPostErrorUrl :: Expectation
testPostErrorUrl = runNoLoggingT $ do
  RuntimeClient{..} <- runtimeClientWith' eitherDecode httpClient
  Event{..} <- getNextEvent
  void $ postError eventID $ Error "Test" "Test"
  where
    runtimeClientWith' = runtimeClientWith @_ @_ @Text @Text
    httpClient =
      defaultTestHttpClient Null
        `withPostUrl` "http://example.com/2018-06-01/runtime/invocation/12345/error"

testPostInitErrorUrl :: Expectation
testPostInitErrorUrl = runNoLoggingT $ do
  RuntimeClient{..} <- runtimeClientWith' eitherDecode httpClient
  Event{..} <- getNextEvent
  void . postInitError $ Error "Test" "Test"
  where
    runtimeClientWith' = runtimeClientWith @_ @_ @Text @Text
    httpClient =
      defaultTestHttpClient Null
        `withPostUrl` "http://example.com/2018-06-01/runtime/init/error"

withPostBody :: (Show b) => HttpClient a -> b -> HttpClient a
withPostBody (HttpClient get' post') b = HttpClient get' (\x y -> shouldBe y body' >> post' x y)
  where body' = pack . show $ b

testPostResponseBody :: Expectation
testPostResponseBody = runNoLoggingT $ do
  RuntimeClient{..} <- runtimeClientWith' eitherDecode httpClient
  Event{..} <- getNextEvent
  void $ postResponse eventID body'
  where
    runtimeClientWith' = runtimeClientWith @_ @_ @Text @Text
    httpClient = defaultTestHttpClient Null `withPostBody` body'
    body' :: Text
    body' = "Hello, World"

testPostErrorBody :: Expectation
testPostErrorBody = runNoLoggingT $ do
  RuntimeClient{..} <- runtimeClientWith' eitherDecode httpClient
  Event{..} <- getNextEvent
  void $ postError eventID error'
  where
    runtimeClientWith' = runtimeClientWith @_ @_ @Text @Text
    httpClient = defaultTestHttpClient Null `withPostBody` error'
    error' = Error "Test" "Hello, World"

testPostInitErrorBody :: Expectation
testPostInitErrorBody = runNoLoggingT $ do
  RuntimeClient{..} <- runtimeClientWith' eitherDecode httpClient
  Event{..} <- getNextEvent
  void $ postInitError error'
  where
    runtimeClientWith' = runtimeClientWith @_ @_ @Text @Text
    httpClient = defaultTestHttpClient Null `withPostBody` error'
    error' = Error "Test" "Hello, World"

