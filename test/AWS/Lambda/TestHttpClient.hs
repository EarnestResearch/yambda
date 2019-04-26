{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module AWS.Lambda.TestHttpClient where

import           AWS.Lambda.HttpClient
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy.Internal as LIB
import qualified Data.HashMap.Strict as MAP
import           Network.HTTP.Types

data TestHttpResponse a =
  TestHttpResponse {
    _headers :: MAP.HashMap HeaderName SB.ByteString
  , _body    :: a
  } deriving (
    Show
  , Eq
  )

makeLenses ''TestHttpResponse

instance HttpResponse (TestHttpResponse LIB.ByteString) LIB.ByteString where
  responseHeader name = headers . at name . non ""
  responseBody = body

defaultHeaders :: MAP.HashMap HeaderName SB.ByteString
defaultHeaders =
  MAP.fromList
    [ ("Lambda-Runtime-Aws-Request-Id", "12345")
    , ("Lambda-Runtime-Trace-Id", "67890")
    ]

emptyResponse :: TestHttpResponse LIB.ByteString
emptyResponse = TestHttpResponse defaultHeaders ""

defaultTestHttpClient :: (ToJSON e) => e -> HttpClient (TestHttpResponse LIB.ByteString)
defaultTestHttpClient nextEvent = HttpClient get' post'
  where
    get' _ = pure $ emptyResponse & body .~ encode nextEvent
    post' _ _ = pure emptyResponse

