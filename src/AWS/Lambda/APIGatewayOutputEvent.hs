{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AWS.Lambda.APIGatewayOutputEvent
  (
    APIGatewayOutputEvent(..)
  , apiGatewayOutputEvent
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.HashMap.Strict
import GHC.Generics

data APIGatewayOutputEvent =
  APIGatewayOutputEvent {
    isBase64Encoded :: Bool
  , statusCode :: Int
  , headers :: Maybe (HashMap Text Text)
  , multiValueHeaders :: Maybe (HashMap Text [Text])
  , body :: Text
  } deriving (
    Generic
  , Show
  )

instance ToJSON APIGatewayOutputEvent

apiGatewayOutputEvent :: Text -> APIGatewayOutputEvent
apiGatewayOutputEvent = APIGatewayOutputEvent False 200 Nothing Nothing

