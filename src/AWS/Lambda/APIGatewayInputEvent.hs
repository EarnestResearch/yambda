{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module AWS.Lambda.APIGatewayInputEvent
  (
    APIGatewayInputEvent(..)
  ) where

import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import GHC.Generics

data APIGatewayInputEvent a =
  APIGatewayInputEvent {
    resource                        :: Text
  , path                            :: Text
  , httpMethod                      :: Text
  , headers                         :: Maybe (HashMap Text Text)
  , multiValueHeaders               :: Maybe (HashMap Text [Text])
  , queryStringParameters           :: Maybe (HashMap Text Text)
  , multiValueQueryStringParameters :: Maybe (HashMap Text [Text])
  , pathParameters                  :: Maybe (HashMap Text Text)
  , stageVariables                  :: Maybe (HashMap Text Text)
  , requestContext                  :: HashMap Text Value
  , body                            :: a
  , isBase64Encoded                 :: Bool
  } deriving (
    Generic
  , Show
  )

instance FromJSON a => FromJSON (APIGatewayInputEvent a)
