{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module AWS.Lambda.Types where

import AWS.Lambda.Encoding
import Data.Aeson
import Data.Text
import GHC.Generics

newtype EventID = EventID String deriving (Show)
type ErrorMessage = String

data Event a =
  Event {
    eventID   :: EventID
  , eventBody :: Either String a
  } deriving (
    Show
  )

data Error =
  Error {
    errorType    :: Text
  , errorMessage :: Value
  } deriving (
    Show,
    Generic,
    ToJSON
  )
  deriving LambdaEncode via (LambdaToJSON Error)
