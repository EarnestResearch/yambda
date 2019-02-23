{-# LANGUAGE TemplateHaskell #-}

module AWS.Lambda.InvocationEvent where

import AWS.Lambda.APIGatewayInputEvent
import AWS.Lambda.KinesisDataStreamsEvent
import Data.Aeson.TH
import Control.Applicative

data InvocationEvent =
    APIGateway APIGatewayInputEvent
  | Kinesis KinesisDataStreamsEvent

$(deriveJSON defaultOptions{sumEncoding = UntaggedValue} ''InvocationEvent)
