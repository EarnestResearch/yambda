{-# LANGUAGE TemplateHaskell #-}

module AWS.Lambda.InvocationEvent where

import AWS.Lambda.APIGatewayInputEvent
import AWS.Lambda.KinesisDataStreamsEvent
import Data.Aeson.TH
import Control.Applicative

data InvocationEvent a =
    APIGateway APIGatewayInputEvent
  | Kinesis (KinesisDataStreamsEvent a)

$(deriveJSON defaultOptions{sumEncoding = UntaggedValue} ''InvocationEvent)
