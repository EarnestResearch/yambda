{-# LANGUAGE TemplateHaskell #-}

module AWS.Lambda.OutputEvent where

import AWS.Lambda.APIGatewayOutputEvent
import AWS.Lambda.KinesisDataStreamsEvent
import Data.Aeson.TH

data OutputEvent =
    APIGateway APIGatewayOutputEvent
  | Kinesis KinesisDataStreamsEvent

$(deriveJSON defaultOptions ''OutputEvent)