{-# LANGUAGE TemplateHaskell #-}

module AWS.Lambda.OutputEvent where

import AWS.Lambda.APIGatewayOutputEvent
import AWS.Lambda.KinesisDataStreamsEvent
import Data.Aeson.TH

data OutputEvent a =
    APIGateway APIGatewayOutputEvent
  | Kinesis (KinesisDataStreamsEvent a)

$(deriveJSON defaultOptions ''OutputEvent)