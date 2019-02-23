{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AWS.Lambda.InvocationEvent as I
import qualified AWS.Lambda.OutputEvent as O
import AWS.Lambda.APIGatewayInputEvent
import AWS.Lambda.APIGatewayOutputEvent (apiGatewayOutputEvent)
import AWS.Lambda.RuntimeClient
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Data.Text (Text, pack)
import Data.Aeson hiding (Error)

main :: IO ()
main = runStderrLoggingT $ do
  client <- runtimeClient
  forever $ echo client

echo :: (MonadLogger m, MonadIO m) => RuntimeClient (I.InvocationEvent Text) (O.OutputEvent Text) m -> m ()
echo RuntimeClient{..} = do
  Event{..} <- getNextEvent
  case eventBody of
    Right (I.Kinesis k) -> postResponse eventID (O.Kinesis k)
    Right (I.APIGateway APIGatewayInputEvent{..}) -> postResponse eventID $ (O.APIGateway $ apiGatewayOutputEvent body)
    Left e -> postError eventID $ Error "Unexpected Error" $ pack e