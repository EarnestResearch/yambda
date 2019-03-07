{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import AWS.Lambda.APIGatewayInputEvent
import AWS.Lambda.APIGatewayOutputEvent
import AWS.Lambda.RuntimeClient
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Data.Text (Text, pack)

main :: IO ()
main = runStderrLoggingT $ do
  client <- runtimeClient
  forever $ echo client

type Runtime a =
  RuntimeClient (APIGatewayInputEvent Text) APIGatewayOutputEvent a

echo :: (MonadLogger m, MonadIO m) => Runtime m -> m ()
echo RuntimeClient{..} = do
  Event{..} <- getNextEvent
  case eventBody of
    Right (APIGatewayInputEvent{..}) -> postResponse eventID $ apiGatewayOutputEvent body
    Left e -> postError eventID $ Error "Unexpected Error" $ pack e
