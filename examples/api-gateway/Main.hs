{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import AWS.Lambda.APIGatewayInputEvent
import AWS.Lambda.APIGatewayOutputEvent hiding (body)
import AWS.Lambda.RuntimeClient
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Text (pack)

main :: IO ()
main = runStderrLoggingT $ do
  client <- runtimeClient
  forever $ echo client

echo :: (MonadLogger m, MonadIO m) => RuntimeClient APIGatewayInputEvent APIGatewayOutputEvent m -> m ()
echo RuntimeClient{..} = do
  Event{..} <- getNextEvent
  case eventBody of
    Right ie -> postResponse eventID $ apiGatewayOutputEvent $ ie ^. body
    Left e   -> postError eventID $ Error "Unexpected Error" $ pack e
