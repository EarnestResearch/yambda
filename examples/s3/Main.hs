{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}
module Main where

import AWS.Lambda.S3Event
import AWS.Lambda.RuntimeClient
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Text (pack)

main :: IO ()
main = runStderrLoggingT $ do
  client <- runtimeClient
  forever $ echo client

echo :: (MonadLogger m, MonadIO m) => RuntimeClient S3Event S3Event m -> m ()
echo RuntimeClient{..} = do
  Event{..} <- getNextEvent
  case eventBody of
    Right e -> postResponse eventID e
    Left e -> postError eventID $ Error "Unexpected Error" $ pack e
