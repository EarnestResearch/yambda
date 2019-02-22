{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.Lambda.KinesisDataStreamsEvent
import AWS.Lambda.RuntimeClient
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Data.Text (Text, pack)

main :: IO ()
main = runStderrLoggingT $ do
  client <- runtimeClient @KinesisDataStreamsEvent @KinesisDataStreamsEvent
  forever $ echo client

echo :: (MonadLogger m, MonadIO m) => RuntimeClient KinesisDataStreamsEvent KinesisDataStreamsEvent m -> m ()
echo RuntimeClient{..} = do
  Event{..} <- getNextEvent
  case eventBody of
    Right k -> postResponse eventID $ k
    Left e -> postError eventID $ Error "Unexpected Error" $ pack e

