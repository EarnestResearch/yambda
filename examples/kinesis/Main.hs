module Main where

import AWS.Lambda.Handler
import AWS.Lambda.KinesisDataStreamsEvent
import Control.Monad.Logger
import Control.Monad.IO.Class

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: (MonadIO m, MonadLogger m) => KinesisDataStreamsEvent -> m KinesisDataStreamsEvent
echo = pure
