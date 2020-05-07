module Main where

import AWS.Lambda.Event.Kinesis
import AWS.Lambda.Handler
import Control.Monad.Logger


main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: Applicative m => KinesisDataStreamsEvent -> m KinesisDataStreamsEvent
echo = pure
