module Main where

import AWS.Lambda.JsonHandler
import AWS.Lambda.KinesisDataStreamsEvent
import Control.Monad.Logger
import AWS.Lambda.Handler

main :: IO ()
main = runStderrLoggingT $ jsonHandler echo

echo :: KinesisDataStreamsEvent -> KinesisDataStreamsEvent
echo = id
