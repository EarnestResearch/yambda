module Main where

import AWS.Lambda.Handler
import AWS.Lambda.KinesisDataStreamsEvent
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: KinesisDataStreamsEvent -> KinesisDataStreamsEvent
echo = id
