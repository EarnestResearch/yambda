module Main where

import AWS.Lambda.Handler
import AWS.Lambda.KinesisDataStreamsEvent
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: Applicative m => KinesisDataStreamsEvent -> m KinesisDataStreamsEvent
echo = pure
