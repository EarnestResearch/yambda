module Main where

import AWS.Lambda.Encoding
import AWS.Lambda.Handler
import AWS.Lambda.KinesisDataStreamsEvent
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: Applicative m => KinesisDataStreamsEvent -> m KinesisDataStreamsEvent
echo = pure
