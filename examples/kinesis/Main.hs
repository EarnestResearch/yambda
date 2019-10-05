module Main where

import AWS.Lambda.Handler
import AWS.Lambda.KinesisDataStreamsEvent
import AWS.Lambda.Encoding
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Except

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: Applicative m => KinesisDataStreamsEvent -> m KinesisDataStreamsEvent
echo = pure
