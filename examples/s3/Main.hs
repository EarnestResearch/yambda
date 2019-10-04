module Main where

import AWS.Lambda.Handler
import AWS.Lambda.S3Event
import Control.Monad.Logger
import Control.Monad.IO.Class

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: (MonadIO m, MonadLogger m) => S3Event -> m S3Event
echo = pure

