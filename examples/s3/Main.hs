module Main where

import AWS.Lambda.Event.S3
import AWS.Lambda.Handler
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: Applicative m => S3Event -> m S3Event
echo = pure

