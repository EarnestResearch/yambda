module Main where

import AWS.Lambda.Handler
import AWS.Lambda.S3Event
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: S3Event -> S3Event
echo = id

