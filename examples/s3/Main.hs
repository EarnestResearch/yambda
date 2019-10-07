module Main where

import AWS.Lambda.Handler
import AWS.Lambda.S3Event
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: Applicative m => S3Event -> m S3Event
echo = pure

