module Main where

import AWS.Lambda.JsonHandler
import AWS.Lambda.SNSEvent
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ jsonHandler echo

echo :: SNSEvent -> SNSEvent
echo = id
