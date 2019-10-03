module Main where

import AWS.Lambda.JsonHandler
import AWS.Lambda.SNSEvent
import Control.Monad.Logger
import AWS.Lambda.Handler

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: SNSEvent -> SNSEvent
echo = id
