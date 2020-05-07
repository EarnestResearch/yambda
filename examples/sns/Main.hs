module Main where

import AWS.Lambda.Event.SNS
import AWS.Lambda.Handler
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: Applicative m => SNSEvent -> m SNSEvent
echo = pure
