module Main where

import AWS.Lambda.Handler
import AWS.Lambda.SNSEvent
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: Applicative m => SNSEvent -> m SNSEvent
echo = pure
