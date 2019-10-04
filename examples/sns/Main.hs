module Main where

import AWS.Lambda.Handler
import AWS.Lambda.SNSEvent
import Control.Monad.Logger
import Control.Monad.IO.Class

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: (MonadIO m, MonadLogger m) => SNSEvent -> m SNSEvent
echo = pure
