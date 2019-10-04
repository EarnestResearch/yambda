module Main where

import AWS.Lambda.Handler
import AWS.Lambda.SNSEvent
import AWS.Lambda.Encoding
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Except

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: (LambdaEncode e, MonadIO m, MonadLogger m, MonadError e m) => SNSEvent -> m SNSEvent
echo = pure
