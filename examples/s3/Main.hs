module Main where

import AWS.Lambda.Handler
import AWS.Lambda.S3Event
import AWS.Lambda.Encoding
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Except

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: (LambdaEncode e, MonadIO m, MonadLogger m, MonadError e m) => S3Event -> m S3Event
echo = pure

