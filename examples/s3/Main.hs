{-# LANGUAGE RecordWildCards   #-}
module Main where

import AWS.Lambda.JsonHandler
import AWS.Lambda.S3Event
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ jsonHandler echo

echo :: S3Event -> S3Event
echo = id

