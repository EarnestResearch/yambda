{-# LANGUAGE RecordWildCards   #-}
module Main where

import AWS.Lambda.JsonHandler
import AWS.Lambda.APIGatewayInputEvent
import AWS.Lambda.APIGatewayOutputEvent hiding (body)
import Control.Monad.Logger
import Control.Lens

main :: IO ()
main = runStderrLoggingT $ jsonHandler echo

echo :: APIGatewayInputEvent -> APIGatewayOutputEvent
echo ie = apiGatewayOutputEvent $ ie ^. body

