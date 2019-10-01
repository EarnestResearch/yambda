module Main where

import AWS.Lambda.APIGatewayInputEvent
import AWS.Lambda.APIGatewayOutputEvent hiding (body)
import AWS.Lambda.JsonHandler
import Control.Lens
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ jsonHandler echo

echo :: APIGatewayInputEvent -> APIGatewayOutputEvent
echo ie = apiGatewayOutputEvent $ ie ^. body

