module Main where

import AWS.Lambda.APIGatewayInputEvent
import AWS.Lambda.APIGatewayOutputEvent hiding (body)
import AWS.Lambda.Handler
import Control.Lens
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: APIGatewayInputEvent -> APIGatewayOutputEvent
echo ie = apiGatewayOutputEvent $ ie ^. body

