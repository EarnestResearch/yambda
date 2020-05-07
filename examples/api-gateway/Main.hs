module Main where

import AWS.Lambda.Event.APIGateway
import AWS.Lambda.Handler
import Control.Lens
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: Applicative m => APIGatewayInputEvent -> m APIGatewayOutputEvent
echo ie = pure $ apiGatewayOutputEvent $ ie ^. body
