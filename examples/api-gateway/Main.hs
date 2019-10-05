module Main where

import AWS.Lambda.APIGatewayInputEvent
import AWS.Lambda.APIGatewayOutputEvent hiding (body)
import AWS.Lambda.Encoding
import AWS.Lambda.Handler
import Control.Lens
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Except

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: Applicative m => APIGatewayInputEvent -> m APIGatewayOutputEvent
echo ie = pure $ apiGatewayOutputEvent $ ie ^. body
