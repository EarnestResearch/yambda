module Main where

import AWS.Lambda.APIGatewayInputEvent
import AWS.Lambda.APIGatewayOutputEvent hiding (body)
import AWS.Lambda.Handler
import Control.Lens
import Control.Monad.Logger
import Control.Monad.IO.Class

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: (MonadIO m, MonadLogger m) => APIGatewayInputEvent -> m APIGatewayOutputEvent
echo ie = pure $ apiGatewayOutputEvent $ ie ^. body
