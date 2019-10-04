{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE DerivingVia #-}
{-#LANGUAGE DeriveAnyClass #-}

module Main where

import AWS.Lambda.Handler
import Dhall
import AWS.Lambda.Encoding
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Except

data User = User {
  name :: Text, 
  accountId :: Natural
} deriving (Generic, Show, Interpret, Inject)
  deriving LambdaDecode via (LambdaFromDhall User)
  deriving LambdaEncode via (LambdaToDhall User)

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: (LambdaEncode e, MonadIO m, MonadLogger m, MonadError e m) => User -> m User
echo = pure
