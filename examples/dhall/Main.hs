{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE DerivingVia #-}
{-#LANGUAGE DeriveAnyClass #-}

module Main where

import AWS.Lambda.Handler
import Dhall
import AWS.Lambda.Encoding
import Control.Monad.Logger

data User = User {
  name :: Text, 
  accountId :: Natural
} deriving (Generic, Show, Interpret, Inject)
  deriving LambdaDecode via (LambdaFromDhall User)
  deriving LambdaEncode via (LambdaToDhall User)

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: Applicative m => User -> m User
echo = pure
