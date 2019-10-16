{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Main where

import AWS.Lambda.Encoding
import AWS.Lambda.Handler
import Control.Monad.Logger
import Dhall

data User = User {
  name      :: Text,
  accountId :: Natural
} deriving (Generic, Show, Interpret, Inject)
  deriving LambdaDecode via (LambdaFromDhall User)
  deriving LambdaEncode via (LambdaToDhall User)

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: Applicative m => User -> m User
echo = pure
