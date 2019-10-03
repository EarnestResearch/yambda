{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE DerivingVia #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE DeriveAnyClass #-}

module Main where

import AWS.Lambda.Handler
import Control.Monad.Logger
import Data.Aeson
import Dhall
import GHC.Generics
import AWS.Lambda.Encoding

data User = User {
  name :: Text, 
  accountId :: Natural
} deriving (Generic, Show, ToJSON)

deriving via (LambdaFromDhall User) instance (LambdaDecode User)
deriving via (LambdaToDhall User) instance (LambdaEncode User)

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: User -> User
echo = id

