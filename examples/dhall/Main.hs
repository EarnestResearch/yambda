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
deriving via (LambdaToJSON User) instance (LambdaEncode User) --TODO: replace once figured out how to encode a type to a dhall expr

instance Interpret User

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: User -> User
echo = id

