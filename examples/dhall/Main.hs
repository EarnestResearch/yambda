{-#LANGUAGE DeriveGeneric #-}
module Main where

import AWS.Lambda.Handler
import Control.Lens
import Control.Monad.Logger
import Dhall
import GHC.Generics

data User = User {
  name :: Text, 
  accountId :: Natural
} deriving (Generic, Show)

instance Interpret User

main :: IO ()
main = runStderrLoggingT $ handler echo

echo :: User -> User
echo = id

