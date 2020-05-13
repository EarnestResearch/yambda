{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

{- | Simple JSON webserver.
     Example taken from  https://github.com/haskell-servant/servant/blob/master/servant-server/example/greet.hs
-}
module Main where

import AWS.Lambda.Wai.Handler
import Control.Monad.Logger
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.Wai
import Servant

-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  pures a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             pures a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Server TestApi
server = helloH :<|> postGreetH :<|> deleteGreetH

  where helloH name Nothing      = helloH name (Just False)
        helloH name (Just False) = pure . Greet $ "Hello, " <> name
        helloH name (Just True)  = pure . Greet . toUpper $ "Hello, " <> name

        postGreetH = pure

        deleteGreetH _ = pure NoContent

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

-- Run as Lambda
main :: IO ()
main = runStderrLoggingT $ handleWaiApplication test
