{-# LANGUAGE RecordWildCards #-}


-- | API GW / WAI conversions
module AWS.Lambda.Wai.Encoding
  ( gwReqToWai
  , waiResToGw
  ) where


import           AWS.Lambda.Event.APIGatewayV2
import qualified Data.Aeson as JSON
import           Network.Wai


gwReqToWai :: HTTPRequest JSON.Value -> Request
gwReqToWai = undefined


waiResToGw :: Response -> HTTPResponse JSON.Value
waiResToGw = undefined
