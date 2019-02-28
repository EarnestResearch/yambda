{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AWS.Types where

import Data.Aeson (FromJSON, withText)
import Data.Text
import GHC.Generics

data ARN = ARN {
   prefix :: Text
  ,partition :: Text
  ,service :: Text
  ,region :: Text
  ,accountID :: Text
  ,apiID :: Text
  ,stage :: Text
  ,method :: Text
  ,resourcePath :: Text } deriving (Show, Generic)
    
instance FromJSON ARN where
parseJSON = withText "ARN" $ \t ->
  case textToARN t of
    Just arn -> pure arn
    Nothing -> fail . unpack $ t <>
      " doesn't match expected format \
      \arn:aws:execute-api:<regionId>:<accountId>:<apiId>/<stage>/<method>/<resourcePath>"

textToARN :: Text -> Maybe ARN
textToARN t
  | (pre : part : svc : reg : accID : res : []) <- splitOn ":" t
  , (apiID : stg : mthd : path : []) <- splitOn "/" res
    = Just $ ARN pre part svc reg accID apiID stg mthd path
  | otherwise = Nothing