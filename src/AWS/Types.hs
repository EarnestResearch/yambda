{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AWS.Types where

import Data.Aeson (FromJSON, ToJSON, withText)
import Data.Text
import GHC.Generics

data ARN = ARN {
   prefix :: Text
  ,partition :: Text
  ,service :: Text
  ,region :: Text
  ,accountID :: Text
  ,resource :: Text
  } deriving (Show, Generic)
    
instance FromJSON ARN where
parseJSON = withText "ARN" $ \t ->
  case textToARN t of
    Just arn -> pure arn
    Nothing -> fail . unpack $ t <>
      " doesn't match expected format \
      \arn:aws:<service>:<region>:<accountId>:<resource>"

instance ToJSON ARN where
toJSON arn = intercalate ":" arn

textToARN :: Text -> Maybe ARN
textToARN t
  | (pre : part : svc : reg : accID : res) <- splitOn ":" t
    = Just $ ARN pre part svc reg accID (intercalate ":" res)
  | otherwise = Nothing