{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}


-- | Helper code for event JSON encoding
module AWS.Lambda.Event.JSON where

import           Data.Aeson
import qualified Data.Char as C
import           GHC.Generics


-- | Used to derive event JSON instances via its implementation
newtype EventJSON e = EventJSON e


-- | Implement options for a type to get JSON derivations via EventJSON
class HasEventJSONOptions a where
  getEventJsonOptions :: Options
  getEventJsonOptions = defaultOptions { fieldLabelModifier = drop 1 } -- common default for our events


instance
  ( Generic e
  , GToJSON Zero (Rep e)
  , GToEncoding Zero (Rep e)
  , HasEventJSONOptions e
  ) => ToJSON (EventJSON e)
  where
    toJSON (EventJSON e) = genericToJSON (getEventJsonOptions @e) e
    toEncoding (EventJSON e) = genericToEncoding (getEventJsonOptions @e) e


instance
  ( Generic e
  , GFromJSON Zero (Rep e)
  , HasEventJSONOptions e
  ) => FromJSON (EventJSON e)
  where
    parseJSON v = EventJSON <$> genericParseJSON (getEventJsonOptions @e) v


capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = C.toUpper x : xs
