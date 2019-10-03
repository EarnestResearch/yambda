{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AWS.Lambda.Encoding where
import qualified Data.Aeson as A
import Dhall
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.ByteString as BS
import Control.Exception
import Data.Bifunctor
import Data.Coerce

class LambdaDecode e where
    decodeInput :: LB.ByteString -> IO (Either String e)

class LambdaEncode r where
    encodeOutput :: r -> BS.ByteString

newtype LambdaFromJSON a = LambdaFromJSON a deriving A.FromJSON
newtype LambdaToJSON a = LambdaToJSON a deriving A.ToJSON
newtype LambdaFromDhall a = LambdaFromDhall a deriving Interpret
newtype LambdaToDhall a = LambdaToDhall a --TODO: create LambdaEncode instance

instance A.FromJSON a => LambdaDecode (LambdaFromJSON a) where
    decodeInput = pure . A.eitherDecode

instance A.ToJSON a => LambdaEncode (LambdaToJSON a) where
    encodeOutput = LB.toStrict . A.encode

instance Interpret a => LambdaDecode (LambdaFromDhall a) where
    decodeInput = fmap showLeft . try . input auto . LT.toStrict . LTE.decodeUtf8
        where
            showLeft :: Either IOException b -> Either String b
            showLeft = bimap show id  

instance LambdaDecode Text where
    decodeInput = pure . Right . LT.toStrict . LTE.decodeUtf8

instance LambdaEncode Text where
    encodeOutput = TE.encodeUtf8

instance LambdaDecode LB.ByteString where
    decodeInput = pure . Right

instance LambdaEncode LB.ByteString where
    encodeOutput = LB.toStrict