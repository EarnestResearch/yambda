{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module AWS.Lambda.Encoding where
import qualified Data.Aeson as A
import qualified Dhall as D
import qualified Dhall.Core as DC
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Exception
import Data.Bifunctor
import Data.Coerce

class LambdaDecode e where
    decodeInput :: LB.ByteString -> IO (Either String e)

class LambdaEncode r where
    encodeOutput :: r -> BS.ByteString

newtype LambdaFromJSON a = LambdaFromJSON a deriving A.FromJSON
newtype LambdaToJSON a = LambdaToJSON a deriving A.ToJSON
newtype LambdaFromDhall a = LambdaFromDhall a deriving D.Interpret
newtype LambdaToDhall a = LambdaToDhall a deriving D.Inject

instance A.FromJSON a => LambdaDecode (LambdaFromJSON a) where
    decodeInput = pure . A.eitherDecode

instance A.ToJSON a => LambdaEncode (LambdaToJSON a) where
    encodeOutput = LB.toStrict . A.encode

instance D.Interpret a => LambdaDecode (LambdaFromDhall a) where
    decodeInput = fmap showLeft . try . D.input D.auto . LT.toStrict . LTE.decodeUtf8 . decodeText
        where
            showLeft :: Either IOException b -> Either String b
            showLeft = first show  
            decodeText :: LB.ByteString -> LB.ByteString
            decodeText = either (LTE.encodeUtf8 . LT.pack) LTE.encodeUtf8 . A.eitherDecode

instance D.Inject a => LambdaEncode (LambdaToDhall a) where
    encodeOutput = TE.encodeUtf8 . DC.pretty . D.embed D.inject

instance LambdaDecode T.Text where
    decodeInput = pure . Right . LT.toStrict . LTE.decodeUtf8

instance LambdaEncode T.Text where
    encodeOutput = TE.encodeUtf8

instance LambdaDecode LB.ByteString where
    decodeInput = pure . Right

instance LambdaEncode LB.ByteString where
    encodeOutput = LB.toStrict

instance LambdaEncode IOException where
    encodeOutput = BS8.pack . show

instance LambdaDecode () where
    decodeInput _ = pure $ Right ()

instance LambdaEncode () where
    encodeOutput _ = ""
