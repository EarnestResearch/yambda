{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Control.Exception
import Data.Bifunctor
import Data.Coerce
import GHC.Generics

class LambdaDecode e where
    decodeInput :: LB.ByteString -> IO (Either String e)

class LambdaEncode r where
    encodeOutput :: r -> BS.ByteString

newtype LambdaFromJSON a = LambdaFromJSON a deriving A.FromJSON
newtype LambdaToJSON a = LambdaToJSON a deriving A.ToJSON
newtype LambdaFromDhall a = LambdaFromDhall a deriving Generic
newtype LambdaToDhall a = LambdaToDhall a deriving Generic

instance A.FromJSON a => LambdaDecode (LambdaFromJSON a) where
    decodeInput = pure . A.eitherDecode

instance A.ToJSON a => LambdaEncode (LambdaToJSON a) where
    encodeOutput = LB.toStrict . A.encode

instance D.Interpret a => LambdaDecode (LambdaFromDhall a) where
    decodeInput = fmap showLeft . try . D.input D.genericAuto . LT.toStrict . LTE.decodeUtf8
        where
            showLeft :: Either IOException b -> Either String b
            showLeft = bimap show id  

instance D.Inject a => LambdaEncode (LambdaToDhall a) where
    encodeOutput = TE.encodeUtf8 . DC.pretty . D.embed D.genericInject

instance LambdaDecode T.Text where
    decodeInput = pure . Right . LT.toStrict . LTE.decodeUtf8

instance LambdaEncode T.Text where
    encodeOutput = TE.encodeUtf8

instance LambdaDecode LB.ByteString where
    decodeInput = pure . Right

instance LambdaEncode LB.ByteString where
    encodeOutput = LB.toStrict