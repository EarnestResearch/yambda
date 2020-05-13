{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- requestBody is deprecated but it's a record field
{-# OPTIONS_GHC  -Wno-warnings-deprecations #-}

-- | API GW / WAI conversions
module AWS.Lambda.Wai.Encoding
  ( gwReqToWai
  , waiResToGw
  ) where


import           AWS.Lambda.Event.APIGatewayV2 hiding (rawQueryString)
import           Control.Arrow ((***))
import           Control.Exception
import           Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.CaseInsensitive as CI
import           Data.IORef
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as H
import qualified Network.Socket as Sock
import qualified Network.Wai as W
import qualified Network.Wai.Internal as W


-- | Thrown when response body is not a valid JSON
newtype JSONBodyException = JSONBodyException String deriving (Eq, Show)
instance Exception JSONBodyException

-- | Thrown when we can't determine remote host
newtype RemoteHostException = RemoteHostException String deriving (Eq, Show)
instance Exception RemoteHostException


-- | Converts API GW request to WAI HTTP request
gwReqToWai
  :: HTTPRequest
  -> IO W.Request
gwReqToWai r@HTTPRequest{..} = do
  remoteHost <- getRemoteHost
  isFirstRef <- newIORef False

  let  requestBody = do
         isFirst <- atomicModifyIORef' isFirstRef (False,)
         pure $ if isFirst then requestBodyData else B.empty

       requestBodyLength = W.KnownLength (fromIntegral (B.length requestBodyData))

  pure $ W.Request{..}

  where
    getRemoteHost = do
      let srcIp = _requestContext ^. http . sourceIp . to T.unpack
      addrs <- Sock.getAddrInfo Nothing (Just srcIp) Nothing
      maybe (throwIO . RemoteHostException $ "Failed to get addrinfo for " <> srcIp)
            (pure . Sock.addrAddress)
            (listToMaybe addrs)

    requestBodyData :: B.ByteString
    requestBodyData = either (const B.empty) id $ requestBodyBytes r

    requestMethod = TE.encodeUtf8 $ _requestContext ^. http . method
    httpVersion = H.http10
    rawPathInfo = TE.encodeUtf8 _rawPath
    rawQueryString = TE.encodeUtf8 _rawQueryString
    requestHeaders = ((CI.mk . TE.encodeUtf8) *** TE.encodeUtf8) <$> Map.toList _headers
    isSecure = _requestContext ^. http . protocol . to (== "https")
    pathInfo = H.decodePathSegments rawPathInfo
    queryString = H.parseQuery rawQueryString
    vault = mempty
    requestHeaderHost = _requestContext ^. domainName . to (Just . TE.encodeUtf8)
    requestHeaderRange = Nothing
    requestHeaderReferer = List.lookup H.hReferer requestHeaders
    requestHeaderUserAgent = _requestContext ^. http . userAgent . to (Just . TE.encodeUtf8)




{- | Converts WAI HTTP response to API GW response
     Note that API GW only truly supports text data,
     i.e. except for response body (which can be base64 encoded)
     all headers, etc must be valid UTF8.

     Also note that text and JSON content types are assumed to be
     valid UTF8-encoded binary strings.
-}
waiResToGw
  :: W.Response
  -> IO HTTPResponse
waiResToGw waiRes = do
  (resBody, resBase64) <- resBodyEncoding

  pure $ HTTPResponse
    { _statusCode = H.statusCode $ W.responseStatus waiRes
    , _headers = resHeaders -- TODO: confirm that this works for multi-value headers and cookies
    , _multiValueHeaders = Nothing -- TODO: confirm that 'headers' is enough
    , _body = resBody
    , _isBase64Encoded = resBase64
    , _cookies = Nothing  -- TODO: confirm that 'headers' is enough
    }

  where
    -- not using a map because here the comparison must be case-insensitive but
    -- the list is usually tiny
    resContentType :: Maybe B.ByteString
    resContentType
      = List.lookup H.hContentType
      $ W.responseHeaders waiRes


    resHeaders :: Maybe (Map Text Text)
    resHeaders = case W.responseHeaders waiRes
      of [] -> Nothing
         hs -> Just . Map.fromList $
               (    TE.decodeUtf8 . CI.original
               *** TE.decodeUtf8
               ) <$> hs


    canHaveBody :: Bool -- took from warp's implementation
    canHaveBody
      =  sc /= 204
      && sc /= 205
      && sc /= 304
      && sc >= 200
      where
        sc = H.statusCode $ W.responseStatus waiRes


    resBodyEncoding :: IO (Maybe Text, Maybe Bool)
    resBodyEncoding
      | not canHaveBody                                   = noBody
      | maybe False (B.isPrefixOf "text/") ct             = textBody
      | ct == Just "application/json"                     = textBody
      | maybe False (B.isPrefixOf "application/json;") ct = textBody
      | maybe False (B.isSuffixOf "+json") ct             = textBody
      | isJust ct                                         = base64Body
      | otherwise                                         = unspecifiedBody
      where ct = resContentType


    noBody :: IO (Maybe Text, Maybe Bool)
    noBody = pure (Nothing, Nothing)


    textBody :: IO (Maybe Text, Maybe Bool)
    textBody = do
      bb <- resBodyBytes
      pure ( (Just . TE.decodeUtf8 . LB.toStrict) bb
           , Just False
           )


    base64Body :: IO (Maybe Text, Maybe Bool)
    base64Body = do
      bb <- base64BodyBytes
      pure ( Just bb
           , Just True
           )


    unspecifiedBody :: IO (Maybe Text, Maybe Bool)
    unspecifiedBody = do
      bb <- base64BodyBytes
      if T.null bb
      then noBody
      else pure ( Just bb
                , Just True
                )


    base64BodyBytes :: IO Text
    base64BodyBytes
        = B64.encodeBase64
        . LB.toStrict
      <$> resBodyBytes


    resBodyBytes :: IO LB.ByteString
    resBodyBytes = case waiRes
      of (W.ResponseBuilder _ _ b) -> pure $ BB.toLazyByteString b
         _ -> let (_, _, withChunk) = W.responseToStream waiRes
               in drainChunks withChunk


    drainChunks
      :: ((W.StreamingBody -> IO ()) -> IO ())
      -> IO LB.ByteString
    drainChunks withChunk = do
      bbRef <- newIORef mempty

      withChunk  $ \withBuilder ->
        flip withBuilder (pure ()) $ \b ->
          modifyIORef' bbRef (<> b)

      BB.toLazyByteString <$> readIORef bbRef
