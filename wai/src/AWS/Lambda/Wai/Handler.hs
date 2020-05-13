{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{- | Adapts API Gateway models to WAI request/response.

     The goal is to support packaging small, infrequently
     used JSON APIs as Lambdas using existing WAI-compatible
     frameworks.

     Note that this conversion is only an approximation and
     many things possible in HTTP are not representable
     (e.g. streaming).

     To debug requests/responses please enable "DEBUG" environmental
     variable in your lambda.
-}
module AWS.Lambda.Wai.Handler
  ( handleWaiApplication
  ) where


import           AWS.Lambda.Event.APIGatewayV2
import           AWS.Lambda.RuntimeClient
import           AWS.Lambda.Wai.Encoding
import           Control.Lens
import           Control.Monad
import           Control.Monad.Logger
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Network.Wai
import           Network.Wai.Internal (ResponseReceived (..))
import           System.Environment
import           UnliftIO


-- | Wraps a Lambda execution layer around a WAI app
handleWaiApplication
  :: forall m
   . MonadUnliftIO m
  => MonadLogger m
  => Application
  -> m ()
handleWaiApplication waiApp = do
  debugMode <- isJust <$> liftIO (lookupEnv "DEBUG")

  client <- runtimeClient
  forever $ tryHandleEvent debugMode client `catchAny` \e ->
      $logErrorSH e -- error here means we failed to decode request, can't respond

  where
    toJsonText :: JSON.ToJSON a => a -> Text
    toJsonText = TE.decodeUtf8 . LB.toStrict . JSON.encode

    tryHandleEvent
      :: Bool
      -> RuntimeClient HTTPRequest HTTPResponse m
      -> m ()
    tryHandleEvent debugMode RuntimeClient{..} = do
      evt@Event{..} <- getNextEvent
      when debugMode $ $logDebug $ toJsonText evt

      let onError e = do
            $logError $ toJsonText (show e, evt) -- add full event and an error to lambda logs for debugging
            postResponse eventID serverErrorResponse

      handleAny onError $
        case eventBody of
          Right req -> handleWaiApp debugMode req (postResponse eventID)
          Left e    -> onError e -- note that this should never happen as we ask for a generic Value body


    handleWaiApp
      :: Bool
      -> HTTPRequest
      -> (HTTPResponse -> m ())
      -> m ()
    handleWaiApp debugMode req postLambdaResponse = void $ withRunInIO $ \rio -> do
      waiReq <- gwReqToWai req
      waiApp  -- Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
        waiReq
        (\httpRes -> do lamRes <- waiResToGw httpRes
                        when debugMode $ rio $ $logDebug $ toJsonText lamRes
                        rio $ postLambdaResponse lamRes
                        pure ResponseReceived)


serverErrorResponse :: HTTPResponse
serverErrorResponse = httpResponse 500 & body ?~ "Server error"
