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
import           Network.Wai
import           Network.Wai.Internal (ResponseReceived (..))
import           UnliftIO


-- | Wraps a Lambda execution layer around a WAI app
handleWaiApplication
  :: forall m
   . MonadUnliftIO m
  => MonadLogger m
  => Application
  -> m ()
handleWaiApplication waiApp = do
  client <- runtimeClient
  forever $ tryHandleEvent client `catchAny` \e ->
      $logErrorSH e -- error here means we failed to decode request, can't respond

  where
    tryHandleEvent
      :: RuntimeClient (HTTPRequest JSON.Value) (HTTPResponse JSON.Value) m
      -> m ()
    tryHandleEvent RuntimeClient{..} = do
      evt@Event{..} <- getNextEvent

      let onError e = do
            $logErrorSH (evt, e) -- add full event and an error to lambda logs for debugging
            postResponse eventID serverErrorResponse

      handleAny onError $
        case eventBody of
          Right req -> handleWaiApp req (postResponse eventID)
          Left e    -> onError e -- note that this should never happen as we ask for a generic Value body


    handleWaiApp
      :: HTTPRequest JSON.Value
      -> (HTTPResponse JSON.Value -> m ())
      -> m ()
    handleWaiApp req postLambdaResponse = void $ withRunInIO $ \rio -> do
      waiReq <- gwReqToWai req
      waiApp  -- Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
        waiReq
        (\httpRes -> do lamRes <- waiResToGw httpRes
                        rio $ postLambdaResponse lamRes
                        pure ResponseReceived)


serverErrorResponse :: HTTPResponse JSON.Value
serverErrorResponse = httpResponse 500 & body ?~ JSON.String "Server error"
