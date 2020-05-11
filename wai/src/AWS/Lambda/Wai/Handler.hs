{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module AWS.Lambda.Wai.Handler where

import           AWS.Lambda.Event.APIGatewayV2
import           AWS.Lambda.RuntimeClient
import           AWS.Lambda.Wai.Encoding
import           Control.Monad
import           Control.Monad.Logger
import qualified Data.Aeson as JSON
import           Network.Wai
import           Network.Wai.Internal (ResponseReceived (..))
import           UnliftIO


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
    handleWaiApp req postLambdaResponse = void $ withRunInIO $ \rio ->
      waiApp  -- Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
      (gwReqToWai req)
      (\httpRes -> do rio $ postLambdaResponse (waiResToGw httpRes)
                      pure ResponseReceived)



serverErrorResponse :: HTTPResponse JSON.Value
serverErrorResponse = undefined
