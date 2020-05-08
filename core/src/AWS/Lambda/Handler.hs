{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module AWS.Lambda.Handler (handler) where

import           AWS.Lambda.Encoding
import           AWS.Lambda.RuntimeClient
import           Control.Monad
import           Control.Monad.Logger
import qualified Data.Aeson as J
import qualified Data.Text as T
import           UnliftIO hiding (handle)


-- | Loop forever handling events with a default runtime client and a given callback
handler
  :: MonadUnliftIO m
  => MonadLogger m
  => Show e
  => LambdaDecode e
  => LambdaEncode r
  => (e -> m r)
  -> m ()
handler f = forever $ do
  client <- runtimeClient
  forever $ handleOne client f


-- | Handle single event with a given runtime client and a callback
handleOne
  :: MonadUnliftIO m
  => MonadLogger m
  => Show e
  => RuntimeClient e r m
  -> (e -> m r)
  -> m ()
handleOne RuntimeClient{..} f =
  tryHandleEvent `catchAny` \e ->
    $logErrorSH e -- on this level there's no event ID to report but try to at least log

  where
    tshow = T.pack . show

    tryHandleEvent = do
      evt@Event{..} <- getNextEvent

      handleAny (\e -> do
                    $logErrorSH (evt, e) -- add full event and an error to lambda logs for debugging
                    (postError eventID . Error "Call failed" . J.String . tshow) e
                ) $
        case eventBody of
          Right val -> f val >>= postResponse eventID
          Left e    -> postError eventID (Error "Failed to parse event" $ (J.String . T.pack) e)
