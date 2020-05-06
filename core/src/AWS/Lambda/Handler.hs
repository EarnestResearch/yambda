{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AWS.Lambda.Handler
  ( handler
  , handle
  ) where

import           AWS.Lambda.Encoding
import           AWS.Lambda.RuntimeClient
import           Control.Monad
import           Control.Monad.Logger
import qualified Data.Aeson as A
import qualified Data.Text as T
import           UnliftIO hiding (handle)


-- | Loop forever handling events with a default runtime client and a given callback
handler
  :: MonadUnliftIO m
  => MonadLogger m
  => LambdaDecode e
  => LambdaEncode r
  => (e -> m r)
  -> m ()
handler f = forever $ do
  client <- runtimeClient
  forever $ handle client f


-- | Handle single event with a given runtime client and a callback
handle
  :: MonadUnliftIO m
  => MonadLogger m
  => RuntimeClient e r m
  -> (e -> m r)
  -> m ()
handle RuntimeClient{..} f =
  tryHandleEvent `catchAny` \e ->
    logErrorN (tshow e) -- on this level there's no event ID to report but try to at least log

  where
    tshow = T.pack . show

    tryHandleEvent = do
      Event{..} <- getNextEvent

      handleAny (postError eventID . Error "Call failed" . A.String . tshow) $
        case eventBody of
          Right val -> f val >>= postResponse eventID
          Left e    -> postError eventID (Error "Failed to parse event" $ (A.String . T.pack) e)
