{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AWS.Lambda.Handler where

import           AWS.Lambda.Encoding
import           AWS.Lambda.RuntimeClient
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Logger
import qualified Data.Aeson as A
import           Data.Text

handler :: (LambdaEncode err, MonadError err m, MonadIO m, MonadLogger m, LambdaDecode e, LambdaEncode r) => (e -> m r) -> m ()
handler f = forever $ do
  client <- runtimeClient
  forever $ handle client f

handle :: (LambdaEncode err, MonadError err m, MonadIO m) => RuntimeClient e r m -> (e -> m r) -> m ()
handle RuntimeClient{..} f = do
  Event{..} <- getNextEvent
  let
    result = case eventBody of
      Right val -> f val >>= postResponse eventID
      Left e    -> postError eventID (Error "Parse failure" $ (A.String . pack) e)
  catchError result (postError eventID . Error "Error" . encodeOutput)
