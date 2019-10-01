{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}

module AWS.Lambda.JsonHandler where

import AWS.Lambda.RuntimeClient
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Aeson as A
import Data.Attoparsec.ByteString
import Data.Text

jsonHandler :: (MonadIO m, MonadLogger m, A.FromJSON e, Show e, Show r) => (e -> r) -> m ()
jsonHandler f = forever $ do
  client <- runtimeClient A.eitherDecode
  forever $ handle client f

handle :: (MonadIO m, MonadLogger m, A.FromJSON e, Show e, Show r) => RuntimeClient e r m -> (e -> r) -> m ()
handle RuntimeClient{..} f = do
  event@Event{..} <- getNextEvent
  case eventBody of
    Right val -> postResponse eventID (f val)
    Left e -> postError eventID (Error "Parse failure" $ pack e)
