{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AWS.Lambda.DhallHandler where

import           AWS.Lambda.RuntimeClient
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Dhall
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Control.Exception hiding (handle)
import Data.Bifunctor

dhallHandler :: (MonadIO m, MonadLogger m, Interpret e, Show e, Show r) => (e -> r) -> m ()
dhallHandler f = forever $ do
  client <- runtimeClient decode
  forever $ handle client f

handle :: (MonadIO m, MonadLogger m, Interpret e, Show e, Show r) => RuntimeClient e r m -> (e -> r) -> m ()
handle RuntimeClient{..} f = do
  event@Event{..} <- getNextEvent
  case eventBody of
    Right val -> postResponse eventID (f val)
    Left e    -> postError eventID (Error "Parse failure" $ T.pack e)

decode :: (MonadIO m, MonadLogger m, Interpret e) => LB.ByteString -> m (Either String e)
decode =
    liftIO . fmap showLeft . try . input auto . LT.toStrict . LTE.decodeUtf8
  where
    showLeft :: Either IOException b -> Either String b
    showLeft = bimap show id  
