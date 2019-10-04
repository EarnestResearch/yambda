{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AWS.Lambda.Handler where

import           AWS.Lambda.Encoding
import           AWS.Lambda.RuntimeClient
import           AWS.Lambda.Types
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Text
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

handler :: (LambdaEncode err, MonadError err m, MonadIO m, MonadLogger m, LambdaDecode e, LambdaEncode r) => (e -> m r) -> m ()
handler f = forever $ do
  client <- runtimeClient
  forever $ handle client f

handle :: (LambdaEncode err, MonadError err m, MonadIO m, MonadLogger m, LambdaDecode e, LambdaEncode r) => RuntimeClient e r m -> (e -> m r) -> m ()
handle c@RuntimeClient{..} f = do
  event@Event{..} <- getNextEvent
  let 
    result = case eventBody of
      Right val -> f val >>= postResponse eventID
      Left e    -> postError eventID (Error "Parse failure" $ pack e)
  catchError result (postError eventID . Error "Error" . TE.decodeUtf8 . encodeOutput)
