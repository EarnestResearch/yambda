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

jsonHandler :: (MonadIO m, MonadLogger m, A.FromJSON e, Show r) => (e -> r) -> m ()
jsonHandler f = do
  RuntimeClient{..} <- runtimeClient A.json
  event@Event{..} <- getNextEvent
  case eventBody of
    Done _ e -> case A.fromJSON e of
      A.Success e -> postResponse eventID (f e)
      A.Error e -> postError eventID (Error "Parse failure" $ pack e)
    Partial _ -> postError eventID (Error "Parse failure" $ "Partial")
    Fail _ _ e ->  postError eventID (Error "Parse failure" $ pack e)
