{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module AWS.Lambda.RuntimeClient
  ( runtimeClient
  , runtimeClientWith
  , RuntimeClient(..)
  , Error(..)
  , Event(..)
  , EventID
  ) where

import           AWS.Lambda.HttpClient
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.Aeson as A
import           Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy as LB
import           Data.Maybe
import           Data.Text (Text)
import           GHC.Generics
import           System.Environment

newtype EventID = EventID String deriving (Show)
type ErrorMessage = String

data Event a =
  Event {
    eventID   :: EventID
  , eventBody :: Result a
  } deriving (
    Show
  )

data Error =
  Error {
    errorType    :: Text
  , errorMessage :: Text
  } deriving (
    Show
  )

data RuntimeClient e r m =
  RuntimeClient {
    getNextEvent :: (MonadIO m, MonadLogger m) => m (Event e)
  , postResponse :: (MonadIO m, MonadLogger m) => EventID -> r -> m ()
  , postError :: (MonadIO m, MonadLogger m) => EventID -> Error -> m ()
  , postInitError :: (MonadIO m, MonadLogger m) => Error -> m ()
  }

runtimeClient ::
  (MonadLogger m, MonadIO m, Show r) => Parser e -> 
  m (RuntimeClient e r m)
runtimeClient parser = runtimeClientWith parser =<< liftIO defaultHttpClient

runtimeClientWith ::
  (HttpResponse a ByteString, MonadLogger m, MonadIO m, Show r) =>
  Parser e -> HttpClient a -> m (RuntimeClient e r m)
runtimeClientWith parser httpClient = do
  runtimeHost <- liftIO $ lookupEnv runtimeHostEnv
  unless (isJust runtimeHost) ($(logErrorSH) errorMsg)
  let endpoints' = endpoints $ forceMaybe errorMsg runtimeHost
  pure $
    RuntimeClient {
      getNextEvent  = getNextEvent'  endpoints' httpClient parser
    , postResponse  = postResponse'  endpoints' httpClient
    , postError     = postError'     endpoints' httpClient
    , postInitError = postInitError' endpoints' httpClient
    }
  where
    runtimeHostEnv = "AWS_LAMBDA_RUNTIME_API"
    errorMsg = "Missing required environment variable \'AWS_LAMBDA_RUNTIME_API\'."

data Endpoints =
  Endpoints {
    baseURL :: String
  , nextURL :: String
  }

endpoints :: String -> Endpoints
endpoints host =
  Endpoints {
    baseURL = baseURL'
  , nextURL = baseURL' <> "/invocation/next"
  }
  where baseURL' = "http://" <> host <> "/2018-06-01/runtime"

forceMaybe :: String -> Maybe a -> a
forceMaybe errorMsg = fromMaybe (error errorMsg)

getNextEvent' ::
  (HttpResponse a ByteString, MonadIO m, MonadLogger m) =>
  Endpoints -> HttpClient a -> Parser e -> m (Event e)
getNextEvent' Endpoints{..} HttpClient{..} parser = do
  response <- liftIO $ get nextURL
  setTraceID response
  eventID  <- parseEventID response
  let eventBody = parseEvent parser response
  pure $ Event eventID eventBody

setTraceID ::
  (HttpResponse a ByteString, MonadIO m, MonadLogger m) =>
  a -> m ()
setTraceID response = do
  let
    traceEnv = "_X_AMZN_TRACE_ID"
    traceID  = response ^? responseHeader "Lambda-Runtime-Trace-Id" <&> B.unpack
    errorMsg = "Missing response header \"Lambda-Runtime-Trace-Id\"."
  unless (isJust traceID) ($(logError) errorMsg)
  case traceID of
    Just traceID' -> liftIO $ setEnv traceEnv traceID'
    Nothing       -> liftIO $ unsetEnv traceEnv

parseEventID ::
  (HttpResponse a ByteString, MonadIO m, MonadLogger m) =>
  a -> m EventID
parseEventID response = do
  let
    errorMsg = "Missing required response header \"Lambda-Runtime-Aws-Request-Id\"."
    eventID  = response ^? responseHeader "Lambda-Runtime-Aws-Request-Id" <&> EventID . B.unpack
  unless (isJust eventID) ($(logErrorSH) errorMsg)
  pure $ forceMaybe errorMsg eventID

parseEvent ::
  (HttpResponse a ByteString) =>
  Parser e -> a  -> Result e
parseEvent parser response = parse parser $ LB.toStrict (response ^. responseBody)

postResponse' ::
  (HttpResponse a ByteString, MonadIO m, MonadLogger m, Show r) =>
  Endpoints -> HttpClient a -> EventID -> r -> m ()
postResponse' Endpoints{..} HttpClient{..} (EventID eventID) response =
  void . liftIO $ post responseURL (B.pack . show $ response)
  where responseURL = baseURL <> "/invocation/" <> eventID <> "/response"

postError' ::
  (HttpResponse a ByteString, MonadIO m, MonadLogger m) =>
  Endpoints -> HttpClient a -> EventID -> Error -> m ()
postError' Endpoints{..} HttpClient{..} (EventID eventID) error' =
  void . liftIO $ post errorURL (B.pack . show $ error')
  where errorURL = baseURL <> "/invocation/" <> eventID <> "/error"

postInitError' ::
  (HttpResponse a ByteString, MonadIO m, MonadLogger m) =>
  Endpoints -> HttpClient a -> Error -> m ()
postInitError' Endpoints{..} HttpClient{..} error' =
  void . liftIO $ post errorURL (B.pack . show $ error')
  where errorURL = baseURL <> "/init/error"

