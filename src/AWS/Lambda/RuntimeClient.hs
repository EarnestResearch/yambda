{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module AWS.Lambda.RuntimeClient
  (
    runtimeClient
  , RuntimeClient(..)
  , Error(..)
  , Event(..)
  , EventID
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Data.Aeson hiding (Error)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy.Internal
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import qualified Network.HTTP.Client as C
import           Network.Wreq
import           Network.Wreq.Session as S
import           System.Environment

newtype EventID = EventID String deriving (Show)
type ErrorMessage = String

data Event a =
  Event {
    eventID   :: EventID
  , eventBody :: Either ErrorMessage a
  } deriving (
    Show
  )

data Error =
  Error {
    errorType    :: Text
  , errorMessage :: Text
  } deriving (
    Show
  , Generic
  )

instance ToJSON Error

data RuntimeClient e r m =
  RuntimeClient {
    getNextEvent :: (FromJSON e, MonadIO m, MonadLogger m) => m (Event e)
  , postResponse :: (ToJSON r, MonadIO m, MonadLogger m) => EventID -> r -> m ()
  , postError :: (MonadIO m, MonadLogger m) => EventID -> Error -> m ()
  , postInitError :: (MonadIO m, MonadLogger m) => Error -> m ()
  }

runtimeClient :: (FromJSON e, ToJSON r, MonadLogger m, MonadIO m) => m (RuntimeClient e r m)
runtimeClient = do
  let
    runtimeHostEnv = "AWS_LAMBDA_RUNTIME_API"
    errorMsg = "Missing required environment variable \'AWS_LAMBDA_RUNTIME_API\'."
    settings =
      C.defaultManagerSettings {
        C.managerResponseTimeout = C.responseTimeoutNone
      }
  runtimeHost <- liftIO $ lookupEnv runtimeHostEnv
  session     <- liftIO $ S.newSessionControl Nothing settings
  unless (isJust runtimeHost) ($(logErrorSH) errorMsg)
  let endpoints' = endpoints . forceMaybe errorMsg $ runtimeHost
  pure $
    RuntimeClient {
      getNextEvent  = getNextEvent' endpoints' session
    , postResponse  = postResponse' endpoints' session
    , postError     = postError' endpoints' session
    , postInitError = postInitError' endpoints' session
    }

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

getNextEvent' :: (FromJSON e, MonadIO m, MonadLogger m) => Endpoints -> Session -> m (Event e)
getNextEvent' Endpoints{..} session = do
  response <- liftIO $ S.get session nextURL
  setTraceID response
  eventID  <- parseEventID response
  let eventBody = parseEvent response
  pure $ Event eventID eventBody

setTraceID :: (MonadIO m, MonadLogger m) => Response ByteString -> m ()
setTraceID response = do
  let
    traceEnv = "_X_AMZN_TRACE_ID"
    traceID  = fmap B.unpack $ response ^? responseHeader "Lambda-Runtime-Trace-Id"
    errorMsg = "Missing response header \"Lambda-Runtime-Trace-Id\"."
  unless (isJust traceID) ($(logError) errorMsg)
  case traceID of
    Just traceID' -> liftIO $ setEnv traceEnv traceID'
    Nothing       -> liftIO $ unsetEnv traceEnv

parseEventID :: (MonadIO m, MonadLogger m) => Response ByteString -> m EventID
parseEventID response = do
  let
    header   = "Lambda-Runtime-Aws-Request-Id"
    errorMsg = "Missing required response header \"Lambda-Runtime-Aws-Request-Id\"."
    eventID  = fmap (EventID . B.unpack) (response ^? responseHeader header)
  unless (isJust eventID) ($(logErrorSH) errorMsg)
  pure $ forceMaybe errorMsg eventID

parseEvent :: (FromJSON e) => Response ByteString -> Either ErrorMessage e
parseEvent = eitherDecode . (^. responseBody)

postResponse' :: (ToJSON r, MonadIO m, MonadLogger m) => Endpoints -> Session -> EventID -> r -> m ()
postResponse' Endpoints{..} session (EventID eventID) r = do
  let
    responseURL = baseURL <> "/invocation/" <> eventID <> "/response"
    response    = encode r
  liftIO $ S.post session responseURL response
  return ()

postError' :: (MonadIO m, MonadLogger m) => Endpoints -> Session -> EventID -> Error -> m ()
postError' Endpoints{..} session (EventID eventID) error' = do
  let
    errorURL = baseURL <> "/invocation/" <> eventID <> "/error"
    error''  = encode error'
  liftIO $ S.post session errorURL error''
  return ()

postInitError' :: (MonadIO m, MonadLogger m) => Endpoints -> Session -> Error -> m ()
postInitError' Endpoints{..} session error' = do
  let
    errorURL = baseURL <> "/init/error"
    error''  = encode error'
  liftIO $ S.post session errorURL error''
  return ()

