{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}

module AWS.Lambda.HttpClient where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy.Internal as LIB
import qualified Network.HTTP.Client as C
import           Network.HTTP.Types (HeaderName)
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as S

class HttpResponse a b | a -> b where
  responseHeader :: HeaderName -> Traversal' a SB.ByteString
  responseBody :: Lens' a b

instance HttpResponse (W.Response body) body where
  responseHeader = W.responseHeader
  responseBody   = W.responseBody

data HttpClient a =
  HttpClient {
    get  :: (HttpResponse a LIB.ByteString) => String -> IO a
  , post :: (HttpResponse a LIB.ByteString) => String -> Encoding -> IO a
  }

defaultHttpClient :: IO (HttpClient (W.Response LIB.ByteString))
defaultHttpClient = do
  session <- S.newSessionControl Nothing settings
  pure $ HttpClient (S.get session) (S.post session)
  where
    settings =
      C.defaultManagerSettings {
        -- Lambda enforces a timeout so we can wait indefinitely for the next event
        C.managerResponseTimeout = C.responseTimeoutNone
      }

