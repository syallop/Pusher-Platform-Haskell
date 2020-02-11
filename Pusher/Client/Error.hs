{-# LANGUAGE
    DuplicateRecordFields
  , EmptyDataDecls
  , MultiParamTypeClasses
  , OverloadedStrings
  , TupleSections
  , TypeApplications
  #-}
{-|
Module      : Pusher.Client.Error
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

This module defines 'ErrorResponse's which are returned from requests and subscriptions which successfully fail. It does not capture unexpected errors in client implementation, networking, etc.

These errors can be interpreted as an end-user by consulting the documentation pointed to by the errorURI. An error "type" and "description" give a summary of the cause of the error.

Programatically, these errors can be interpreted according to the status code and headers. Status code classes follow HTTP (2xx success, 3xx transient, 4xx client error, 5xx server error). The presence of a @Retry-After@ header indicates how soon you may repeat the request/ subscription that triggered the error.

When implementing clients, sensible retry policies should be followed before logging/ surfacing the error to the end user.
-}
module Pusher.Client.Error
  ( ErrorResponse (..)
  , ErrorResponseBody (..)
  )
  where

import Prelude hiding (read)

import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Data.Text

-- | An Error successfully returned from the API.
-- The status code mirrors http - 4xx indicates the client made a bad request,
-- 5xx indicates the server etc.
data ErrorResponse = ErrorResponse
  { -- | The status code mirrors HTTP. I.E. 4xx indicates the client made a bad
    -- request. 5xx indicates the server encountered some internal error.
    _errorStatusCode   :: Int
  , _errorResponseBody :: ErrorResponseBody
  }
  deriving Show

-- | The body of an error successfully returned from the API.
data ErrorResponseBody = ErrorResponseBody
  {
    -- | A unique string identifying the specific type of error.
    _errorType        :: Text

    -- | A longer description of the meaning of the error.
  , _errorDescription :: Maybe Text

    -- | A link to further documentation on the error.
  , _errorURI         :: Text

    -- | Key-value pairs that are specific to the error type and may provide more
    -- detail as to what caused the error.
  , _errorAttributes  :: Maybe (Map Text Value)
  }
  deriving Show

instance FromJSON ErrorResponseBody where
  parseJSON v = case v of
    Object o
      -- TODO: Status may belong outside of ErrorResponse/ this instance should not exist.
      -> ErrorResponseBody <$> o .:  "error"
                           <*> o .:  "error_description"
                           <*> o .:  "error_uri"
                           <*> o .:? "error_attributes"

    _ -> typeMismatch "ErrorResponseBody" v

