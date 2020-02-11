{-# LANGUAGE
    DuplicateRecordFields
  , EmptyDataDecls
  , MultiParamTypeClasses
  , DeriveFunctor
  , DeriveAnyClass
  , OverloadedStrings
  , TupleSections
  , TypeApplications
  #-}
{-|
Module      : Pusher.Client.Result
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

A 'PusherResult' encapsulates successful results from querying Pusher as well
as expected errors and opaque error messages thrown from client-side errors.

Expected errors are returned as 'ErrorResponse's from the API and can often be handled programatically. Other failures are unexpected an unlikely to be able to be handled in any way other than logging and re-initialising the world.
-}
module Pusher.Client.Result
  ( PusherResult (..)
  )
  where

import Prelude hiding (read)

import Data.Text

import Pusher.Client.Error

-- | Either some successful response @a@, an ErrorResponse returned by the api
-- or some error that does not form part of the api.
data PusherResult a
  = PusherSuccess a                   -- ^ A successful response from the api
  | PusherErrorResponse ErrorResponse -- ^ A successful 'ErrorResponse' from the api, indicating why the request failed
  | PusherFailure Text                -- ^ An unexpected failure from the api or library internals
  deriving (Functor,Applicative,Monad)

instance Show a => Show (PusherResult a) where
  show r = case r of
    PusherSuccess a
      -> show a
    PusherErrorResponse errResp
      -> "Error response from Pusher: " <> show errResp
    PusherFailure msg
      -> "Error: " <> show msg

