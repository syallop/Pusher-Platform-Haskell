{-# LANGUAGE
    DataKinds
  , DeriveFunctor
  , DuplicateRecordFields
  , FunctionalDependencies
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  #-}
{-|
Module      : Pusher.Client.Token
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

This module provides convenience functions for operating with 'AccessToken's which are used to authorize request.
-}
module Pusher.Client.Token where

import Prelude hiding (read)

import qualified Data.Aeson as JSON
import qualified Data.Map.Strict as Map
import qualified Web.JWT as JWT

import Pusher.Model

-- | Create a signed token for an 'Instance' of a Pusher service.
mkAccessToken
  :: Instance          -- ^ Instance ID of the Pusher application
  -> KeyID             -- ^ Public id which identifies the key used to sign the token
  -> KeySecret         -- ^ Private secret for the key used to cryptographically sign the token
  -> IssuedAt          -- ^ Time the access token is issued at
  -> Expiry            -- ^ Time the access token expires. May be no longer than a day ahead.
  -> TokenSubject      -- ^ The Subject the token is concerned about. E.G. a UserID
  -> TokenClaims       -- ^ Claims/ properties the subject is being granted. E.G. "super-user":True
  -> Maybe AccessToken -- ^ A signed access token that can be passed to requests/ subscriptions to provide authentication
mkAccessToken inst keyID keySecret issuedAt expiresAt subject additionalClaims = do
  issuer  <- JWT.stringOrURI $ "api_keys/" <> keyID
  subject <- JWT.stringOrURI subject
  issued  <- JWT.numericDate issuedAt
  expiry  <- JWT.numericDate expiresAt
  let signer = JWT.HMACSecret keySecret
      header = JWT.JOSEHeader
                 { typ = Just "JWT"
                 , alg = Just JWT.HS256
                 , cty = Nothing
                 , kid = Nothing
                 }
      claims = JWT.JWTClaimsSet
        { iss = Just issuer
        , sub = Just subject
        , iat = Just issued
        , exp = Just expiry
        , aud = Nothing
        , nbf = Nothing
        , jti = Nothing
        , unregisteredClaims = JWT.ClaimsMap $ additionalClaims <> Map.fromList [("instance", JSON.String inst)]
        }
   in Just $ JWT.encodeSigned signer header claims

