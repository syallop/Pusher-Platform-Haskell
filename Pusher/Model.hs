{-# LANGUAGE
    DuplicateRecordFields
  , EmptyDataDecls
  , EmptyDataDeriving
  , OverloadedStrings
  , ScopedTypeVariables
  #-}
{-|
Module      : Pusher.Model
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Shared data types used in making Pusher requests/ subscriptions and parsing their responses/ events.

|-}
module Pusher.Model
  ( AccessToken
  , IssuedAt
  , Expiry
  , TokenClaims
  , TokenSubject
  , KeyID
  , KeySecret
  , Instance
  , ServiceName
  , ServiceVersion
  , ClusterName (..)
  , Host (..)

  , renderClusterName
  , renderHost
  )
  where

import Data.Aeson
import Data.ByteString
import Data.Map.Strict
import Data.Text
import Data.Time.Clock

-- TODO: Consider using newtypes rather than type-aliases in cases where
-- conflating a type could introduce errors.

-- | Unix timestamp denoting when an 'AccessToken' was issued.
type IssuedAt = NominalDiffTime

-- | Unix timestamp denoting when an 'AccessToken' should be considered to be
-- expired.
type Expiry = NominalDiffTime

-- | An 'AccessToken's claims are a map of key values that encode additional
-- properties of the token.
type TokenClaims = Map Text Value

-- | An 'AccessToken's subject describes who it was issued by/ is providing the
-- signature.
type TokenSubject = Text

-- | An AccessToken is passed to endpoints requiring auth to prove that the
-- contained claims (which may be things such as user ids, etc), expiry
-- etc have been approved by the owner of the Key Secret.
--
-- An AccessToken is a signed base64 encoded string.
--
-- In typical usage, a client will be issued with an AccessToken out of band by
-- your server (likely claiming they have a particular user-id). The client will
-- then pass the AccessToken with each request they make to prove that your
-- authorized their requests.
type AccessToken = Text

-- | Identifies the public ID of a key.
type KeyID = Text

-- | The secret component of a Key that only the instance owner should possess.
type KeySecret = ByteString

-- | An instance of some service.
type Instance = Text

-- | The service name a request is forwarded to.
type ServiceName = Text

-- | A services version.
type ServiceVersion = Text

-- | Hosts have Clusters that host services.
data ClusterName
  = US1
  | CustomCluster Text

-- | Render a ClusterName to its 'Text'tual form.
renderClusterName :: ClusterName -> Text
renderClusterName c = case c of
  US1 -> "us1"
  CustomCluster name -> name

-- | Hosts may have many clusters.
data Host
  = PusherPlatform
  | CustomHost Text

-- | Render a Host to its 'Text'ual form.
renderHost :: Host -> Text
renderHost h = case h of
  PusherPlatform
    -> "pusherplatform.io"

  CustomHost name
    -> name

