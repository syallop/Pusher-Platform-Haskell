{-# LANGUAGE
    DataKinds
  , DeriveFunctor
  , DuplicateRecordFields
  , ExistentialQuantification
  , FunctionalDependencies
  , ConstraintKinds
  , RankNTypes
  , FlexibleInstances
  , KindSignatures
  , TypeFamilies
  , FlexibleContexts
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  #-}
{-|
Module      : Pusher.Client
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

This module defines the Classes 'ClientRequest' and 'ClientSubscribe' which
define how clients should issue request-responses and establish subscriptions to Pusher services.

'HTTP2Client' is a conforming instance exported by this package.

Consumers of @clients@ most likely want to use 'request' and 'subscribe' so that
they can pass and recieve objects in their data model.

- See 'Pusher.Client.Request' for documentation of how to write 'ToRequest' instances.
- See 'Pusher.Client.Subscribe' for documentation of how to write 'ToSubscribe' instances.
-}
module Pusher.Client
  (
  -- * Clients
  -- | Client's can make 'requests' and use 'subscribe' to establish 'Subscriptions'.
  --
  -- To create a Client, pass an instance of 'ClientRequest' and
  -- 'ClientSusbcribe' to 'mkClient'.
    Client ()
  , mkClient
  , request
  , subscribe

  -- * Request-Response
  -- | Clients with 'ClientRequest' instances understand how to send plain
  --  'Requests' to Pusher and return plan 'Responses'.
  --  It is likely you want to pass request-like types from your data model into
  --  'request' in order to recieve structured responses rather than operating
  --  with 'clientRequest' directly.
  --
  -- See the documentation in 'Pusher.Client.Request' for example usage.
  , ClientRequest
  , clientRequest

  -- * Long-lived Subscriptions
  -- | Client with 'ClientSubscribe' instances understand how to establish plain
  --  'Subscriptions' to Pusher.
  --
  --  It is likely you want to pass subscribe-like types from your data model
  --  into 'subscribe' in order to recieve structured events rather than
  --  operating with 'clientSubscribe' directly.
  , ClientSubscribe
  , clientSubscribe

  -- * Subscriptions
  -- | Clients establish subscriptions by calling 'subscribe'.
  -- Read events by calling 'readEvent'.
  , Subscription
  , readEvent
  , openStatus
  , close

  , readRawEvent
  )
  where

import Prelude hiding (read)

import Data.Aeson.Types
import qualified Data.Text as Text

import Pusher.Client.Request
import Pusher.Client.Result
import Pusher.Client.Subscribe
import Pusher.Model

-- | A Client can make 'requests' and establish 'Subscriptions' with
-- 'subscribe'.
data Client = forall client. (ClientRequest client, ClientSubscribe client) => Client client

mkClient :: (ClientRequest client, ClientSubscribe client) => client -> Client
mkClient = Client

instance ClientRequest Client where
  clientRequest (Client client) = clientRequest client

instance ClientSubscribe Client where
  clientSubscribe (Client client) = clientSubscribe client

-- | 'ClientRequest' @client@s are used to issue regular request-responses.
class ClientRequest client where
  -- | Issue a 'Request' object to a specific 'Instance' with optional 'AccessToken'.
  --
  -- Consumers should use 'request' to issue request-like types from their data model.
  -- See 'IsRequest' if you need to define such an instance.
  clientRequest
    :: client
    -> Instance
    -> Maybe AccessToken
    -> Request
    -> IO (PusherResult Response)

-- | Issue a @request@ to Pusher and return the @response@.
request
  :: forall request response client
   . ( IsRequest request response
     , ClientRequest client
     )
  => client
  -> Instance
  -> Maybe AccessToken
  -> request
  -> IO (PusherResult response)
request client instanceId mAccessToken req = do
  opaqueResult <- clientRequest client instanceId mAccessToken (toRequest req)
  case opaqueResult of
    PusherSuccess opaqueSuccess
      -> case parseEither fromResponse opaqueSuccess of
           Left err
             -> pure $ PusherFailure $ "Failed to parse response from '" <> (Text.pack . show $ opaqueSuccess) <> "'" <> ". With: " <> (Text.pack err)
           Right success
             -> pure $ PusherSuccess success

    PusherFailure errMsg
      -> pure $ PusherFailure $ "Failed to issue request " <> (Text.pack . show . toRequest $ req) <> " with: " <> errMsg

    PusherErrorResponse errResp
      -> pure $ PusherErrorResponse errResp

-- | 'ClientSubscribe' @client@s are used to establish long-lived 'subscription's.
class ClientSubscribe client where
  -- | Establish a @subscription@ to a specific 'Instance' with an optional 'AccessToken'.
  --
  -- Consumers should use 'subscribe' to establish subscriptions from their data model.
  -- See 'IsSubscribe' if you need to define such an instance.
  clientSubscribe
    :: client
    -> Instance
    -> Maybe AccessToken
    -> SubscriptionRequest
    -> IO (PusherResult (Subscription event))

-- | Initiate a subscription to Pusher.
--
-- After establishing a subscription it is likely you will want to recursively
-- call 'readEvent' to consume events.
subscribe
  :: forall subscriptionRequest client event
   . ( IsSubscribe subscriptionRequest event
     , ClientSubscribe client
     )
  => client
  -> Instance
  -> Maybe AccessToken
  -> subscriptionRequest
  -> IO (PusherResult (Subscription event))
subscribe client instanceId mAccessToken req = clientSubscribe client instanceId mAccessToken (toSubscribeRequest req)

