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
Module      : Pusher.Client.Subscribe
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

This module models long-lived subscriptions/ streaming responses from a Pusher service.
-}
module Pusher.Client.Subscribe
  (
  -- * Model
  -- | At the core of the model are 'SubscriptionRequest's, 'Subscription's and
  -- their events.
  -- They are unaware of clusters, instances and access tokens (which are
  -- supplied by clients).
  --
  -- You should only need to work with these types directly when writing new
  -- underlying Client implementations or when defining new subscription
  -- operations on a Pusher service.
  --
  -- When issuing subscriptions to a Pusher api, you should be using
  -- 'IsSubscribe' instances which allow modelling operations at a higher level.
    SubscriptionRequest (..)
  , RawEvent (..)
  , Event (..)
  , EventEndOfSubscription (..)

  -- * Subscriptions
  -- | Subscriptions are like a long-lived response that can be continually queried
  -- for new @event@s.
  --
  -- Subscriptions should be established by calling 'subscribe' on a client.
  -- Once established 'openStatus' provides the initial success status code.
  -- There should not be unsuccessful status codes as the client responsible for
  -- creating the subscription should fail with a descriptive 'ErrorResponse'.
  --
  -- Once opened, events can be read with 'readEvent' or 'readRawEvent' if the
  -- control events are relevant.
  --
  -- If the consumer is finished with a subscription, they should call 'close'
  -- to inform the server.
  , Subscription (..)
  , openStatus
  , readRawEvent
  , close

  -- * Building and parsing
  -- |
  -- Manually constructing and parsing 'SubscriptionRequest's and their
  -- subscriptions events can be tedious and error prone. Instead 'IsSubscribe'
  -- instances can be used to work with models of individual subscription
  -- operations.
  --
  -- For example, to define a @SubscribeRoomMemberships@ operation for a @Chatkit@ service
  -- you may start by modeling the subscription request and event types like:
  --
  -- @
  --   -- A Request to subscribe to room membership events:
  --   data SubscribeRoomMemberships = SubscribeRoomMemberships RoomID
  --
  --   -- Events from a Room membership subscription include joining and leaving events.
  --   data RoomMembershipEvent
  --     =  UserJoined Timestamp UserID
  --     | UserLeft Timestamp UserID
  -- @
  --
  -- Tie these types together with an 'IsSubscribe' instance:
  --
  -- @
  --   instance IsSubscribe SubscribeRoomMembership RoomMembershipEvent where
  -- @
  --
  -- Define how to transform a @SubscribeRoomMemberships@ request into a
  -- 'SubscriptionRequest' that can be used by a conforming Client to establish
  -- a subscription:
  --
  -- @
  --   toSubscribeRequest (SubscribeRoomMemberships roomID) = SubscriptionRequest
  --    { _headers = []
  --    , _service = "chatkit"
  --    , _version = "v6"
  --    , _path    = "/rooms/" <> roomID <> "/memberships"
  --    }
  -- @
  --
  -- Define how to parse successful individual events  like:
  --
  -- @
  --   fromSubscribeEvent (Event _eventID _headers body) = parseJSON body
  -- @
  --
  -- __Note__ that unsuccessful events/ subscription ends (end of streams) are
  -- handled by the clients which understand how to parse the 'ErrorResponse'
  -- format.
  --
  -- Event parsing makes use of an Aeson Parser 'FromJSON' instance:
  --
  -- @
  --   instance FromJSON RoomMembershipEvent where
  --     parseJSON v = case v of
  --       Object o
  --         -> do eventName :: Text <- o .: "event_name"
  --               timestamp         <- o .: "timestamp"
  --               dat               <- o .: "data"
  --
  --              case eventName of
  --                "user_joined"
  --                  -> UserJoined
  --                       <$> pure timestamp
  --                       <*> dat .: "user_id"
  --
  --                "user_left"
  --                  -> UserLeft
  --                       <$> pure timestamp
  --                       <*> dat .: "user_id"
  --                unknown
  --                  -> fail $ "Unknown Room membership event: " <> show unknown
  --      _ -> typeMismatch "RoomMembershipEvent" v
  -- @
  --
  -- You should now be able to work directly with the @SubscribeRoomMembership@
  -- model by passing @SubscribeRoomMembership@ where subscription requests are
  -- expected. The events will be @RoomMembershipEvent@s
  , IsSubscribe (..)
  , readEvent
  )
  where

import Prelude hiding (read)

import Data.Text
import Data.Aeson
import Data.Map (Map)
import Data.Aeson.Types
import Data.Vector
import Network.HPACK
import qualified Data.Text as Text

import Pusher.Model
import Pusher.Client.Result

-- | A 'SubscriptionRequest' is a concrete operation on a versioned Pusher
-- service. E.G. to subscribe to room membership changes in the from a <https://pusher.com/chatkit chatkit> service.
--
-- SubscriptionRequests may be built with 'toSubscribeRequest'.
data SubscriptionRequest = SubscriptionRequest
  { _headers :: HeaderList
  , _service :: Text           -- The name of the service a subscription targets
  , _version :: ServiceVersion
  , _path    :: Text
  }
  deriving Show

-- | A 'RawEvent' is returned by a Client when reading from a 'Subscription' as
-- established by a 'SubscriptionRequest' to a specific Pusher 'Instance'.
--
-- The primary RawEvent consumers are interested in will be the 'Event'
-- contained in the 'RegularEvent'. This is the event 'IsSubscribe' instances
-- parse events from.
data RawEvent
  -- | This event is recieved when the api chooses to terminate the
  -- subscription. This can happen in success and failure conditions. The status
  -- code and headers of the 'EventEndOfSubscription' should be consulted to
  -- determine which.
  = EndOfSubscription
    { _eos :: EventEndOfSubscription
    }

  -- | The primary type of event a consumer is interested in.
  -- 'IsSubscribe' instances will parse this event.
  | RegularEvent
    { _event :: Event
    }

  -- | ControlEvents pertain to the subscriptions connection itself E.G. pings
  -- to keep the subscription alive.
  -- The body is an opaque JSON value. Client implementations should consult
  -- protocol spec to determine what to do with these events.
  | ControlEvent
    { _body :: Value
    }

  deriving Show
instance FromJSON RawEvent where
  parseJSON v = case v of
    Array vector
      -> let mEventType = vector !? 0
          in case mEventType of
               Nothing
                 -> fail $ "Raw Event has no event type at index 0: " <> show vector

               Just o
                 -> case o of
                      Number 0
                        -> ControlEvent
                           <$> maybe (fail $ "ControlEvent has no body index at 1: " <> show vector) pure (vector !? 1)

                      Number 1
                         -> (\eventID headers body -> RegularEvent $ Event eventID headers body)
                            <$> (maybe (fail $ "Event has no eventID index at 1: " <> show vector) parseJSON (vector !? 1))
                            <*> (maybe (fail $ "Event has no headers index at 2: " <> show vector) parseJSON (vector !? 2))
                            <*> (maybe (fail $ "Event has no body index at 3: "    <> show vector) parseJSON (vector !? 3))

                      Number 255
                         -> (\status headers body -> EndOfSubscription $ EventEndOfSubscription status headers body)
                            <$> maybe (fail $ "Event has no status index at 1: "  <> show vector) parseJSON (vector !? 1)
                            <*> maybe (fail $ "Event has no headers index at 2: " <> show vector) parseJSON (vector !? 2)
                            <*> maybe (fail $ "Event has no body index at 3: "    <> show vector) parseJSON (vector !? 3)

                      _ -> fail $ "Did not find a known event type Number at index 0 (0,1,255): " <> show vector
    _ -> typeMismatch "RawEvent" v

-- | Events are recieved on a 'Subscription' and directly pertain to the
-- topic (I.E. they are not control events or EndOfSubscriptions).
--
-- An 'Event' has an id, headers and an unparsed JSON body.
--
-- This is the 'Event' that 'fromSubscribeEvent' consumes.
data Event = Event
  { _eventID :: Text          -- ^ Events have a unique ID useful for client implementations who may want to ignore duplicate IDs or supply old event ids for resumable subscriptions.
  , _headers :: Map Text Text -- ^ Additional details about the Event may be kept in headers.
  , _body    :: Value         -- ^ Event bodys are JSON encoded.
  }
  deriving Show
-- TODO: Expose a consistent header type. Request uses HeaderList.

-- | An EventEndOfSubscription may be recieved when waiting for a RegularEvent.
-- It indicates that the subscription has been terminated by the server.
--
-- The status code indicates whether the termination was due to success (Like
-- the subscription being permanently out of events) or due to some failure
-- (such as auth expiry).
--
-- The presence of a 'Retry-After' header indicates whether the consumer may
-- attempt to re-establish the subscription.
data EventEndOfSubscription = EventEndOfSubscription
  { _status  :: Int           -- ^ The status corresponds to a HTTP status code and indicates whether the subscription __ended__ successfully or not.
  , _headers :: Map Text Text -- ^ Additional details about the event may be kept in headers E.G. @Retry-After@.
  , _body    :: Value         -- ^ EndOfSubscription bodys are JSON encoded
  }
  deriving Show

-- | The result of issuing a 'SubscriptionRequest' should be a 'Subscription'.
--
-- Users are likely to call 'readEvent' recursively and handle recieved events.
data Subscription event = Subscription
  { _openStatus   :: Maybe Int
  , _readRawEvent :: IO (PusherResult RawEvent)
  , _close        :: IO (Maybe ())
  }

instance Show event => Show (Subscription event) where
  show (Subscription open _ _) = "Subscription that opened with status " <> show open

  -- | Retrieve the status code returned from establishing the Subscription.
  -- 2xx indicates success
  -- 3xx indicates temporary errors
  -- 4xx indicates client errors
  -- 5xx indicates server errors
  --
  -- Note this is __not__ the same as the status code of a terminated
  -- subscription. This status code is found on the 'EndOfSubscription' event.
openStatus :: Subscription event -> Maybe Int
openStatus (Subscription status _ _) = status

-- | Block waiting for a 'RawEvent' on a subscription.
readRawEvent :: Subscription event -> IO (PusherResult RawEvent)
readRawEvent (Subscription _ f _) = f

-- | Indicate that we are done reading from the subscription. 'readRawEvent'
-- should not succeed after calling this function.
close :: Subscription event -> IO (Maybe ())
close (Subscription _ _ f) = f

--- | Read an 'event' from the subscription.
--- - Control events are ignored.
--- - End Of Subscription indicates the server considers the Subscription closed.
---   It should no longer be read from.
readEvent
  :: (IsSubscribe req event)
  => Subscription event
  -> IO (PusherResult (Either EventEndOfSubscription event))
readEvent subscription = do
  eRawEvent <- readRawEvent subscription
  case eRawEvent of
    PusherFailure errMsg
      -> pure $ PusherFailure errMsg

    PusherErrorResponse errResp
      -> pure $ PusherErrorResponse errResp

    PusherSuccess rawEvent
      -> case rawEvent of
           -- TODO: Are the only control events heartbeats?
           ControlEvent _controlBody
             -> readEvent subscription

          -- TODO: Maybe mark the underlying Stream as closed. If not, the next read is
          -- likely to error. The caller shouldnt be making further reads though if they
          -- have multiple readers they would need to co-ordinate while we already have
          -- the internal means to coordinate. Again, the caller should likely only have a
          -- single reader so..
           EndOfSubscription eos
             -> pure $ PusherSuccess $ Left eos

           RegularEvent event
             -> case parseEither fromSubscribeEvent event of
                  Left err
                    -> pure $ PusherFailure $ "Failed to parse event from subscription '" <> (Text.pack . show $ event) <> "'" <> ". With: " <> (Text.pack err)
                  Right ev
                     -> pure $ PusherSuccess $ Right ev

-- | IsSubscribe pairs a subscription @req@uest type with a type of @event@s a
-- successfully opened subscription will emit.
--
-- [@req@] can be built into a generic SubscriptionRequest and have an associated
-- service and version.
-- [@event@] can be parsed from the generic Events a Subscription emits.
class IsSubscribe req event | req -> event, event -> req where
  -- | Convert a @req@ into a generic SubscriptionRequest.
  toSubscribeRequest :: req -> SubscriptionRequest

  -- | Parse a generic Event into an @event@.
  fromSubscribeEvent :: Event -> Parser event

-- | The trivial instance pairs opaque SubscriptionRequests and Events.
instance IsSubscribe SubscriptionRequest Event where
  toSubscribeRequest req = req
  fromSubscribeEvent ev = pure ev

