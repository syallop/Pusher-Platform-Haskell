# Pusher-Platform-Haskell

This repository defines clients for Pusher platform services such as [Chatkit](https://pusher.com/chatkit).

It may be used to:
- Directly issue [requests](#making-requests) and establish long-lived [subscriptions](#establishing-subscriptions) to arbitrary endpoints.
- Build server-side SDKs (which have access to`SecretKey`s to authorize request-reponses)
- Build client-side SDKs (which obtain `AccessTokens` out of band from a server to issue request-responses and establish subscriptions)

Note:
- Channels and Beams are _not_ supported.
- This package is unofficial and alpha quality.

Continue reading this document to learn how to [install this dependency](#Installing),
[connect to an instance](#Connecting-to-an-instance), [make requests](#Making-Requests),
[establish subscriptions](#Establishing-Subscriptions) and [handle errors](#Errors). Or
consult [documentation](#Documentation).

The example fragments below are collected in a working [example application](/Examples/server-example/Server-Example.hs).

**Contents**:
- [Installing](#Installing)
- [Connecting to an instance](#Connecting-to-an-instance)
  - [AccessTokens](#AccessTokens)
- [Making Requests](#Making-Requests)
  - [Plain](#Plain-requests)
  - [Structured](#Structured-Requests-and-Responses)
- [Establishing Subscriptions](#Establishing-Subscriptions)
  - [Plain](#Plain-Subscriptions)
  - [Structured](#Structured-Subscriptions-and-Events)
- [Errors](#Errors)
  - [ErrorResponses](#ErrorResponses)
  - [Failures](#Failures)
  - [Short-circuiting](#Short-circuiting)
- [Documentation](#Documentation)
- [Module hierarchy](#Module-hierarchy)
- [Developing this library](#Developing-this-library)

## Installing

1. Start a project

If you have an existing application, skip to step 2. Otherwise, start a new
Haskell project. [Stack](https://docs.haskellstack.org/en/stable/README/) is
known to work but you should be able to use other tools such as cabal.

With stack:

```bash
mkdir MyApp && cd MyApp && stack init
```

2. Add `Pusher-Platform-Haskell` as a dependency to `MyApp.cabal`:

```haskell
  ...
  build-depends:       base >= 4.12
                     , pusher-platform-haskell
  ...
```

3. Tell your build tool where to find this dependency.

`Pusher-Platform-Haskell` is not currently on Hackage or a stack resolver.

If using stack, add this repository as a source to your `stack.yaml`:

```
resolver: lts-14.20
packages:
- .
extra-deps:
- git: "https://github.com/syallop/Pusher-Platform-Haskell"
  commit: master
```

If using cabal directly you may be able to install this repository globally:
```sh
git clone https://github.com/syallop/Pusher-Platform-Haskell.git && cd Pusher-Platform-Haskell && cabal install
```

You should now be able to `import Pusher` in your application.

Continue reading this document to learn how to
[connect to an instance](#Connecting-to-an-instance), [make requests](#Making-Requests),
[subscriptions](#Establishing-Subscriptions) and [handle errors](#Errors). Or
consult [documentation](#Documentation).

## Connecting to an instance

To interact with a Pusher platform service, you will need your instance and key
id. Currently Chatkit is the only supported service. Obtain credentials or
create a free instance in the [dashboard](https://dash.pusher.com).

The "Instance Locator" displayed in the dashboard takes the form `VERSION:CLUSTER:INSTANCEID`.
The "Secret Key" takes the form `KEYID:KEYSECRET`.

Note:
- The `keySecret` is the private key used to sign requests. It should NOT be
  shared with untrusted clients.
- The `instanceID` identifies your instance and can be shared
- The `keyID` identifies the secret key you will use to authorize requests and
  can be shared.
First obtain credentials for a Chatkit instance

Create an environment that points at a single instance of a Pusher service:
```haskell
import Pusher

instanceID  = "my-instance-id"
keyID       = "my-key-id"
keySecret   = error "Only supply key secret to trusted servers"
clusterName = US1
host        = PusherPlatform

main :: IO ()
main = do
  Just env <- mkPusherEnv instanceID keyID clusterName host []
  ...
```

Use this environment to issue requests and establish subscriptions by using
`runPusher` like:

```haskell
main :: IO
main = do
  Just env <- mkPusherEnv instanceID keyID clusterName host []

  result <- runPusher env pusherActions 
  case result of
    PusherSuccess ()
      -> putStrLn "Successfully executed actions"

    PusherErrorResponse errResp
      -> putStrLn $ "Got error response from api: " <> show errResp

    PusherFailure errMsg
      -> fail $ show errMsg

pusherActions :: Pusher ()
pusherActions = do
  pusherIO $ putStrLn "Hello world"
  -- More Pusher actions can be chained here. Failures will shortcircuit.
```

### AccessTokens
Most requests will require an `AccessToken` to be supplied for authorization. These are created
by signing a [JWT](https://tools.ietf.org/html/rfc7519) with your `SecretKey`.

`SecretKeys` must never be given to untrusted clients as it would allow them to
authorize any request.

If you are in an untrusted client context you cannot securely generate
`AccessTokens`. You should communicate with a trusted server context who is
responsible for deciding whether to grant you an appropriate `AccessToken`.

If you are in a trusted server context, you may generate `AccessToken`s:
```haskell
import Data.Map.Strict
import Data.Text
import Data.Time.Clock.POSIX
import Test.RandomStrings
import qualified Data.Aeson as JSON
import qualified Data.Map as Map

import Pusher.Client.Token

pusherActions :: Pusher ()
pusherActions = do
  pusherIO $ putStrLn "Hello world"
  -- More Pusher actions can be chained here. Failures will shortcircuit.

  -- Create an access token for alice
  aliceUserID <- pusherIO $ generateUserID "alice"
  accessToken <- pusherIO $ createAccessTokenAtTrustedServer aliceUserID

  pure ()

createAccessTokenAtTrustedServer :: Text -> IO (Maybe AccessToken)
createAccessTokenAtTrustedServer forUser = do
  issuedAt <- getPOSIXTime
  let expiresAt = issuedAt + 60*60
  let subject = forUser
  let claims = Map.fromList [("su", JSON.Bool True)]

  pure $ mkAccessToken instanceID keyID keySecret issuedAt expiresAt subject claims

generateUserID :: Text -> IO Text
generateUserID name = do
  suffix <- randomWord randomASCII 10
  pure $ name <> "-" <> pack suffix
```

## Making Requests
### Plain requests
Manually construct `Request`s and receive `Response`s:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)

pusherActions :: Pusher ()
pusherActions = do
  -- Create an access token for alice
  aliceUserID <- pusherIO $ generateUserID "alice"
  accessToken <- pusherIO $ createAccessTokenAtTrustedServer aliceUserID

  -- Construct Request to create alice. Response must still be parsed manually.
  let createAliceRequest = Request
        { _method  = "POST"
        , _headers = []
        , _service = "chatkit"
        , _version = "v6"
        , _path    = "/users"
        , _body    = Just .toStrict . encodingToLazyByteString . pairs . mconcat $
            [ "id"   .= aliceUserID
            , "name" .= ("alice"::Text)
            ]
        }
  (Response _ jsonBody _) <- pusherRequest accessToken createAliceRequest
  pusherIO $ putStrLn $ "Successfully created user: " <> show jsonBody
```

### Structured Requests and Responses

Constructing `Requests` like this can be tedious and error prone. `IsRequest`
can be used to work with more structured data.

For example, you may have (or want to define) a `CreateUser` operation for a
`Chatkit` service.

The model may look like:

```haskell
import Data.Text

-- A request to create a user with a name and avatar
data CreateUser = CreateUser
  { _createID     :: Text
  , _createName   :: Text
  , _createAvatar :: Text
  }

-- The successful response to creating a user
data CreateUserResponse = Created User

-- A successfully created user
-- In real code you may want to use stronger types than plain Text.
data User = User
  { _userID    :: Text
  , _userName  :: Text
  , _avatarURL :: Maybe Text
  , _createdAt :: Text
  , _updatedAt :: Text
  }
  deriving Show
```

These types should then be tied together with an `IsRequest` instance.
- `toRequest` describes how to transform a `CreateUser` request into a `Request` that clients can send.
- `fromResponse` describes how successful `Response`s should be parsed into the response model.

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Types

instance IsRequest CreateUser CreateUserResponse where
  toRequest (CreateUser id name avatar) = Request
    { _method  = "POST"
    , _headers = []
    , _service = "chatkit"
    , _version = "v6"
    , _path    = "/users"
    , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
      ["name"        .= name
      ,"id"          .= id
      ,"avatar_url"  .= avatar
      ]
    }

  fromResponse resp@(Response _headers body _mHeaders) = withStatus 201 resp $ Created <$> parseJSON body

instance FromJSON User where
  parseJSON v = case v of
           Object o -> User <$> o .: "id"
                            <*> o .: "name"
                            <*> o .:? "avatar_url"
                            <*> o .: "created_at"
                            <*> o .: "updated_at"
           _ -> typeMismatch "User" v
```

__Note__ that unsuccessful responses are handled by the clients which understand how to parse the `ErrorResponse` format.

You can then work with this model instead of plain requests:

```haskell
pusherActions :: Pusher ()
pusherActions = do
  -- Create an access token for alice
  aliceUserID <- pusherIO $ generateUserID "alice"
  accessToken <- pusherIO $ createAccessTokenAtTrustedServer aliceUserID

  -- Create bob with the User model that deals with encoding/ decoding for us
  bobUserID <- pusherIO $ generateUserID "bob"
  Created bobUser <- pusherRequest accessToken $ CreateUser bobUserID "bob" "https://avatar.com/bob.jpg"
  pusherIO . print $ bobUser
```

## Establishing Subscriptions
### Plain Subscriptions
Manually establish `Subscription`s and read the first event:

```haskell
pusherActions :: Pusher ()
pusherActions = do
  pusherIO $ putStrLn "Hello world"
  -- More Pusher actions can be chained here. Failures will shortcircuit.

  -- Create an access token for alice
  aliceUserID <- pusherIO $ generateUserID "alice"
  accessToken <- pusherIO $ createAccessTokenAtTrustedServer aliceUserID

  -- Construct Request to create alice. Response must still be parsed manually.
  let createAliceRequest = Request
        { _method  = "POST"
        , _headers = []
        , _service = "chatkit"
        , _version = "v6"
        , _path    = "/users"
        , _body    = Just .toStrict . encodingToLazyByteString . pairs . mconcat $
            [ "id"   .= aliceUserID
            , "name" .= ("alice"::Text)
            ]
        }
  (Response _ jsonBody _) <- pusherRequest accessToken createAliceRequest
  pusherIO $ putStrLn $ "Successfully created user: " <> show jsonBody

  -- Construct a subscription request to a users read cursors and read the first
  -- json event which must be parsed manually.
  let subscribeUserCursorsRequest = SubscriptionRequest
        { _headers = []
        , _service = "chatkit_cursors"
        , _version = "v2"
        , _path    = "/cursors/0/users/" <> aliceUserID
        }
  subscription <- pusherSubscribe accessToken subscribeUserCursorsRequest
  pusherIO $ putStrLn $ "Successfully subscribed to cursors for: alice "
  event <- pusherReadEvent Nothing subscription
  case event of
    Left (EventEndOfSubscription _status _headers _body)
      -> fail "Failed to read first event from subscription"

    Right (Event _eventID _headers jsonEvent)
      -> pusherIO $ putStrLn $ "Read first event: " <> show jsonEvent
  pusherClose subscription
```

You may then recursively read from the subscription until either:
- You are done and call `close`/ `pusherClose`
- The server is done and returns an `EventEndOfStream`

### Structured Subscriptions and Events

Constructing `SubscriptionRequests` and parsing the `Event`s can be tedious and
error prone. `IsSubscribe` can be used to work with more structured data.

For example, to define a `SubscribeUserCursors` operation for a `Chatkit`
service you may start by modelling the subscription request and event types
like:

```haskell
import Data.Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Scientific

-- A Request to subscribe to a users cursor events
data SubscribeUserCursors = SubscribeUserCursors
  { _subscribeUserID :: Text
  }

-- | Events from a User Cursor subscription.
data UserCursorEvent
  -- | The Users previously set Cursors at the time of opening the Subscription
  = InitialUserCursors
    { _timestamp :: Text
    , _cursors   :: [Cursor]
    }

  -- | The Users Cursor has been created or updated.
  | NewUserCursor
    { _timestamp :: Text
    , _cursor    :: Cursor
    }
  deriving Show

-- | A Cursor points to a Message in a Room.
data Cursor = Cursor
  { _type       :: CursorType
  , _roomID     :: Text
  , _userID     :: Text
  , _position   :: Int
  , _updatedAt  :: UTCTime
  }
  deriving Show

-- | Read Cursors are currently the only Cursor type.
data CursorType
  = ReadCursor
  deriving Show
```

These types should then be tied together with an `IsSubscribe` instance.
- `toSubscribeRequest` describes how to transform a `SubscribeUserCursors`
  request into a `SubscriptionRequest` that clients can use to establish a
  subscription.
- `fromSubscribeEvent` describes how successful `Event`s should be parsed into
  the event model.

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Types

instance IsSubscribe SubscribeUserCursors UserCursorEvent where
  toSubscribeRequest (SubscribeUserCursors userID) = SubscriptionRequest
   { _headers = []
   , _service = "chatkit_cursors"
   , _version = "v2"
   , _path    = "/cursors/0/users/" <> userID
   }

  fromSubscribeEvent (Event _eventID _headers body) = parseJSON body

instance FromJSON UserCursorEvent where
  parseJSON v = case v of
    Object o
      -> do eventName :: Text <- o .: "event_name"
            timestamp <- o .: "timestamp"
            dat       <- o .: "data"

            case eventName of
              "initial_state"
                -> InitialUserCursors
                   <$> pure timestamp
                   <*> dat .: "cursors"

              "new_cursor"
                -> NewUserCursor
                   <$> pure timestamp
                   <*> parseJSON (Object dat)

              unknown
                -> fail $ "Unknown User Cursor event: " <> show unknown
    _ -> typeMismatch "UserCursorEvent" v

instance FromJSON Cursor where
  parseJSON v = case v of
    Object o -> Cursor <$> o .: "cursor_type"
                       <*> o .: "room_id"
                       <*> o .: "user_id"
                       <*> o .: "position"
                       <*> o .: "updated_at"
    _ -> typeMismatch "Cursor" v

instance FromJSON CursorType where
  parseJSON v = case v of
    Number s -> case toBoundedInteger s of
      Nothing -> typeMismatch "CursorType" v
      Just (i :: Int) -> case i of
        0 -> pure ReadCursor
        _ -> typeMismatch "CursorType" v
    _          -> typeMismatch "CursorType" v
```

__Note__ that unsuccessful events are handled by the clients which understand
how to parse the 'ErrorResponse' format.

You can then work with this model instead of plain subscriptions:

```haskell
{-# LANGUAGE FlexibleContexts #-}

pusherActions :: Pusher ()
pusherActions = do
  -- Create an access token for alice
  aliceUserID <- pusherIO $ generateUserID "alice"
  accessToken <- pusherIO $ createAccessTokenAtTrustedServer aliceUserID

  -- Create bob with the User model that deals with encoding/ decoding for us
  bobUserID <- pusherIO $ generateUserID "bob"
  Created bobUser <- pusherRequest accessToken $ CreateUser bobUserID "bob" "https://avatar.com/bob.jpg"
  pusherIO . print $ bobUser

  -- Subscribe to a users cursors with the model that deals with
  -- encoding/ decoding events for us.
  subscription <- pusherSubscribe accessToken $ SubscribeUserCursors bobUserID
  pusherIO $ putStrLn $ "Successfully subscribed to cursors for: bob"
  read subscription
  where
    read subscription = do
      cursorEvent <- pusherReadEvent (Just 5000000) subscription
      case cursorEvent of
        Left (EventEndOfSubscription _status _headers _body)
          -> do pusherIO $ putStrLn "End of subscription"
                pure ()

        Right (InitialUserCursors timestamp cursors)
          -> do pusherIO $ putStrLn $ "Bob has cursors " <> show cursors <> " at " <> show timestamp
                read subscription

        Right (NewUserCursor timestamp cursor)
          -> do pusherIO $ putStrLn $ "Bob has a new cursor " <> show cursor <> " at " <> show timestamp
                read subscription
```

## Errors
Executing the `Pusher` type with `runPusher` returns a `PusherResult`. Asides
from success, the result may contain two classes of errors - `PusherFailures`
and `PusherErrorResponses`.

### ErrorResponses 
`PusherErrorResponses` indicate an 'expected' error returned by the API. The
contained `ErrorResponse` will have a status code, a description of the error
and a link to documentation.

The type looks like:
```haskell
data ErrorResponse = ErrorResponse
  { -- ^ The status code mirrors HTTP. I.E. 4xx indicates the client made a bad
    -- request. 5xx indicates the server encountered some internal error.
    _errorStatusCode   :: Int
  , _errorResponseBody :: ErrorResponseBody
  }

-- | An Error 'successfully' returned from the API.
-- The status code mirrors http - 4xx indicates the client made a bad request,
-- 5xx indicates the server etc.
data ErrorResponseBody = ErrorResponseBody
  {
    -- ^ A unique string identifying the specific type of error.
    _errorType        :: Text

    -- ^ A longer description of the meaning of the error.
  , _errorDescription :: Maybe Text

    -- ^ A link to further documentation on the error.
  , _errorURI         :: Text

    -- ^ Key-value pairs that are specific to the error type and may provide more
    -- detail as to what caused the error.
  , _errorAttributes  :: Maybe (Map Text Value)
  }
```

The status codes and presence of a `Retry-After` header indicate whether a
request can be retried.

E.G.

- 2xx could indicate a subscription has unexpected closed 'successfully' from
  the servers point of view. You may want to re-establish.
- 429 with `Retry-After` header indicates you have hit a ratelimit and should
  retry after the specified period.
- 4xx indicates the request is malformed in some way and likely cannot be
  retried without modifying some property indicated by the error type.
- 5xx indicates the server is having an internal problem. A `Retry-After` header
  should be present and indicate how soon you should retry the request.

### Failures
`PusherFailure`s indicate a logic error in the libraries implementation or it's
dependencies. You may be able to retry these requests but it is more likely the
library is in an invalid state and should be completely re-initialised.

For example, this could be caused by:
- An internal error in the underlying HTTP2 client
- A double close on a `Subscription`

### Short-circuiting
By default, both types of error short-circuit a `Pusher` computation. I.E. in a
`do` block, if one request fails, it's error will be immediately
returned and any remaining requests will not be issued. If you do NOT want this
behavior, `pusherTry` can be used to explicitly handle `ErrorResponses`.

## Documentation
A local copy of the reference docs can be built and opened with:
```bash
stack haddock --open
```
Note: This will build documentation for every transitive dependency and may take a long time on first run.

## Module hierarchy

```
├── Pusher
│   ├── Client
│   │   ├── Error
│   │   ├── HTTP2
│   │   ├── Request
│   │   ├── Result
│   │   ├── Subscribe
│   │   └── Token
│   └── Model
```

| Import                  | Contains                                                                                                                                                                                                                             |
| ----                    | ----                                                                                                                                                                                                                                 |
| Pusher                  | A `Pusher` type for chaining computations that make requests/ establish subscriptions. This type has a Monad interface with short-circuiting of failed operations.                                                                   |
| Pusher.Model            | Shared data types used in making requests/ subscriptions and parsing their responses/ events                                                                                                                                         |
| Pusher.Client           | The interface Client-like things must implement. Users of clients should use `request` from `ClientRequest` and `subscribe` from `ClientSubscribe`. Implementors of new clients should define `clientRequest` and `clientSubscribe`. |
| Pusher.Client.Error     | The format of 'successful' `ErrorResponses` returned by failing api calls                                                                                                                                                            |
| Pusher.Client.HTTP2     | A concrete implementation of a client that multiplexes requests over HTTP2. The `Pusher` type uses this concrete client by default                                                                                                   |
| Pusher.Client.Request   | Models traditional Request-Responses that all clients should understand                                                                                                                                                              |
| Pusher.Client.Result    | Requests to the api that may succeed, successfully fail or fail internally are wrapped by this module.                                                                                                                               |
| Pusher.Client.Subscribe | Models long-lived subscriptions/ streaming responses that all clients should understand.                                                                                                                                             |
| Pusher.Client.Token     | Convenience functions for generating `AccessTokens` which are used to authorize requests.                                                                                                                                            |

## Developing this library

Build with:
```bash
stack build
```

There are currently no tests.

