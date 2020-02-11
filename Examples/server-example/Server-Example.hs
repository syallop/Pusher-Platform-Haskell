{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Aeson
import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Encoding
import Data.Aeson.Types
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import Data.Map.Strict
import Data.Scientific
import Data.Text
import Data.Text
import Data.Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import Test.RandomStrings

import Pusher
import Pusher.Client.Token

instanceID  = "my-instance-id"
keyID       = "my-key-id"
keySecret   = error "Only supply key secret to trusted servers"
clusterName = US1
host        = PusherPlatform

main :: IO ()
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

  -- Create bob with the User model that deals with encoding/ decoding for us
  bobUserID <- pusherIO $ generateUserID "bob"
  Created bobUser <- pusherRequest accessToken $ CreateUser bobUserID "bob" "https://avatar.com/bob.jpg"
  pusherIO . print $ bobUser

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

generateUserID :: Text -> IO Text
generateUserID name = do
  suffix <- randomWord randomASCII 10
  pure $ name <> "-" <> pack suffix

createAccessTokenAtTrustedServer :: Text -> IO (Maybe AccessToken)
createAccessTokenAtTrustedServer user = do
  issuedAt <- getPOSIXTime
  let expiresAt = issuedAt + 60*60
  let subject = user
  let claims = Map.fromList [("su", JSON.Bool True)]

  pure $ mkAccessToken instanceID keyID keySecret issuedAt expiresAt subject claims

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

