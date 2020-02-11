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
Module      : Pusher.Client.Request
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

This module models traditional request-responses to a Pusher service.
-}
module Pusher.Client.Request
  (
  -- * Model
  -- | At the core of the model are plain 'Request' and 'Response' types.
  -- They are unaware of clusters, instances and access tokens (which are
  -- supplied by clients).
  --
  -- You should only need to work with these types directly when writing new
  -- underlying Client implementations or when defining new operations on a
  -- Pusher service.
  --
  -- When issuing requests to a Pusher api, you should be using 'IsRequest'
  -- instances which allow modelling operations at a higher level.
    Request (..)
  , Response (..)

  -- * Building & parsing
  -- |
  -- Manually constructing and parsing 'Request's and 'Response's can be tedious
  -- and error prone. Instead 'IsRequest' instances can be used to work with
  -- models of individual operations.
  --
  -- For example, to define a @CreateUser@ operation for a @Chatkit@ service you
  -- may start by modeling the request and response type like:
  --
  -- @
  --  -- A request to create a user with a name and avatar
  --  data CreateUser = CreateUser Name AvatarURL
  --
  --  -- The successful response to creating a user
  --  data CreateUserResponse = Created User
  --
  --  -- A successfully created user
  --  data User = User
  --    { _userID    :: UserID
  --    , _userName  :: UserName
  --    , _avatarURL :: Maybe URL
  --    , _createdAt :: Timestamp
  --    , _updatedAt :: Timestamp
  --    }
  -- @
  --
  -- Tie these types together with an 'IsRequest' instance:
  --
  -- @
  --   instance IsRequest CreateUser CreateUserResponse where
  -- @
  --
  -- Define how to transform a @CreateUser@ request into a 'Request' that can be
  -- sent by a conforming Client:
  --
  -- @
  --   toRequest (CreateUser name avatar) = Request
  --     { _method  = "POST"
  --     , _headers = []
  --     , _service = "chatkit"
  --     , _version = "v6"
  --     , _path    = "/users"
  --     , _body    = Just . toStrict . encodingToLazyByteString . pairs . mconcat $
  --       ["name"        .= name
  --       ,"avatar_url"  .= avatar
  --       ]
  --     }
  -- @
  --
  -- Define how to parse successful responses like:
  --
  -- @
  --   fromResponse resp@(Response _headers body _mHeaders) = withStatus 201 resp $ CreateUserResponse <$> parseJSON body
  -- @
  --
  -- __Note__ that unsuccessful responses are handled by the clients which
  -- understand how to parse the 'ErrorResponse' format.
  --
  -- Which makes use of an Aeson Parser 'FromJSON' instance:
  --
  -- @
  --   instance FromJSON User where
  --       parseJSON v = case v of
  --         Object o -> User <$> o .: "id"
  --                          <*> o .: "name"
  --                          <*> o .:? "avatar_url"
  --                          <*> o .: "created_at"
  --                          <*> o .: "updated_at"
  --         _ -> typeMismatch "User" v
  -- @
  --
  -- You should now be able to work directly with the @CreateUser@ model by
  -- passing @CreateUser@ values where requests are expected. The response will
  -- be a @CreateUserResponse@.
  , IsRequest (..)

  -- * Helpers
  , lookupResponseStatus
  , withStatus
  , parseErrorResponse
  )
  where

import Prelude hiding (read)

import Control.Monad
import Data.ByteString
import Data.Aeson.Types
import Data.Text
import Network.HPACK
import qualified Data.ByteString.Char8 as ByteString

import Pusher.Client.Error
import Pusher.Model

-- | A 'Request' is a concrete operation on a versioned Pusher service. E.G. to get user @alice@ from a <https://pusher.com/chatkit chatkit> service.
--
-- Requests may be executed by Clients 'Pusher.Client.request' in order to return 'Response's.
--
-- Requests may be built with 'toRequest'.
data Request = Request
  { _method  :: Text             -- ^ The type of operation. Like a HTTP method. E.G. @GET@, @PUT@, etc.
  , _headers :: HeaderList       -- ^ Key-values that modify a request. Like HTTP headers.
  , _service :: Text             -- ^ The name of the Pusher service a request targets. E.G. @chatkit@
  , _version :: ServiceVersion   -- ^ The services version. E.G. @v6@
  , _path    :: Text             -- ^ The resource being operated upons path. E.G. @/users/alice@
  , _body    :: Maybe ByteString -- ^ An optional body. E.G. @"{ name: alice}"@
  }
  deriving Show

-- TODO: Should a request body always be JSON?

-- | A 'Response' is a returned by a Client when a 'Request' is issued to an
-- specific Pusher 'Instance'. It's status and body must be inspected to
-- detemine whether the response is successful or not.
--
-- Responses may be parsed with 'fromResponse'.
data Response = Response
  { _headers  :: HeaderList       -- ^ Additional details about the response may be kept in Headers. E.G. @Retry-After@ and HTTP2-like meta headers like @:status@.
  , _body     :: Value            -- ^ If a response has a body it will be JSON encoded.
  , _mHeaders :: Maybe HeaderList -- ^ Optional trailing headers. This is currently unused.
  }
-- TODO: Response is too specific to HTTP2.
-- - Separate status code?
-- - Drop trailing headers?
instance Show Response where
  show (Response headers body trailers) =
    let headerStr :: String
        headerStr = Prelude.foldr (\(k,v) acc -> ByteString.unpack k <> ":" <> ByteString.unpack v <> " " <> acc) "" headers
     in headerStr <> "\n" <> show body

-- | Lookup the status header of a Response.
-- 2xx indicates success
-- 3xx indicates temporary failures
-- 4xx indicates a client error
-- 5xx indicates a server error
-- @Nothing@ indicates a malformed response.
lookupResponseStatus :: Response -> Maybe Int
lookupResponseStatus (Response headers _ _) = lookup ":status" >=> fmap fst . ByteString.readInt $ headers

-- | Require that the response has a specific status code before proceeding with
-- the rest of the parser.
withStatus :: Int -> Response -> Parser a -> Parser a
withStatus expectStatus resp parser = case lookupResponseStatus resp of
  Nothing
    -> fail "Response without a status code"
  Just status
    | status == expectStatus
    -> parser
    | otherwise
    -> fail $ "Unexpected response" <> show resp

-- | Parse an 'ErrorResponse' from the body of a 'Response'.
parseErrorResponse :: Response -> Parser ErrorResponse
parseErrorResponse resp@(Response _headers body _mHeaders) = case lookupResponseStatus resp of
  Nothing
    -> fail "Response does not have a status code"

  Just status
    -> withObject "ErrorResponse" (\o -> ErrorResponse <$> pure status <*> parseJSON body) body

-- | Instances of 'IsRequest' can be made that define how to translate more descriptive types into Requests and how to decode their associated
-- Responses.
class IsRequest request response | request -> response, response -> request where
  -- | Convert a request-like operation in your data model to a 'Request' object
  -- that can be understood by Pusher Clients.
  toRequest    :: request -> Request

  -- | Parse a generic 'Response' type returned by Pusher Clients into your data
  -- models @response@ type.
  fromResponse :: Response -> Parser response

-- | The trivial instance pairs opaque Request and Responses
instance IsRequest Request Response where
  toRequest req = req
  fromResponse resp = pure resp

