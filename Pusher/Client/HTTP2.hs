{-# LANGUAGE
    DataKinds
  , DeriveFunctor
  , DuplicateRecordFields
  , FunctionalDependencies
  , FlexibleInstances
  , TypeFamilies
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  #-}
{-|
Module      : Pusher.Client.HTTP2
Copyright   : (c) Samuel A. Yallop, 2019
Maintainer  : syallop@gmail.com
Stability   : experimental

This module provides the means to create a client that points
at a single Pusher cluster with an underlying HTTP2 client.
-}
module Pusher.Client.HTTP2
  (
    -- * Construct a client
    -- | Construct a client cable of issuing requests and establishing
    --   subscriptions to a Pusher cluster.
    HTTP2ClientOptions (..)
  , HTTP2Client ()
  , mkHTTP2Client

  -- * Requests
  -- | HTTP2Client has a ClientRequest instance where request = http2Request
  , http2Request

  -- * Subscriptions
  -- | HTTP2Client has a ClientSubscribe instance where subscribe = http2Subscribe
  -- which returns a HTTP2Subscription
  , HTTP2Subscription ()
  , http2Subscribe

  -- * HTTP2Subscriptions have a Subscription instance and can have their open
  -- status inspected, events read and be closed for further reads.
  , http2OpenStatus
  , http2ReadRawEvent
  , http2Close
  )
  where

import Prelude hiding (read)

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Default.Class
import Data.IORef
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Network.HPACK
import Network.HTTP2
import Network.HTTP2.Client
import Network.HTTP2.Client.Helpers
import Network.TLS as TLS hiding (sendData, Header)
import Network.TLS.Extra.Cipher as TLS
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy  as L
import qualified Data.Text as Text

import Pusher.Model
import Pusher.Client
import Pusher.Client.Result
import Pusher.Client.Request
import Pusher.Client.Error
import Pusher.Client.Subscribe

-- | Options for constructing a HTTP2Client
data HTTP2ClientOptions = HTTP2ClientOptions
  { _cluster       :: ClusterName
  , _host          :: Host
  , _commonHeaders :: HeaderList
  }

-- | A HTTP2Client can be used to issue requests and subscriptions to a Pusher
-- cluster over a HTTP2 protocol.
data HTTP2Client = HTTP2Client
  { _authority     :: Text
  , _client        :: Http2Client
  , _commonHeaders :: HeaderList
  }

instance ClientRequest HTTP2Client where
  clientRequest client inst mAccessToken req = do
    eResp <- http2Request client inst mAccessToken req
    case eResp of
      PusherFailure errMsg
        -> pure (PusherFailure errMsg)

      PusherErrorResponse errResp
        -> pure (PusherErrorResponse errResp)

      PusherSuccess resp
        -> case lookupResponseStatus resp of
             Nothing
               -> pure (PusherFailure $ "Response does not have a status code " <> (Text.pack . show $ resp))

             Just status
               | status < 200 || 300 <= status
               -> case parseEither parseErrorResponse resp of
                    Left err
                      -> pure (PusherFailure $ "Failed to parse error response from'" <> (Text.pack . show $ resp) <> "'. With: " <> (Text.pack err))
                    Right errResp
                      -> pure (PusherErrorResponse errResp)

               | otherwise
               -> pure (PusherSuccess resp)

instance ClientSubscribe HTTP2Client where
  clientSubscribe client inst mAccessToken req = do
   eResp <- http2Subscribe client inst mAccessToken req
   case eResp of
    PusherFailure errMsg
      -> pure (PusherFailure errMsg)

    PusherErrorResponse errResp
      -> pure (PusherErrorResponse errResp)

    PusherSuccess subscriptionResponse
      -> do eSubscription <- http2Open subscriptionResponse
            case eSubscription of
              PusherFailure errMsg
                -> pure (PusherFailure errMsg)

              PusherErrorResponse errResp
                -> pure (PusherErrorResponse errResp)

              PusherSuccess (subscription@(HTTP2Subscription _ _ _ _ headers))
                -> case http2OpenStatus subscription of
                     Nothing
                       -> pure (PusherFailure $ "Subscription open does not have a status code " <> (Text.pack . show $ subscription))

                     -- If the status indicates an error opening the subscription, attempt to read an ErrorResponse body
                     Just status
                       | status < 200 || 300 <= status
                       -> do eErrResp <- http2ReadConnectErrorResponse subscription
                             case eErrResp of
                               Left errMsg
                                 -> pure (PusherFailure errMsg)

                               Right errResp
                                 -> pure (PusherErrorResponse errResp)

                       | otherwise
                       -> pure (PusherSuccess $ http2Subscription subscription)

-- | Cast a 'HTTP2Subscription' to the general 'Subscription' type clients
-- understand.
http2Subscription :: HTTP2Subscription event -> Subscription event
http2Subscription http2Subscription = Subscription
  { _openStatus   = http2OpenStatus   http2Subscription
  , _readRawEvent = http2ReadRawEvent http2Subscription
  , _close        = http2Close        http2Subscription
  }

-- | Create a new ClusterClient
mkHTTP2Client :: HTTP2ClientOptions -> IO (Maybe HTTP2Client)
mkHTTP2Client (HTTP2ClientOptions cluster host commonHeaders) = do
  eClient <- runClientIO $ do
    frameConnection <- newHttp2FrameConnection (Text.unpack authority) 443 (Just tlsParams)
    client <- newHttp2Client frameConnection 8192 8192 [(SettingsInitialWindowSize,10000000)] defaultGoAwayHandler ignoreFallbackHandler
    return . Just $ HTTP2Client authority client commonHeaders
  case eClient of
    Left clientError
      -> fail $ "Failed to create client: " <> show clientError

    Right mClient
      -> return mClient
  where
   authority = renderClusterName cluster <> "." <> renderHost host

   tlsParams :: ClientParams
   tlsParams = TLS.ClientParams
     { TLS.clientWantSessionResume       = Nothing
     , TLS.clientUseMaxFragmentLength    = Nothing
     , TLS.clientServerIdentification    = (Text.unpack authority, "")
     , TLS.clientUseServerNameIndication = True
     , TLS.clientShared                  = def
     , TLS.clientHooks                   = def { TLS.onServerCertificate = \_ _ _ _ -> return [] }
     , TLS.clientSupported               = def { TLS.supportedCiphers = TLS.ciphersuite_default }
     , TLS.clientDebug                   = def
     }

-- | Issue a request
http2Request
  :: HTTP2Client
  -> Instance
  -> Maybe AccessToken
  -> Request
  -> IO (PusherResult Response)
http2Request (HTTP2Client authority client commonHeaders) inst mAccessToken (Request method reqHeaders service version path mBody) = do
  eResp :: Either ClientError (Either TooMuchConcurrency (Either Text (Either ErrorResponse Response))) <- runClientIO $ withHttp2Stream client $ \stream ->
    let fullPath :: Text
        fullPath = "/services/"<>service<>"/"<>version<>"/"<>inst<>"/"<>path

        requestHeaders :: HeaderList
        requestHeaders =
          [ (":method"   , encodeUtf8 method)
          , (":scheme"   , "https")
          , (":path"     , encodeUtf8 fullPath)
          , (":authority", encodeUtf8 authority)
          ] <> reqHeaders
            <> commonHeaders
            <> (case mAccessToken of
                 Nothing
                   -> []
                 Just accessToken
                   -> [mkAuthorizationHeader accessToken]
               )
        initStream = headers stream requestHeaders (setEndHeader . (if isJust mBody then id else setEndStream))
        resetPushPromises _ pps _ _ _ = _rst pps RefusedStream

        handler :: IncomingFlowControl -> OutgoingFlowControl -> ClientIO (Either Text (Either ErrorResponse Response))
        handler incomingStreamFlow outgoingStreamFlow = do
          -- If there's a body, upload it
          () <- case mBody of
                  Nothing
                    -> pure ()

                  Just body
                    -> do upload body (\frameFlags -> frameFlags) client (_outgoingFlowControl client) stream outgoingStreamFlow
                          sendData client stream setEndStream ""

          -- Wait for the result
          streamResult <- waitStream stream incomingStreamFlow resetPushPromises
          case fromStreamResult streamResult of
            Left errorCode
              -> pure $ Left $ "error from stream result " <> (Text.pack $ show errorCode)

            Right (headerList, payload, mHeaderList)
              -> case decode . L.fromStrict $ payload of
                   -- Payload is not a JSON Object
                   Nothing
                     -- TODO: Are we losing information calling this Null? =>
                     -- - Can parse errors be treated as Null?
                     -- - Are we covering the empty body?
                     -- TODO: Should detect ErrorResponses here
                     -> return $ Right $ Right $ Response headerList JSON.Null mHeaderList

                   Just object
                     -> return $ Right $ Right $ Response headerList object mHeaderList
     in StreamDefinition initStream handler

  case eResp of
    Left clientError
      -> pure $ PusherFailure $ "Client error in request " <> (Text.pack $ show clientError)

    Right (Left _tooMuchConcurrency)
      -> pure $ PusherFailure "Too much concurrency"

    Right (Right (Left err))
      -> pure $ PusherFailure err

    Right (Right (Right (Left errResp)))
      -> pure $ PusherErrorResponse errResp

    Right (Right (Right (Right resp)))
      -> pure $ PusherSuccess resp

-- | AccessTokens are encoded as a JWT header.
mkAuthorizationHeader :: AccessToken -> Header
mkAuthorizationHeader t = ("Authorization","Bearer "<> encodeUtf8 t)

-- | Subscriptions emit events until closed by either side.
data HTTP2Subscription event = HTTP2Subscription
  { _stream                        :: Http2Stream
  , _streamClosed                  :: IORef Bool
  , _connectionIncomingFlowControl :: IncomingFlowControl
  , _streamIncomingFlowControl     :: IncomingFlowControl
  , _headers                       :: HeaderList
  }
instance Show (HTTP2Subscription event) where
  show _ = "SUBSCRIPTION"

-- | An internal response to a SubscriptionRequest.
-- TODO:
-- - Does this need to exist as it isn't exposed?
-- - If the subscription class allowed observing partial opens this would be
--   an instance.
data SubscriptionResponse = SubscriptionResponse
  { _stream                        :: Http2Stream
  , _streamClosed                  :: IORef Bool
  , _connectionIncomingFlowControl :: IncomingFlowControl
  , _streamIncomingFlowControl     :: IncomingFlowControl
  }

-- | Issue a subscription request to a ClusterClient.
http2Subscribe
  :: HTTP2Client
  -> Instance
  -> Maybe AccessToken
  -> SubscriptionRequest
  -> IO (PusherResult SubscriptionResponse)
http2Subscribe (HTTP2Client authority client commonHeaders) inst mAccessToken (SubscriptionRequest reqHeaders service version path) = do
  eResp :: Either ClientError (Either TooMuchConcurrency (Either Text (Either ErrorResponse SubscriptionResponse))) <- runClientIO $ withHttp2Stream client $ \stream ->
    let fullPath :: Text
        fullPath = "/services/"<>service<>"/"<>version<>"/"<>inst<>"/"<>path

        incomingConnectionFlow = _incomingFlowControl client
        requestHeaders :: HeaderList
        requestHeaders =
          [ (":method"   , encodeUtf8 "SUBSCRIBE")
          , (":scheme"   , "https")
          , (":path"     , encodeUtf8 fullPath)
          , (":authority", encodeUtf8 authority)
          ] <> reqHeaders
            <> commonHeaders
            <> (case mAccessToken of
                 Nothing
                   -> []
                 Just accessToken
                   -> [mkAuthorizationHeader accessToken]
               )
        initStream = headers stream requestHeaders (setEndHeader . setEndStream)

        handler :: IncomingFlowControl -> OutgoingFlowControl -> ClientIO (Either Text (Either ErrorResponse SubscriptionResponse))
        handler incomingStreamFlow _outgoingStreamFlow = do
          -- Subscriptions do not (currently?) accept request bodies.
          streamClosed <- liftIO . newIORef $ False
          return $ Right $ Right $ SubscriptionResponse stream streamClosed incomingConnectionFlow incomingStreamFlow
      in StreamDefinition initStream handler
  case eResp of
    Left clientError
      -> pure $ PusherFailure $ "Client error in subscription " <> (Text.pack . show $ clientError)

    Right (Left _tooMuchConcurrency)
      -> pure $ PusherFailure $ "Too much concurrency in subscription"

    Right (Right (Left err))
      -> pure $ PusherFailure err

    Right (Right (Right (Left errResp)))
      -> pure $ PusherErrorResponse errResp

    Right (Right (Right (Right resp)))
      -> pure $ PusherSuccess resp

-- | Lookup the status header returned when the subscription was established.
http2OpenStatus :: HTTP2Subscription e -> Maybe Int
http2OpenStatus (HTTP2Subscription _ _ _ _ headers) = lookup ":status" >=> fmap fst . ByteString.readInt $ headers

-- Replenish the stream flow control by a given length and send a connection
-- flow control WINDOW_UPDATE frame.
replenishFlowControls :: IncomingFlowControl -> IncomingFlowControl -> Int -> ClientIO ()
replenishFlowControls incomingConnectionFlowControl incomingStreamFlowControl length = do
  _ <- liftIO $ _consumeCredit incomingStreamFlowControl length
  liftIO $ _addCredit incomingStreamFlowControl length
  _ <- _updateWindow incomingStreamFlowControl
  -- connection-level flow control already accounted for in the
  -- 'creditDataFramesStep' of 'dispatchLoop' in http2-client
  _ <- _updateWindow incomingConnectionFlowControl
  return ()

-- | Open a Subscription from a SubscriptionResponse gets the initial headers.
http2Open :: SubscriptionResponse -> IO (PusherResult (HTTP2Subscription event))
http2Open (SubscriptionResponse stream streamClosed incomingConnectionFlowControl incomingStreamFlowControl) = do
  closed <- readIORef streamClosed
  if closed
    then pure $ PusherFailure "Cannot open a closed subscription"
    else do eSubscription <- runClientIO $ waitForHeaders stream
            case eSubscription of
              Left clientError
                -> pure $ PusherFailure $ "Subscription could not be opened due to an error in the underlying http client " <> (Text.pack $ show clientError)

              Right (Left errMsg)
                -> pure $ PusherFailure errMsg

              Right (Right subscription)
                -> pure $ PusherSuccess subscription
  where
    waitForHeaders stream = do
      ev <- _waitEvent stream
      case ev of
        StreamHeadersEvent frameHeader headers
          -> do -- Set he streamClosed IORef if the stream has been closed
                let streamClosedByServer = testEndStream $ flags frameHeader
                when streamClosedByServer $ liftIO $ writeIORef streamClosed True
                closed <- liftIO $ readIORef streamClosed

                if closed
                  then pure $ Left "Subscription closed while opening"
                  else pure $ Right $ HTTP2Subscription stream streamClosed incomingConnectionFlowControl incomingStreamFlowControl headers

        -- Consume push promises and continue waiting
        StreamPushPromiseEvent _ ppStreamId ppHeaders
          -> do let resetPushPromises _ pps _ _ _ = _rst pps RefusedStream
                _handlePushPromise stream ppStreamId ppHeaders resetPushPromises
                waitForHeaders stream

        unexpectedEvent
          -> pure $ Left $ "Unexpected http event when attempting to establish subscription: " <> (Text.pack $ show unexpectedEvent)



-- | Read a new RawEvent from a Subscription.
-- Block for RawEvents. Nothing when closed.
http2ReadRawEvent :: HTTP2Subscription event -> IO (PusherResult RawEvent)
http2ReadRawEvent (HTTP2Subscription stream streamClosed incomingConnectionFlowControl incomingStreamFlowControl _openHeaders) = do
  closed <- readIORef streamClosed
  if closed
    then pure $ PusherFailure "Cannot read event from closed subscription"
    else do eEvent <- runClientIO $ waitForRawEvent stream
            case eEvent of
              Left clientError
                -> pure $ PusherFailure $ "Client error in request " <> (Text.pack $ show clientError)

              Right mEvent
                -> return mEvent
  where
    waitForRawEvent stream = do
      httpEv <- _waitEvent stream
      case httpEv of
        StreamDataEvent dataFrameHeader body
          -> do -- TODO: Is 'replenishFlowControls' performing the entire
                -- consumeCredit, addCredit, updateWindow dance?
                replenishFlowControls incomingConnectionFlowControl incomingStreamFlowControl (payloadLength dataFrameHeader)
                let streamClosedByServer = testEndStream $ flags dataFrameHeader
                when streamClosedByServer $ liftIO $ writeIORef streamClosed True

                case eitherDecode . L.fromStrict $ body of
                  Left err
                    -> pure $ PusherFailure $ "Failed to decode raw event from subscription body: " <> (Text.pack $ show body) <> ". with error :" <> (Text.pack $ show err)

                  Right rawEvent
                    -> return $ PusherSuccess $ rawEvent

        -- Throw error if headers are recieved. We should have recieved the
        -- initial headers and don't support trailers and so this indicates an
        -- invalid state.
        StreamHeadersEvent _ hdrs
          -> pure $ PusherFailure $ "Expected data on http2 stream in subscription but recieved headers: " <> (Text.pack $ show hdrs)

        -- Consume push promises and continue waiting
        StreamPushPromiseEvent _ ppStreamId ppHeaders
          -> do let resetPushPromises _ pps _ _ _ = _rst pps RefusedStream
                _handlePushPromise stream ppStreamId ppHeaders resetPushPromises
                waitForRawEvent stream

        h2Event
          -> pure $ PusherFailure $ "Expected data on http2 stream in subscription, but received: " <> (Text.pack $ show h2Event)

-- Attempt to read a Subscription as if it has failed on open and returned an ErrorResponse.
-- This is different to an ErrorResponse returned as an event some time after a successful open.
http2ReadConnectErrorResponse :: HTTP2Subscription event -> IO (Either Text ErrorResponse)
http2ReadConnectErrorResponse subscription@(HTTP2Subscription stream streamClosed incomingConnectionFlowControl incomingStreamFlowControl openHeaders) = do
  closed <- readIORef streamClosed
  if closed
    then pure $ Left "Cannot attempt to read a connect error response from a closed subscription"
    else case http2OpenStatus subscription of
           Nothing
             -> pure $ Left "Cannot read Subscription open ErrorResponse as it does not have a status"

           Just openStatus
             -> do eErrResp <- runClientIO $ waitForErrorResponse stream
                   case eErrResp of
                     Left clientError
                       -> pure $ Left $ "Client error reading subscription error response" <> (Text.pack $ show clientError)

                     Right (Left errMsg)
                       -> pure $ Left errMsg

                     Right (Right errRespBody)
                       -> pure $ Right $ ErrorResponse openStatus errRespBody
  where
    waitForErrorResponse stream = do
      httpEv <- _waitEvent stream
      case httpEv of
        StreamDataEvent dataFrameHeader body
          -> do replenishFlowControls incomingConnectionFlowControl incomingStreamFlowControl (payloadLength dataFrameHeader)
                let streamClosedByServer = testEndStream $ flags dataFrameHeader
                when streamClosedByServer $ liftIO $ writeIORef streamClosed True

                case eitherDecode . L.fromStrict $ body of
                  Left err
                    -> pure $ Left $ "Failed to read error response from a subscription " <> (Text.pack $ show body) <> ". with error :" <> (Text.pack $ show err)

                  Right errRespBody
                    -> return $ Right $ errRespBody

        -- Throw error if headers are recieved. We should have recieved the
        -- initial headers and don't support trailers and so this indicates an
        -- invalid state.
        StreamHeadersEvent _ hdrs
          -> pure $ Left $ "Expected data on http2 stream in subscription but recieved headers: " <> (Text.pack $ show hdrs)

        -- Consume push promises and continue waiting
        StreamPushPromiseEvent _ ppStreamId ppHeaders
          -> do let resetPushPromises _ pps _ _ _ = _rst pps RefusedStream
                _handlePushPromise stream ppStreamId ppHeaders resetPushPromises
                waitForErrorResponse stream

        h2Event
          -> pure $ Left $ "Expected data on http2 stream on subscription, but received: " <> (Text.pack $ show h2Event)

-- | Close a Subscription. Nothing when already closed.
-- TODO: Actually close the stream. (By sending a rst?)
http2Close :: HTTP2Subscription event -> IO (Maybe ())
http2Close (HTTP2Subscription stream streamClosed incomingStreamFlow incomingConnectionFlow headers) = do
  -- Set the stream closed to signal that nobody else should read
  writeIORef streamClosed True

  -- Send a RST to the server to signal we're done with the stream
  eRes <- runClientIO $ _rst stream StreamClosed
  pure $ case eRes of
    -- TODO: This interface is bad. We're throwing away why this failed.
    Left clientError
      -> Nothing
    Right ()
      -> Just ()

