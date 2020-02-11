{-# LANGUAGE
    DeriveFunctor
  , DeriveAnyClass
  , DuplicateRecordFields
  , EmptyDataDecls
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  , RankNTypes
  #-}
{-|
Module      : Pusher
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Defines a 'Pusher' type for chaining computations that make requests/ establish
subscriptions to Pusher platform services.
-}

module Pusher
  (
  -- * Configuration
  -- | Create a read-only environment with 'mkPusherEnv'.
    PusherEnv(..)
  , mkPusherEnv

  -- * Requests and Subscriptions to Pusher may be made within this type

  -- * Core type
  -- | 'Pusher' can be thought of as a reader of 'PusherEnv'
  -- state that may perform IO and short-circuit failing operations.
  --
  -- Use 'pusherRequest' to issue request-responses.
  -- Use 'pusherSubscribe' to establish subscriptions and 'pusherReadEvent' to
  -- read new events from subscriptions.
  , Pusher()
  , PusherResult(..)
  , runPusher
  , pusherRequest
  , pusherSubscribe
  , pusherClose
  , pusherReadEvent
  , pusherOpenStatus
  , pusherReadRawEvent

  , pusherIO
  , pusherLift
  , pusherFail
  , pusherErrorResponse
  , pusherTry
  , getEnv

  -- * Re-export
  , module Pusher.Client.Request
  , module Pusher.Client.Subscribe
  , module Pusher.Model
  )
  where

import Pusher.Client
import Pusher.Client.HTTP2
import Pusher.Client.Result
import Pusher.Client.Request
import Pusher.Client.Error
import Pusher.Client.Subscribe
import Pusher.Model

import System.Timeout
import Data.Text

import Network.HPACK

-- | Environment required to make requests to Pusher.
-- HTTP2 Clients are retained such that their connections can be reused.
data PusherEnv = PusherEnv
  { _instanceID     :: Instance -- ^ Instance ID to which all requests are made
  , _keyID          :: KeyID    -- ^ The identifier for the instances key
  , _clusterClient  :: Client   -- ^ A client configured to access a Pusher cluster
  }

-- | Create an Environment for making requests to Pusher
mkPusherEnv
  :: Instance
  -> KeyID
  -> ClusterName
  -> Host
  -> HeaderList
  -> IO (Maybe PusherEnv)
mkPusherEnv inst keyID clusterName host commonHeaders = do
  mClient <- mkHTTP2Client (HTTP2ClientOptions clusterName host commonHeaders)
  case mClient of
    Nothing
      -> return Nothing

    Just client
      -> return . Just $ PusherEnv
           { _instanceID     = inst
           , _keyID          = keyID
           , _clusterClient  = mkClient client
           }

-- | Pusher threads a 'PusherEnv' into its computations. Failures short-circuit and cause early termination.
newtype Pusher a = Pusher {_runPusher :: PusherEnv -> IO (PusherEnv,PusherResult a)}
  deriving Functor

-- | Execute a Pusher function, returning it's possible result.
runPusher :: PusherEnv -> Pusher a -> IO (PusherResult a)
runPusher env c = do
  (_env', ma) <- _runPusher c env
  return ma

-- | Lift an IO action into the Pusher context.
pusherIO :: IO a -> Pusher a
pusherIO f = Pusher $ \env -> do
  a <- f
  return (env, PusherSuccess a)

-- Lift an IO PusherResult into the Pusher context.
pusherLift :: IO (PusherResult a) -> Pusher a
pusherLift f = Pusher $ \env -> do
  result <- f
  pure (env, result)

-- | A Pusher function that fails.
pusherFail :: Text -> Pusher a
pusherFail msg = Pusher $ \env -> return (env, PusherFailure msg)

-- | Lift an ErrorResponse into the Pusher context.
pusherErrorResponse :: ErrorResponse -> Pusher a
pusherErrorResponse errResp = Pusher $ \env -> return (env, PusherErrorResponse errResp)

-- | Lift the success/ failure of a Pusher computation into the return type.
-- This is useful if you have a computation you can recover from and don't want
-- the default short-circuiting behavior of the Monad/ Applicative instances.
pusherTry :: Pusher a -> Pusher (PusherResult a)
pusherTry (Pusher f) = Pusher $ \env -> do
  (env',res) <- f env
  pure (env', PusherSuccess res)

-- | Extract the current environment.
getEnv :: Pusher PusherEnv
getEnv = Pusher $ \env -> return (env,PusherSuccess env)

-- | Issue a request to Pusher, returning the corresponding response type.
-- Most requests require an AccessToken for user authentication.
pusherRequest
  :: forall req resp
   . IsRequest req resp
  => Maybe AccessToken
  -> req
  -> Pusher resp
pusherRequest mAccessToken req = do
  env <- getEnv
  let client = (_clusterClient :: PusherEnv -> Client) env
  let inst   = _instanceID env
  pusherLift $ request client inst mAccessToken req

-- | Issue a request for a subscription to Pusher
--
-- Subscriptions can be consumed with 'pusherReadEvent' to return @event@ values.
pusherSubscribe
  :: forall req event
   . IsSubscribe req event
  => Maybe AccessToken
  -> req
  -> Pusher (Subscription event)
pusherSubscribe mAccessToken req = do
  env <- getEnv
  let client = (_clusterClient :: PusherEnv -> Client) env
  let inst   = _instanceID env
  pusherLift $ subscribe client inst mAccessToken req

-- | Read a opaque Event from a Subscription. I.E. the event value has not been
-- parsed or validated.
--
-- To read a parsed event, use 'pusherReadEvent'.
pusherReadRawEvent
  :: Subscription event
  -> Pusher RawEvent
pusherReadRawEvent subscription = Pusher $ \env -> do
  res <- readRawEvent subscription
  pure (env, res)

-- | Read an event from a Subscription. The Event is parsed to the expected type.
pusherReadEvent
  :: IsSubscribe req event
  => Maybe Int
  -> Subscription event
  -> Pusher (Either EventEndOfSubscription event)
pusherReadEvent maxTime subscription = Pusher $ \env -> case maxTime of
  Nothing
     -> do eEv <- readEvent subscription
           case eEv of
             PusherFailure errMsg
               -> pure (env, PusherFailure errMsg)

             PusherErrorResponse errResp
               -> pure (env, PusherErrorResponse errResp)

             PusherSuccess ev
               -> pure (env, PusherSuccess ev)

  Just t
    -> do eTimedout <- timeout t $ readEvent subscription
          case eTimedout of
            -- Time-out
            Nothing
              -> pure (env, PusherFailure "Timeout waiting for event")

            Just (PusherFailure errMsg)
              -> pure (env, PusherFailure errMsg)

            Just (PusherErrorResponse errResp)
              -> pure (env, PusherErrorResponse errResp)

            Just (PusherSuccess res)
              -> pure (env, PusherSuccess res)

-- | Calls the underlying 'close' on a subscription. Fails on errors such as
-- double-close.
pusherClose :: Subscription event -> Pusher ()
pusherClose subscription = do
  mClosed <- pusherIO $ close subscription
  case mClosed of
    Nothing
      -> pusherFail "Subscription already closed"
    Just ()
      -> pure ()

-- | Calls the underlying 'openStatus' on a susbcription. Fails if there is no
-- status NOT if the status is negative.
pusherOpenStatus :: Subscription event -> Pusher Int
pusherOpenStatus subscription = case openStatus subscription of
  Nothing
    -> pusherFail "Subscription failed to open/ did not have a status code"
  Just status
    -> pure status

instance Applicative Pusher where
  -- return a successful value without modifying the environment.
  pure a = Pusher $ \env -> return (env, PusherSuccess a)

  -- Short-circuit at the first failure, returning the latest available
  -- environment.
  (Pusher f) <*> (Pusher a) = Pusher $ \env -> do
    (env', mf) <- f env
    case mf of
      PusherFailure errMsg
        -> return (env', PusherFailure errMsg)

      PusherErrorResponse errResp
        -> return (env', PusherErrorResponse errResp)

      PusherSuccess f
        -> do (env'', ma) <- a env'
              case ma of
                PusherFailure errMsg
                  -> return (env'', PusherFailure errMsg)

                PusherErrorResponse errResp
                  -> return (env'', PusherErrorResponse errResp)

                PusherSuccess a
                  -> return (env'', PusherSuccess $ f a)

-- Thread a PusherEnv as state. Short-circuit at the first Nothing return value.
instance Monad Pusher where
  return = pure

  c >>= f = Pusher $ \env -> do
    (env',mA) <- _runPusher c env
    case mA of
      PusherFailure errMsg
        -> return (env', PusherFailure errMsg)

      PusherErrorResponse errResp
        -> return (env', PusherErrorResponse errResp)

      PusherSuccess a
        -> _runPusher (f a) env'

