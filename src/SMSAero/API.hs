{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : SMSAero.API
-- Copyright   : (c) 2015, GetShopTV
-- License     : BSD3
-- Maintainer  : nickolay@getshoptv.com
-- Stability   : experimental
--
-- This module describes SMSAero API and defines corresponding types.
module SMSAero.API (
  -- * API
  SMSAeroAPI,
  SendApi,
  StatusApi,
  -- * Combinators
  SmsAeroJson,
  AnswerJson,
  RequireAuth,
  RequiredQueryParam,
  SmsAeroGet,
  -- * Responses
  SmsAeroResponse(..),
  SendResponse(..),
  MessageStatus(..),
  BalanceResponse(..),
  SendersResponse(..),
  SignResponse(..),
) where

import Data.Aeson
import Data.Monoid ((<>))
import Data.Proxy

import Data.Time (UTCTime(UTCTime))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Applicative
import GHC.TypeLits (Symbol, KnownSymbol)

import Servant.API
import Servant.Client
import Servant.Docs
import Control.Lens (over, (|>))

import GHC.Generics

import SMSAero.Types

-- | Content type for SMSAero JSON answer (it has JSON body but "text/plain" Content-Type).
data SmsAeroJson

instance Accept SmsAeroJson where
  contentType _ = contentType (Proxy :: Proxy PlainText)

instance FromJSON a => MimeUnrender SmsAeroJson a where
  mimeUnrender _ = mimeUnrender (Proxy :: Proxy JSON)

instance ToJSON a => MimeRender SmsAeroJson a where
  mimeRender _ = mimeRender (Proxy :: Proxy JSON)

-- | Like 'QueryParam', but always required.
data RequiredQueryParam (sym :: Symbol) a

instance (HasClient sub, KnownSymbol sym, ToHttpApiData a) => HasClient (RequiredQueryParam sym a :> sub) where
  type Client (RequiredQueryParam sym a :> sub) = a -> Client sub
  clientWithRoute _ req param = clientWithRoute (Proxy :: Proxy (QueryParam sym a :> sub)) req (Just param)

instance (KnownSymbol sym, ToParam (QueryParam sym a), HasDocs sub) => HasDocs (RequiredQueryParam sym a :> sub) where
  docsFor _ (endpoint, action) =
    docsFor subP (endpoint, action')

    where subP = Proxy :: Proxy sub
          paramP = Proxy :: Proxy (QueryParam sym a)
          action' = over params (|> toParam paramP) action

-- | SMSAero authentication credentials.
data RequireAuth

instance HasClient sub => HasClient (RequireAuth :> sub) where
  type Client (RequireAuth :> sub) = SMSAeroAuth -> Client sub

  clientWithRoute _ req SMSAeroAuth{..} =
    clientWithRoute
      (Proxy :: Proxy (RequiredQueryParam "user"     Text :>
                       RequiredQueryParam "password" Text :>
                       sub))
      req
      authUser
      authPassword

instance HasDocs sub => HasDocs (RequireAuth :> sub) where
  docsFor _ (endpoint, action) =
    docsFor subP (endpoint, action')

    where subP = Proxy :: Proxy sub
          userP = DocQueryParam "user"
                    ["alice@example.com", "bob@example.com"]
                    "SMSAero username (email) for authentication."
                    Normal
          passP = DocQueryParam "password"
                    ["5f4dcc3b5aa765d61d8327deb882cf99", "d8578edf8458ce06fbc5bb76a58c5ca4"]
                    "MD5 hash of a password."
                    Normal
          action' = over params ((|> passP) . (|> userP)) action

-- | Implicit parameter that tells SMSAero to respond with JSON.
data AnswerJson

instance HasClient sub => HasClient (AnswerJson :> sub) where
    type Client (AnswerJson :> sub) = Client sub
    clientWithRoute _ req = clientWithRoute (Proxy :: Proxy (RequiredQueryParam "answer" Text :> sub)) req "json"

instance HasDocs sub => HasDocs (AnswerJson :> sub) where
  docsFor _ (endpoint, action) = docsFor subP (endpoint, action')
    where
      subP = Proxy :: Proxy sub
      answerP = DocQueryParam "answer"
                  ["json"]
                  "When present makes SMSAero REST API to respond with JSON."
                  Normal
      action' = over params (|> answerP) action

-- | Regular SMSAero GET API.
type SmsAeroGet a = Get '[SmsAeroJson] (SmsAeroResponse a)

-- | SMSAero API.
type SMSAeroAPI = RequireAuth :> AnswerJson :>
      ("send"     :> SendApi
  :<|> "status"   :> StatusApi
  :<|> "balance"  :> SmsAeroGet BalanceResponse
  :<|> "senders"  :> SmsAeroGet SendersResponse
  :<|> "sign"     :> SmsAeroGet SignResponse)

-- | SMSAero API to send a message.
type SendApi =
  RequiredQueryParam "to"   Phone       :>
  RequiredQueryParam "text" MessageBody :>
  RequiredQueryParam "from" Signature   :>
  QueryParam "date" SMSAeroDate :>
  QueryParam "type" SendType :>
  SmsAeroGet SendResponse

instance ToParam (QueryParam "to" Phone) where
  toParam _ = DocQueryParam "to"
                ["74951234567"]
                "Recipient phone number."
                Normal

instance ToParam (QueryParam "text" MessageBody) where
  toParam _ = DocQueryParam "text"
                ["Hello, world!"]
                "Message content."
                Normal

instance ToParam (QueryParam "from" Signature) where
  toParam _ = DocQueryParam "from"
                ["My Company"]
                "Sender's signature."
                Normal

instance ToParam (QueryParam "date" SMSAeroDate) where
  toParam _ = DocQueryParam "date"
                [show (utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2015 01 31) 0))]
                "Requested datetime of delivery as number of seconds since 01 Jan 1970."
                Normal

-- | SMSAero API to check message status.
type StatusApi = RequiredQueryParam "id" MessageId :> SmsAeroGet MessageStatus

instance ToParam (QueryParam "id" MessageId) where
  toParam _ = DocQueryParam "id"
                ["12345"]
                "Message ID, returned previously by SMSAero."
                Normal

-- | Every SMSAero response is either rejected or provides some info.
data SmsAeroResponse a
  = ResponseOK a        -- ^ Some useful payload.
  | ResponseReject Text -- ^ Rejection reason.
  deriving (Eq, Show, Generic)

-- | SMSAero response to a send request.
data SendResponse
  = SendAccepted MessageId  -- ^ Message accepted.
  | SendNoCredits           -- ^ No credits to send a message.
  deriving (Eq, Show, Generic)

instance ToSample (SmsAeroResponse SendResponse) where
  toSamples _ =
    [ ("When message is sent successfully.", ResponseOK (SendAccepted (MessageId 12345)))
    , ("When SMSAero account does not have enough credit.", ResponseOK SendNoCredits)
    , ("When message sender is incorrect.", ResponseReject "incorrect sender name") ]

-- | SMSAero message status.
data MessageStatus
  = StatusDeliverySuccess   -- ^ Message is successfully delivered.
  | StatusDeliveryFailure   -- ^ Message delivery has failed.
  | StatusSmscSubmit        -- ^ Message submitted to SMSC.
  | StatusSmscReject        -- ^ Message rejected by SMSC.
  | StatusQueue             -- ^ Message queued.
  | StatusWaitStatus        -- ^ Wait for message status.
  deriving (Eq, Enum, Bounded, Show, Read, Generic)

instance ToSample (SmsAeroResponse MessageStatus) where
  toSamples _ =
    [ ("When message has been delivered successfully.", ResponseOK StatusDeliverySuccess)
    , ("When message has been queued.", ResponseOK StatusQueue) ]

-- | SMSAero response to a balance request.
-- This is a number of available messages to send.
newtype BalanceResponse = BalanceResponse Double deriving (Eq, Show)

instance ToSample (SmsAeroResponse BalanceResponse) where
  toSamples _ =
    [ ("Just balance.", ResponseOK (BalanceResponse 247))
    , ("When auth credentials are incorrect.", ResponseReject "incorrect user or password") ]

-- | SMSAero response to a senders request.
-- This is just a list of available signatures.
newtype SendersResponse = SendersResponse [Signature] deriving (Eq, Show, FromJSON, ToJSON)

instance ToSample (SmsAeroResponse SendersResponse) where
  toSamples _ = singleSample (ResponseOK (SendersResponse [Signature "TEST", Signature "My Company"]))

-- | SMSAero response to a sign request.
data SignResponse
  = SignApproved  -- ^ Signature is approved.
  | SignRejected  -- ^ Signature is rejected.
  | SignPending   -- ^ Signature is pending.
  deriving (Eq, Enum, Bounded, Show, Generic)

instance ToSample (SmsAeroResponse SignResponse) where
  toSamples _ =
    [ ("When a new signature is approved.", ResponseOK SignApproved)
    , ("When a new signature is rejected.", ResponseOK SignRejected) ]

instance FromJSON a => FromJSON (SmsAeroResponse a) where
  parseJSON (Object o) = do
    result :: Maybe Text <- o .:? "result"
    case result of
      Just "reject" -> ResponseReject <$> o .: "reason"
      _ -> ResponseOK <$> parseJSON (Object o)
  parseJSON j = ResponseOK <$> parseJSON j

instance ToJSON a => ToJSON (SmsAeroResponse a) where
  toJSON (ResponseOK x) = toJSON x
  toJSON (ResponseReject reason) = object
    [ "result" .= ("reject" :: Text)
    , "reason" .= reason ]

instance FromJSON SendResponse where
  parseJSON (Object o) = do
    result :: Text <- o .: "result"
    case result of
      "accepted"    -> SendAccepted <$> o .: "id"
      "no credits"  -> pure SendNoCredits
      _ -> empty
  parseJSON _ = empty

instance ToJSON SendResponse where
  toJSON (SendAccepted n) = object
    [ "result" .= ("accepted" :: Text)
    , "id"     .= toJSON n ]
  toJSON SendNoCredits = object
    [ "result" .= ("no credits" :: Text)]

-- | Helper to define @parseUrlPiece@ matching @toUrlPiece@.
boundedParseUrlPiece :: (Enum a, Bounded a, ToHttpApiData a) => Text -> Either Text a
boundedParseUrlPiece x = lookupEither ("could not parse: " <> x) x xs
  where
    vals = [minBound..maxBound]
    xs = zip (map toUrlPiece vals) vals
    lookupEither e y ys = maybe (Left e) Right (lookup y ys)

instance FromHttpApiData MessageStatus where
  parseUrlPiece = boundedParseUrlPiece

instance ToHttpApiData MessageStatus where
  toUrlPiece StatusDeliverySuccess  = "delivery success"
  toUrlPiece StatusDeliveryFailure  = "delivery failure"
  toUrlPiece StatusSmscSubmit       = "smsc submit"
  toUrlPiece StatusSmscReject       = "smsc reject"
  toUrlPiece StatusQueue            = "queue"
  toUrlPiece StatusWaitStatus       = "wait status"

instance FromJSON MessageStatus where
  parseJSON (Object o) = do
    result :: Text <- o .: "result"
    case (parseUrlPiece result :: Either Text MessageStatus) of
      Left _ -> empty
      Right status -> return status
  parseJSON _ = empty

instance ToJSON MessageStatus where
  toJSON status = object [ "result" .= toUrlPiece status ]

instance FromJSON BalanceResponse where
  parseJSON (Object o) = BalanceResponse <$> o .: "balance"
  parseJSON _ = empty

instance ToJSON BalanceResponse where
  toJSON (BalanceResponse n) = object [ "balance" .= n ]

instance ToHttpApiData SignResponse where
  toUrlPiece SignApproved = "approved"
  toUrlPiece SignRejected = "rejected"
  toUrlPiece SignPending  = "pending"

instance FromHttpApiData SignResponse where
  parseUrlPiece = boundedParseUrlPiece

instance FromJSON SignResponse where
  parseJSON (Object o) = do
    accepted :: Text <- o .: "accepted"
    case (parseUrlPiece accepted :: Either Text SignResponse) of
      Left _ -> empty
      Right resp -> return resp
  parseJSON _ = empty

instance ToJSON SignResponse where
  toJSON s = object [ "accepted" .= toUrlPiece s ]
