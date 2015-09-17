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
  -- * Types
  SMSAeroAuth(..),
  Signature(..),
  MessageId(..),
  Phone(..),
  SMSAeroDate(..),
  -- * Responses
  SmsAeroResponse(..),
  SendResponse(..),
  StatusResponse(..),
  BalanceResponse(..),
  SendersResponse(..),
  SignResponse(..),
) where

import Data.Aeson
import Data.Proxy

import Data.Time (UTCTime(UTCTime))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Applicative
import GHC.TypeLits (Symbol, KnownSymbol)
import Text.Read (readMaybe)

import Servant.API
import Servant.Client
import Servant.Docs
import Control.Lens (over, (|>))

import GHC.Generics

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

instance (HasClient sub, KnownSymbol sym, ToText a) => HasClient (RequiredQueryParam sym a :> sub) where
  type Client (RequiredQueryParam sym a :> sub) = a -> Client sub
  clientWithRoute _ req baseurl param = clientWithRoute (Proxy :: Proxy (QueryParam sym a :> sub)) req baseurl (Just param)

instance (KnownSymbol sym, ToParam (QueryParam sym a), HasDocs sub) => HasDocs (RequiredQueryParam sym a :> sub) where
  docsFor _ (endpoint, action) =
    docsFor subP (endpoint, action')

    where subP = Proxy :: Proxy sub
          paramP = Proxy :: Proxy (QueryParam sym a)
          action' = over params (|> toParam paramP) action

-- | SMSAero sender's signature. This is used for the "from" field.
newtype Signature = Signature { getSignature :: Text } deriving (Show, FromJSON, ToJSON, ToText, FromText)

-- | SMSAero sent message id.
newtype MessageId = MessageId Integer deriving (Show, FromJSON, ToJSON, ToText, FromText)

-- | SMSAero authentication data.
data SMSAeroAuth = SMSAeroAuth
  { authUser      :: Text   -- ^ Username.
  , authPassword  :: Text   -- ^ MD5 hash of a password.
  }

instance FromJSON SMSAeroAuth where
  parseJSON (Object o) = SMSAeroAuth
    <$> o .: "user"
    <*> o .: "password"
  parseJSON _ = empty

instance ToJSON SMSAeroAuth where
  toJSON SMSAeroAuth{..} = object
    [ "user"     .= authUser
    , "password" .= authPassword ]

-- | Phone number.
newtype Phone = Phone { getPhone :: Integer } deriving (Show, ToText, FromText)

-- | Date. Textually @SMSAeroDate@ is represented as a number of seconds since 01 Jan 1970.
newtype SMSAeroDate = SMSAeroDate { getSMSAeroDate :: UTCTime } deriving (Show)

instance ToText SMSAeroDate where
  toText (SMSAeroDate dt) = Text.pack (show (utcTimeToPOSIXSeconds dt))

instance FromText SMSAeroDate where
  fromText s = do
    n <- fromInteger <$> readMaybe (Text.unpack s)
    return (SMSAeroDate (posixSecondsToUTCTime n))

-- | SMSAero authentication credentials.
data RequireAuth

instance HasClient sub => HasClient (RequireAuth :> sub) where
  type Client (RequireAuth :> sub) = SMSAeroAuth -> Client sub

  clientWithRoute _ req baseurl SMSAeroAuth{..} =
    clientWithRoute
      (Proxy :: Proxy (RequiredQueryParam "user"     Text :>
                       RequiredQueryParam "password" Text :>
                       sub))
      req
      baseurl
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
    clientWithRoute _ req baseurl = clientWithRoute (Proxy :: Proxy (RequiredQueryParam "answer" Text :> sub)) req baseurl "json"

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
  RequiredQueryParam "text" Text        :>
  RequiredQueryParam "from" Signature   :>
  QueryParam "date" SMSAeroDate :>
  SmsAeroGet SendResponse

instance ToParam (QueryParam "to" Phone) where
  toParam _ = DocQueryParam "to"
                ["74951234567"]
                "Recipient phone number."
                Normal

instance ToParam (QueryParam "text" Text) where
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
type StatusApi = RequiredQueryParam "id" MessageId :> SmsAeroGet StatusResponse

instance ToParam (QueryParam "id" MessageId) where
  toParam _ = DocQueryParam "id"
                ["12345"]
                "Message ID, returned previously by SMSAero."
                Normal

-- | Every SMSAero response is either rejected or provides some info.
data SmsAeroResponse a
  = ResponseOK a        -- ^ Some useful payload.
  | ResponseReject Text -- ^ Rejection reason.
  deriving (Show, Generic)
-- | This is a generic instance and __does not match__ @FromJSON@.
instance ToJSON a => ToJSON (SmsAeroResponse a)

-- | SMSAero response to a send request.
data SendResponse
  = SendAccepted MessageId  -- ^ Message accepted.
  | SendNoCredits           -- ^ No credits to send a message.
  deriving (Show, Generic)
-- | This is a generic instance and __does not match__ @FromJSON@.
instance ToJSON SendResponse

instance ToSample (SmsAeroResponse SendResponse) (SmsAeroResponse SendResponse) where
  toSample _ = Nothing

-- | SMSAero response to a status request.
data StatusResponse
  = StatusDeliverySuccess   -- ^ Message is successfully delivered.
  | StatusDeliveryFailure   -- ^ Message delivery has failed.
  | StatusSmscSubmit        -- ^ Message submitted to SMSC.
  | StatusSmscReject        -- ^ Message rejected by SMSC.
  | StatusQueue             -- ^ Message queued.
  | StatusWaitStatus        -- ^ Wait for message status.
  deriving (Show, Generic)
-- | This is a generic instance and __does not match__ @FromJSON@.
instance ToJSON StatusResponse

instance ToSample (SmsAeroResponse StatusResponse) (SmsAeroResponse StatusResponse) where
  toSample _ = Nothing

-- | SMSAero response to a balance request.
-- This is a number of available messages to send.
newtype BalanceResponse = BalanceResponse Double deriving (Show, ToJSON)

instance ToSample (SmsAeroResponse BalanceResponse) (SmsAeroResponse BalanceResponse) where
  toSample _ = Nothing

-- | SMSAero response to a senders request.
-- This is just a list of available signatures.
newtype SendersResponse = SendersResponse [Signature] deriving (Show, FromJSON, ToJSON)

instance ToSample (SmsAeroResponse SendersResponse) (SmsAeroResponse SendersResponse) where
  toSample _ = Nothing

-- | SMSAero response to a sign request.
data SignResponse
  = SignApproved  -- ^ Signature is approved.
  | SignRejected  -- ^ Signature is rejected.
  | SignPending   -- ^ Signature is pending.
  deriving (Show, Generic)
-- | This is a generic instance and __does not match__ @FromJSON@.
instance ToJSON SignResponse

instance ToSample (SmsAeroResponse SignResponse) (SmsAeroResponse SignResponse) where
  toSample _ = Nothing

instance FromJSON a => FromJSON (SmsAeroResponse a) where
  parseJSON (Object o) = do
    result :: Maybe Text <- o .:? "result"
    case result of
      Just "reject" -> ResponseReject <$> o .: "reason"
      _ -> ResponseOK <$> parseJSON (Object o)
  parseJSON j = ResponseOK <$> parseJSON j

instance FromJSON SendResponse where
  parseJSON (Object o) = do
    result :: Text <- o .: "result"
    case result of
      "accepted"    -> SendAccepted <$> o .: "id"
      "no credits"  -> pure SendNoCredits
      _ -> empty
  parseJSON _ = empty

instance FromJSON StatusResponse where
  parseJSON (Object o) = do
    result :: Text <- o .: "result"
    case result of
      "delivery success"  -> pure StatusDeliverySuccess
      "delivery failure"  -> pure StatusDeliveryFailure
      "smsc submit"       -> pure StatusSmscSubmit
      "smsc reject"       -> pure StatusSmscReject
      "queue"             -> pure StatusQueue
      "wait status"       -> pure StatusWaitStatus
      _ -> empty
  parseJSON _ = empty

instance FromJSON BalanceResponse where
  parseJSON (Object o) = do
    balance <- o .: "balance"
    case readMaybe balance of
      Just x  -> pure (BalanceResponse x)
      Nothing -> empty
  parseJSON _ = empty

instance FromJSON SignResponse where
  parseJSON (Object o) = do
    accepted :: Text <- o .: "accepted"
    case accepted of
      "approved" -> pure SignApproved
      "rejected" -> pure SignRejected
      "pending"  -> pure SignPending
      _ -> empty
  parseJSON _ = empty
