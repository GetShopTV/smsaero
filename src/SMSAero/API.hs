{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SMSAero.API where

import Data.Aeson
import Data.Proxy

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Applicative
import GHC.TypeLits (Symbol, KnownSymbol)
import Text.Read (readMaybe)

import Servant.API
import Servant.Client

import GHC.Generics

-- | Content type for SMSAero JSON answer (it has JSON body but "text/plain" Content-Type).
data SmsAeroJson

instance Accept SmsAeroJson where
  contentType _ = contentType (Proxy :: Proxy PlainText)

instance FromJSON a => MimeUnrender SmsAeroJson a where
  mimeUnrender _ = mimeUnrender (Proxy :: Proxy JSON)

-- | Like 'QueryParam', but always required.
data RequiredQueryParam (sym :: Symbol) a

instance (HasClient sub, KnownSymbol sym, ToText a) => HasClient (RequiredQueryParam sym a :> sub) where
  type Client (RequiredQueryParam sym a :> sub) = a -> Client sub
  clientWithRoute _ req baseurl param = clientWithRoute (Proxy :: Proxy (QueryParam sym a :> sub)) req baseurl (Just param)

-- | SMSAero sender's signature. This is used for the "from" field.
newtype Signature = Signature { getSignature :: Text } deriving (Show, FromJSON, ToJSON, ToText)

-- | SMSAero sent message id.
newtype MessageId = MessageId Integer deriving (Show, FromJSON, ToJSON, ToText)

-- | SMSAero authentication data.
data SMSAeroAuth = SMSAeroAuth
  { authUser      :: Text   -- ^ Username.
  , authPassword  :: Text   -- ^ MD5 hash of a password.
  }

-- | Phone number.
newtype Phone = Phone { getPhone :: Integer } deriving (Show, ToText, FromText)

-- | Date.
newtype SMSAeroDate = SMSAeroDate { getSMSAeroDate :: UTCTime } deriving (Show)

instance ToText SMSAeroDate where
  toText (SMSAeroDate dt) = Text.pack (show (utcTimeToPOSIXSeconds dt))

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

-- | Implicit parameter that tells SMSAero to respond with JSON.
data AnswerJson

instance HasClient sub => HasClient (AnswerJson :> sub) where
    type Client (AnswerJson :> sub) = Client sub
    clientWithRoute _ req baseurl = clientWithRoute (Proxy :: Proxy (RequiredQueryParam "answer" Text :> sub)) req baseurl "json"

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

-- | SMSAero API to check message status.
type StatusApi = RequiredQueryParam "id" MessageId :> SmsAeroGet StatusResponse

-- | Every SMSAero response is either rejected or provides some info.
data SmsAeroResponse a
  = ResponseOK a        -- ^ Some useful payload.
  | ResponseReject Text -- ^ Rejection reason.
  deriving (Show, Generic)
instance ToJSON a => ToJSON (SmsAeroResponse a)

-- | SMSAero response to a send request.
data SendResponse
  = SendAccepted MessageId  -- ^ Message accepted.
  | SendNoCredits           -- ^ No credits to send a message.
  deriving (Show, Generic)
instance ToJSON SendResponse

-- | SMSAero response to a status request.
data StatusResponse
  = StatusDeliverySuccess   -- ^ Message is successfully delivered.
  | StatusDeliveryFailure   -- ^ Message delivery has failed.
  | StatusSmscSubmit        -- ^ Message submitted to SMSC.
  | StatusSmscReject        -- ^ Message rejected by SMSC.
  | StatusQueue             -- ^ Message queued.
  | StatusWaitStatus        -- ^ Wait for message status.
  deriving (Show, Generic)
instance ToJSON StatusResponse

-- | SMSAero response to a balance request.
-- This is a number of available messages to send.
newtype BalanceResponse = BalanceResponse Double deriving (Show, ToJSON)

-- | SMSAero response to a senders request.
-- This is just a list of available signatures.
newtype SendersResponse = SendersResponse [Signature] deriving (Show, FromJSON, ToJSON)

-- | SMSAero response to a sign request.
data SignResponse
  = SignApproved  -- ^ Signature is approved.
  | SignRejected  -- ^ Signature is rejected.
  | SignPending   -- ^ Signature is pending.
  deriving (Show, Generic)
instance ToJSON SignResponse

instance FromJSON a => FromJSON (SmsAeroResponse a) where
  parseJSON (Object o) = do
    result :: Maybe Text <- o .:? "result"
    case result of
      Just "reject" -> ResponseReject <$> o .: "reason"
      _ -> ResponseOK <$> parseJSON (Object o)
  parseJSON json = ResponseOK <$> parseJSON json

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
