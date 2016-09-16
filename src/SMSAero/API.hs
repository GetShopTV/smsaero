{-# OPTIONS -fno-warn-orphans #-}
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
  SendToGroupApi,
  StatusApi,
  GroupApi,
  PhoneApi,
  BlacklistApi,
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
  CheckSendingResponse,
  BalanceResponse(..),
  CheckTariffResponse,
  SendersResponse(..),
  SignResponse(..),
  GroupResponse(..),
  PhoneResponse(..),
  BlacklistResponse(..),
) where

import Data.Aeson
import Data.Proxy

import Data.Time (UTCTime(UTCTime))
import Data.Time.Calendar (fromGregorian)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Read (readEither)

import Control.Applicative
import GHC.TypeLits (Symbol, KnownSymbol)

import Servant.API
import Servant.Client
import Servant.Docs
import Servant.Docs.Internal (_params)

import Web.HttpApiData

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

instance (ToParam (QueryParam sym a), HasDocs sub) => HasDocs (RequiredQueryParam sym a :> sub) where
  docsFor _ (endpoint, action) =
    docsFor subP (endpoint, action')

    where subP = Proxy :: Proxy sub
          paramP = Proxy :: Proxy (QueryParam sym a)
          action' = action { _params = params' }
          params' = _params action ++ [toParam paramP]

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
          action' = action { _params = params' }
          params' = _params action ++ [userP, passP]

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
      action' = action { _params = params' }
      params' = _params action ++ [answerP]

-- | Regular SMSAero GET API.
type SmsAeroGet a = Get '[SmsAeroJson] (SmsAeroResponse a)

-- | SMSAero API.
type SMSAeroAPI = RequireAuth :> AnswerJson :>
      ("send"         :> SendApi
  :<|> "sendtogroup"  :> SendToGroupApi
  :<|> "status"       :> StatusApi
  :<|> "checksending" :> CheckSendingApi
  :<|> "balance"      :> SmsAeroGet BalanceResponse
  :<|> "checktarif"   :> SmsAeroGet CheckTariffResponse
  :<|> "senders"      :> SmsAeroGet SendersResponse
  :<|> "sign"         :> SmsAeroGet SignResponse
  :<|> GroupApi
  :<|> PhoneApi
  :<|> "addblacklist" :> BlacklistApi)

-- | SMSAero API to send a message.
type SendApi =
  RequiredQueryParam "to"   Phone       :>
  RequiredQueryParam "text" MessageBody :>
  RequiredQueryParam "from" Signature   :>
  QueryParam "date" SMSAeroDate         :>
  QueryParam "type" SendType            :>
  QueryParam "digital" DigitalChannel   :>
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
                [Text.unpack (toQueryParam (SMSAeroDate (UTCTime (fromGregorian 2015 01 31) 0)))]
                "Requested datetime of delivery as number of seconds since 01 Jan 1970."
                Normal

instance ToParam (QueryParam "type" SendType) where
  toParam _ = DocQueryParam "type"
              (map (Text.unpack . toQueryParam) [minBound..maxBound::SendType])
              "Send type to describe send channel, equals to '2' (free literal signature for all operators except MTS) by default. Can't be used with 'digital' parameter."
              Normal

instance ToParam (QueryParam "digital" DigitalChannel) where
  toParam _ = DocQueryParam "digital"
              [Text.unpack (toQueryParam DigitalChannel)]
              "Send type for digital send channel. Can't be used with 'type' parameter."
              Normal

-- | SMSAero API to send a group message.
type SendToGroupApi =
  RequiredQueryParam "group" Group      :>
  RequiredQueryParam "text" MessageBody :>
  RequiredQueryParam "from" Signature   :>
  QueryParam "date" SMSAeroDate         :>
  QueryParam "type" SendType            :>
  QueryParam "digital" DigitalChannel   :>
  SmsAeroGet SendResponse

instance ToParam (QueryParam "group" Group) where
  toParam _ = DocQueryParam "group"
                ["all", "groupname"]
                "Group name to broadcast a message."
                Normal

-- | SMSAero API to check message status.
type StatusApi = RequiredQueryParam "id" MessageId :> SmsAeroGet MessageStatus

instance ToParam (QueryParam "id" MessageId) where
  toParam _ = DocQueryParam "id"
                ["12345"]
                "Message ID, returned previously by SMSAero."
                Normal

-- | SMSAero API to check broadcast status.
type CheckSendingApi = RequiredQueryParam "id" MessageId :> SmsAeroGet CheckSendingResponse

-- | SMSAero API to add/delete groups.
type GroupApi =
       "checkgroup" :> SmsAeroGet [Group]
  :<|> "addgroup"   :> RequiredQueryParam "group" Group :> SmsAeroGet GroupResponse
  :<|> "delgroup"   :> RequiredQueryParam "group" Group :> SmsAeroGet GroupResponse

-- | SMSAero API to add/delete subscribers.
type PhoneApi =
       "addphone"                       :>
       RequiredQueryParam "phone" Phone :>
       QueryParam "group" Group         :>
       QueryParam "lname" Name          :>
       QueryParam "fname" Name          :>
       QueryParam "sname" Name          :>
       QueryParam "bday"  BirthDate     :>
       QueryParam "param" Text          :>
       SmsAeroGet PhoneResponse
  :<|> "delphone" :> RequiredQueryParam "phone" Phone :> QueryParam "group" Group :> SmsAeroGet PhoneResponse

-- | SMSAero API to add phone to blacklist.
type BlacklistApi = RequiredQueryParam "phone" Phone :> SmsAeroGet BlacklistResponse

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

-- | SMSAero response to a balance request (balance in rubles).
newtype BalanceResponse = BalanceResponse Double deriving (Eq, Show)

instance ToSample (SmsAeroResponse BalanceResponse) where
  toSamples _ =
    [ ("Just balance.", ResponseOK (BalanceResponse 247))
    , ("When auth credentials are incorrect.", ResponseReject "incorrect user or password") ]

-- | SMSAero response to a checktarif request.
type CheckTariffResponse = Map ChannelName Double

-- | SMSAero response to a checksending request.
type CheckSendingResponse = Map MessageId MessageStatus

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

-- | SMSAero response to an addgroup/delgroup request.
newtype GroupResponse = GroupResponse Text deriving (Eq, Show, FromJSON, ToJSON)

-- | SMSAero response to an addphone/delphone request.
newtype PhoneResponse = PhoneResponse Text deriving (Eq, Show, FromJSON, ToJSON)

-- | SMSAero response to an addblacklist request.
newtype BlacklistResponse = BlacklistResponse Text deriving (Eq, Show, FromJSON, ToJSON)

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

instance FromHttpApiData MessageStatus where
  parseQueryParam = parseBoundedQueryParam

instance ToHttpApiData MessageStatus where
  toQueryParam StatusDeliverySuccess  = "delivery success"
  toQueryParam StatusDeliveryFailure  = "delivery failure"
  toQueryParam StatusSmscSubmit       = "smsc submit"
  toQueryParam StatusSmscReject       = "smsc reject"
  toQueryParam StatusQueue            = "queue"
  toQueryParam StatusWaitStatus       = "wait status"

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
  parseJSON (Object o) = do
    str <- o .: "balance"
    case readEither str of
      Left err -> fail err
      Right b  -> return (BalanceResponse b)
  parseJSON _ = empty

instance ToJSON BalanceResponse where
  toJSON (BalanceResponse n) = object [ "balance" .= show n ]

instance ToHttpApiData SignResponse where
  toQueryParam SignApproved = "approved"
  toQueryParam SignRejected = "rejected"
  toQueryParam SignPending  = "pending"

instance FromHttpApiData SignResponse where
  parseQueryParam = parseBoundedQueryParam

instance FromJSON SignResponse where
  parseJSON (Object o) = do
    accepted :: Text <- o .: "accepted"
    case (parseUrlPiece accepted :: Either Text SignResponse) of
      Left _ -> empty
      Right resp -> return resp
  parseJSON _ = empty

instance ToJSON SignResponse where
  toJSON s = object [ "accepted" .= toUrlPiece s ]

instance ToJSON CheckSendingResponse where
  toJSON = toJSON . Map.mapKeys toQueryParam . fmap toQueryParam

