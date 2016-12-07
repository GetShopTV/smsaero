{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : SMSAero.Types
-- Copyright   : (c) 2016, GetShopTV
-- License     : BSD3
-- Maintainer  : nickolay@getshoptv.com
-- Stability   : experimental
--
-- This module defines types used in SMSAero API.
module SMSAero.Types (
  SMSAeroAuth(..),
  Signature(..),
  MessageId(..),
  MessageBody(..),
  Group(..),
  Phone(..),
  SMSAeroDate(..),
  SendType(..),
  DigitalChannel(..),
  Name(..),
  BirthDate(..),
  ChannelName,
) where

import Control.Applicative (empty)

import Data.Aeson
import Data.Int (Int64)
import Data.Monoid

import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

import Data.Text (Text)
import qualified Data.Text as Text

import Web.HttpApiData

-- | SMSAero sender's signature. This is used for the "from" field.
newtype Signature = Signature { getSignature :: Text } deriving (Eq, Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData)

-- | SMSAero sent message id.
newtype MessageId = MessageId Int64
  deriving (Eq, Show, Ord, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData
#if MIN_VERSION_aeson(1,0,0)
  , ToJSONKey, FromJSONKey
#endif
  )

-- | SMSAero message body.
newtype MessageBody = MessageBody Text deriving (Eq, Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData)

-- | SMSAero group name.
newtype Group = Group Text deriving (Eq, Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData)

-- | SMSAero channel name.
type ChannelName = Text

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
newtype Phone = Phone { getPhone :: Int64 } deriving (Eq, Show, ToHttpApiData, FromHttpApiData)

-- | Date. Textually @SMSAeroDate@ is represented as a number of seconds since 01 Jan 1970.
newtype SMSAeroDate = SMSAeroDate { getSMSAeroDate :: UTCTime } deriving (Eq, Show)

instance ToHttpApiData SMSAeroDate where
  toQueryParam (SMSAeroDate dt) = Text.pack (show (utcTimeToPOSIXSeconds dt))

instance FromHttpApiData SMSAeroDate where
  parseQueryParam s = do
     n <- fromInteger <$> parseQueryParam s
     return (SMSAeroDate (posixSecondsToUTCTime n))

-- | Send type. This is used to describe send channel, equals to @FreeSignatureExceptMTC@ by default.
-- Textually @SendType@ is represented as a number from 1 to 6, excluding 5.
data SendType
  = PaidSignature          -- ^ Paid literal signature for all operators.
  | FreeSignatureExceptMTC -- ^ Free literal signature for all operators except MTS.
  | FreeSignature          -- ^ Free literal signature for all operators.
  | InfoSignature          -- ^ Infosignature for all operators.
  | International          -- ^ International delivery (for RU and KZ operators).
  deriving (Eq, Show, Bounded, Enum)

-- | Digital send channel. Textually represented as '1' if the parameter is present.
data DigitalChannel = DigitalChannel

instance ToHttpApiData DigitalChannel where
  toQueryParam _ = "1"

instance FromHttpApiData DigitalChannel where
  parseQueryParam "1" = Right DigitalChannel
  parseQueryParam x = Left ("expected 1 for digital channel (but got " <> x <> ")")

instance ToHttpApiData SendType where
  toQueryParam PaidSignature          = "1"
  toQueryParam FreeSignatureExceptMTC = "2"
  toQueryParam FreeSignature          = "3"
  toQueryParam InfoSignature          = "4"
  toQueryParam International          = "6"

instance FromHttpApiData SendType where
  parseQueryParam = parseBoundedQueryParam

-- | Subscriber's name.
newtype Name = Name Text deriving (Eq, Show, ToHttpApiData, FromHttpApiData)

-- | Subscriber's birth date. Textually represented in %Y-%m-%d format.
newtype BirthDate = BirthDate Day deriving (Eq, Show, ToHttpApiData, FromHttpApiData)

