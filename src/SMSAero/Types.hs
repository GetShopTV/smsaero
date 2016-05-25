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
  Phone(..),
  SMSAeroDate(..),
  SendType,
) where

import Control.Applicative (empty)

import Data.Aeson
import Data.Int (Int64)

import Data.Time (UTCTime(UTCTime))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readEither)

import Web.HttpApiData

-- | SMSAero sender's signature. This is used for the "from" field.
newtype Signature = Signature { getSignature :: Text } deriving (Eq, Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData)

-- | SMSAero sent message id.
newtype MessageId = MessageId Int64 deriving (Eq, Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData)

-- | SMSAero message body.
newtype MessageBody = MessageBody Text deriving (Eq, Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData)

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

type SendType = Int

