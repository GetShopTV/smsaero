-- |
-- Module      : SMSAero.Client
-- Copyright   : (c) 2015, GetShopTV
-- License     : BSD3
-- Maintainer  : nickolay@getshoptv.com
-- Stability   : experimental
--
-- SMSAero HTTP servant client and individual client functions.
module SMSAero.Client (
  -- * Client
  smsAeroClient,
  defaultBaseUrl,
  SmsAero,
  -- ** Sending a message
  smsAeroSend,
  smsAeroSendToGroup,
  -- ** Checking status
  smsAeroStatus,
  smsAeroCheckSending,
  -- ** Checking balance and tariff
  smsAeroBalance,
  smsAeroCheckTariff,
  -- ** Signatures
  smsAeroSenders,
  smsAeroSign,
  -- ** Groups
  smsAeroListGroups,
  smsAeroAddGroup,
  smsAeroDeleteGroup,
  -- ** Contacts
  smsAeroAddPhone,
  smsAeroDeletePhone,
  -- ** Blacklist
  smsAeroAddToBlacklist,
) where

import Data.Proxy
import Data.Text (Text)

import Servant.API
import Servant.Client

import SMSAero.API
import SMSAero.Types

-- | SMSAero client.
smsAeroClient :: Client ClientM SMSAeroAPI
smsAeroClient = client (Proxy :: Proxy SMSAeroAPI)

-- | Default host @https://gate.smsaero.ru@.
defaultBaseUrl :: BaseUrl
defaultBaseUrl = BaseUrl Https "gate.smsaero.ru" 443 ""

-- | Common SMSAero client response type.
type SmsAero a = ClientM (SmsAeroResponse a)

-- | Send a message.
smsAeroSend :: SMSAeroAuth          -- ^ Authentication data (login and MD5 hash of password).
            -> Phone                -- ^ Phone number to send a message to.
            -> MessageBody          -- ^ Message text.
            -> Signature            -- ^ Sender's signature used for the "from" field.
            -> Maybe SMSAeroDate    -- ^ Date stating when to send a postponed message.
            -> Maybe SendType       -- ^ Send channel description.
            -> Maybe DigitalChannel -- ^ Use digital send channel.
            -> SmsAero SendResponse

-- | Send a group message.
smsAeroSendToGroup :: SMSAeroAuth          -- ^ Authentication data (login and MD5 hash of password).
                   -> Group                -- ^ Group name for broadcasting a message.
                   -> MessageBody          -- ^ Message text.
                   -> Signature            -- ^ Sender's signature used for the "from" field.
                   -> Maybe SMSAeroDate    -- ^ Date stating when to send a postponed message.
                   -> Maybe SendType       -- ^ Send channel description.
                   -> Maybe DigitalChannel -- ^ Use digital send channel.
                   -> SmsAero SendResponse

-- | Check status of a previously sent message.
smsAeroStatus :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
              -> MessageId   -- ^ Message ID.
              -> SmsAero MessageStatus

-- | Check status of a broadcast.
smsAeroCheckSending :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
                    -> MessageId   -- ^ Broadcast ID.
                    -> SmsAero CheckSendingResponse

-- | Check balance.
smsAeroBalance :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
               -> SmsAero BalanceResponse

-- | Check tariff.
smsAeroCheckTariff :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
                   -> SmsAero CheckTariffResponse

-- | Check the list of available sender signatures.
smsAeroSenders :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
               -> SmsAero SendersResponse

-- | Acquire a new signature.
smsAeroSign :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
            -> SmsAero SignResponse

-- | Get groups list (corresponds to 'checkgroup' endpoint).
smsAeroListGroups :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
                  -> SmsAero [Group]

-- | Add a group.
smsAeroAddGroup :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
                -> Group       -- ^ Name for the new group.
                -> SmsAero GroupResponse

-- | Delete a group.
smsAeroDeleteGroup :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
                   -> Group       -- ^ Name of the group to be deleted.
                   -> SmsAero GroupResponse

-- | Add a phone to contact list or group.
smsAeroAddPhone :: SMSAeroAuth     -- ^ Authentication data (login and MD5 hash of password).
                -> Phone           -- ^ Subscriber's phone number.
                -> Maybe Group     -- ^ Contact group. If absent, contact will be added to general contact list.
                -> Maybe Name      -- ^ Subscriber's last name.
                -> Maybe Name      -- ^ Subscriber's first name.
                -> Maybe Name      -- ^ Subscriber's middle name.
                -> Maybe BirthDate -- ^ Subscriber's birth date.
                -> Maybe Text      -- ^ Any additional information.
                -> SmsAero PhoneResponse

-- | Delete a phone from contact list or group.
smsAeroDeletePhone :: SMSAeroAuth     -- ^ Authentication data (login and MD5 hash of password).
                   -> Phone           -- ^ Subscriber's phone number.
                   -> Maybe Group     -- ^ Group to remove contact from. If absent, contact will be deleted from general contact list.
                   -> SmsAero PhoneResponse

-- | Add a phone number to blacklist.
smsAeroAddToBlacklist :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password)
                      -> Phone       -- ^ Phone number to be added to blacklist.
                      -> SmsAero BlacklistResponse

smsAeroSend           auth = let (f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroSendToGroup    auth = let (_ :<|> f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroStatus         auth = let (_ :<|> _ :<|> f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroCheckSending   auth = let (_ :<|> _ :<|> _ :<|> f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroBalance        auth = let (_ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroCheckTariff    auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroSenders        auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroSign           auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroListGroups     auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> (f :<|> _ :<|> _) :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroAddGroup       auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> (_ :<|> f :<|> _) :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroDeleteGroup    auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> (_ :<|> _ :<|> f) :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroAddPhone       auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> (f :<|> _) :<|> _) = smsAeroClient auth in f
smsAeroDeletePhone    auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> (_ :<|> f) :<|> _) = smsAeroClient auth in f
smsAeroAddToBlacklist auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f) = smsAeroClient auth in f

