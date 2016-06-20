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

import Network.HTTP.Client (Manager)

-- | SMSAero client.
smsAeroClient :: Client SMSAeroAPI
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
            -> Manager              -- ^ Connection manager.
            -> SmsAero SendResponse
smsAeroSend auth p m s md mt mdig manager = smsAeroSend_ auth p m s md mt mdig manager defaultBaseUrl

-- | Send a group message.
smsAeroSendToGroup :: SMSAeroAuth          -- ^ Authentication data (login and MD5 hash of password).
                   -> Group                -- ^ Group name for broadcasting a message.
                   -> MessageBody          -- ^ Message text.
                   -> Signature            -- ^ Sender's signature used for the "from" field.
                   -> Maybe SMSAeroDate    -- ^ Date stating when to send a postponed message.
                   -> Maybe SendType       -- ^ Send channel description.
                   -> Maybe DigitalChannel -- ^ Use digital send channel.
                   -> Manager              -- ^ Connection manager.
                   -> SmsAero SendResponse
smsAeroSendToGroup auth g m s md mt mdig manager = smsAeroSendToGroup_ auth g m s md mt mdig manager defaultBaseUrl

-- | Check status of a previously sent message.
smsAeroStatus :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
              -> MessageId   -- ^ Message ID.
              -> Manager     -- ^ Connection manager.
              -> SmsAero MessageStatus
smsAeroStatus auth mid manager = smsAeroStatus_ auth mid manager defaultBaseUrl

-- | Check status of a broadcast.
smsAeroCheckSending :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
                    -> MessageId   -- ^ Broadcast ID.
                    -> Manager     -- ^ Connection manager.
                    -> SmsAero CheckSendingResponse
smsAeroCheckSending auth mid manager = smsAeroCheckSending_ auth mid manager defaultBaseUrl

-- | Check balance.
smsAeroBalance :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
               -> Manager     -- ^ Connection manager.
               -> SmsAero BalanceResponse
smsAeroBalance auth manager = smsAeroBalance_ auth manager defaultBaseUrl

-- | Check tariff.
smsAeroCheckTariff :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
                   -> Manager     -- ^ Connection manager.
                   -> SmsAero CheckTariffResponse
smsAeroCheckTariff auth manager = smsAeroCheckTariff_ auth manager defaultBaseUrl

-- | Check the list of available sender signatures.
smsAeroSenders :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
               -> Manager     -- ^ Connection manager.
               -> SmsAero SendersResponse
smsAeroSenders auth manager = smsAeroSenders_ auth manager defaultBaseUrl

-- | Acquire a new signature.
smsAeroSign :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
            -> Manager     -- ^ Connection manager.
            -> SmsAero SignResponse
smsAeroSign auth manager = smsAeroSign_ auth manager defaultBaseUrl

-- | Get groups list (corresponds to 'checkgroup' endpoint).
smsAeroListGroups :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
                  -> Manager     -- ^ Connection manager.
                  -> SmsAero [Group]
smsAeroListGroups auth manager = smsAeroListGroups_ auth manager defaultBaseUrl

-- | Add a group.
smsAeroAddGroup :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
                -> Group       -- ^ Name for the new group.
                -> Manager     -- ^ Connection manager.
                -> SmsAero GroupResponse
smsAeroAddGroup auth g manager = smsAeroAddGroup_ auth g manager defaultBaseUrl

-- | Delete a group.
smsAeroDeleteGroup :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password).
                   -> Group       -- ^ Name of the group to be deleted.
                   -> Manager     -- ^ Connection manager.
                   -> SmsAero GroupResponse
smsAeroDeleteGroup auth g manager = smsAeroDeleteGroup_ auth g manager defaultBaseUrl

-- | Add a phone to contact list or group.
smsAeroAddPhone :: SMSAeroAuth     -- ^ Authentication data (login and MD5 hash of password).
                -> Phone           -- ^ Subscriber's phone number.
                -> Maybe Group     -- ^ Contact group. If absent, contact will be added to general contact list.
                -> Maybe Name      -- ^ Subscriber's last name.
                -> Maybe Name      -- ^ Subscriber's first name.
                -> Maybe Name      -- ^ Subscriber's middle name.
                -> Maybe BirthDate -- ^ Subscriber's birth date.
                -> Maybe Text      -- ^ Any additional information.
                -> Manager         -- ^ Connection manager.
                -> SmsAero PhoneResponse
smsAeroAddPhone auth p g ln fn mn bd txt manager = smsAeroAddPhone_ auth p g ln fn mn bd txt manager defaultBaseUrl

-- | Delete a phone from contact list or group.
smsAeroDeletePhone :: SMSAeroAuth     -- ^ Authentication data (login and MD5 hash of password).
                   -> Phone           -- ^ Subscriber's phone number.
                   -> Maybe Group     -- ^ Group to remove contact from. If absent, contact will be deleted from general contact list.
                   -> Manager         -- ^ Connection manager.
                   -> SmsAero PhoneResponse
smsAeroDeletePhone auth p mg manager = smsAeroDeletePhone_ auth p mg manager defaultBaseUrl

-- | Add a phone number to blacklist.
smsAeroAddToBlacklist :: SMSAeroAuth -- ^ Authentication data (login and MD5 hash of password)
                      -> Phone       -- ^ Phone number to be added to blacklist.
                      -> Manager     -- ^ Connection manager.
                      -> SmsAero BlacklistResponse
smsAeroAddToBlacklist auth p manager = smsAeroAddToBlacklist_ auth p manager defaultBaseUrl

smsAeroSend_ :: SMSAeroAuth -> Phone -> MessageBody -> Signature -> Maybe SMSAeroDate -> Maybe SendType -> Maybe DigitalChannel -> Manager -> BaseUrl -> SmsAero SendResponse
smsAeroSendToGroup_ :: SMSAeroAuth -> Group -> MessageBody -> Signature -> Maybe SMSAeroDate -> Maybe SendType -> Maybe DigitalChannel -> Manager -> BaseUrl -> SmsAero SendResponse
smsAeroStatus_ :: SMSAeroAuth -> MessageId -> Manager -> BaseUrl -> SmsAero MessageStatus
smsAeroCheckSending_ :: SMSAeroAuth -> MessageId -> Manager -> BaseUrl -> SmsAero CheckSendingResponse
smsAeroBalance_ :: SMSAeroAuth -> Manager -> BaseUrl -> SmsAero BalanceResponse
smsAeroCheckTariff_ :: SMSAeroAuth -> Manager -> BaseUrl -> SmsAero CheckTariffResponse
smsAeroSenders_ :: SMSAeroAuth -> Manager -> BaseUrl -> SmsAero SendersResponse
smsAeroSign_ :: SMSAeroAuth -> Manager -> BaseUrl -> SmsAero SignResponse
smsAeroListGroups_ :: SMSAeroAuth -> Manager -> BaseUrl -> SmsAero [Group]
smsAeroAddGroup_ :: SMSAeroAuth -> Group -> Manager -> BaseUrl -> SmsAero GroupResponse
smsAeroDeleteGroup_ :: SMSAeroAuth -> Group -> Manager -> BaseUrl -> SmsAero GroupResponse
smsAeroAddPhone_ :: SMSAeroAuth -> Phone -> Maybe Group -> Maybe Name -> Maybe Name -> Maybe Name -> Maybe BirthDate -> Maybe Text -> Manager -> BaseUrl -> SmsAero PhoneResponse
smsAeroDeletePhone_ :: SMSAeroAuth -> Phone -> Maybe Group -> Manager -> BaseUrl -> SmsAero PhoneResponse
smsAeroAddToBlacklist_ :: SMSAeroAuth -> Phone -> Manager -> BaseUrl -> SmsAero BlacklistResponse

smsAeroSend_           auth = let (f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroSendToGroup_    auth = let (_ :<|> f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroStatus_         auth = let (_ :<|> _ :<|> f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroCheckSending_   auth = let (_ :<|> _ :<|> _ :<|> f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroBalance_        auth = let (_ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroCheckTariff_    auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroSenders_        auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroSign_           auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroListGroups_     auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> (f :<|> _ :<|> _) :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroAddGroup_       auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> (_ :<|> f :<|> _) :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroDeleteGroup_    auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> (_ :<|> _ :<|> f) :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroAddPhone_       auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> (f :<|> _) :<|> _) = smsAeroClient auth in f
smsAeroDeletePhone_    auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> (_ :<|> f) :<|> _) = smsAeroClient auth in f
smsAeroAddToBlacklist_ auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f) = smsAeroClient auth in f

