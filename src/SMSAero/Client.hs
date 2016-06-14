-- |
-- Module      : SMSAero.Client
-- Copyright   : (c) 2015, GetShopTV
-- License     : BSD3
-- Maintainer  : nickolay@getshoptv.com
-- Stability   : experimental
--
-- SMSAero HTTP servant client and individual client functions.
module SMSAero.Client where

import Control.Monad.Trans.Except

import Data.Proxy

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

-- | Common SMSAero client type.
type SmsAero a = Manager -> BaseUrl -> ClientM (SmsAeroResponse a)

-- | Send a message.
smsAeroSend        :: SMSAeroAuth -> Phone -> MessageBody -> Signature -> Maybe SMSAeroDate -> Maybe SendType -> SmsAero SendResponse
-- | Send a group message.
smsAeroSendToGroup :: SMSAeroAuth -> Group -> MessageBody -> Signature -> Maybe SMSAeroDate -> Maybe SendType -> SmsAero SendResponse
-- | Check status of a previously sent message.
smsAeroStatus      :: SMSAeroAuth -> MessageId -> SmsAero MessageStatus
-- | Check balance.
smsAeroBalance     :: SMSAeroAuth -> SmsAero BalanceResponse
-- | Check the list of available sender signatures.
smsAeroSenders     :: SMSAeroAuth -> SmsAero SendersResponse
-- | Acquire a new signature.
smsAeroSign        :: SMSAeroAuth -> SmsAero SignResponse
-- | Add a group.
smsAeroAddGroup    :: SMSAeroAuth -> Group -> SmsAero GroupResponse
-- | Delete a group.
smsAeroDeleteGroup :: SMSAeroAuth -> Group -> SmsAero GroupResponse

smsAeroSend        auth = let (f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroSendToGroup auth = let (_ :<|> f :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroStatus      auth = let (_ :<|> _ :<|> f :<|> _ :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroBalance     auth = let (_ :<|> _ :<|> _ :<|> f :<|> _ :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroSenders     auth = let (_ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ :<|> _) = smsAeroClient auth in f
smsAeroSign        auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = smsAeroClient auth in f
smsAeroAddGroup    auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> (f :<|> _)) = smsAeroClient auth in f
smsAeroDeleteGroup auth = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> (_ :<|> f)) = smsAeroClient auth in f

