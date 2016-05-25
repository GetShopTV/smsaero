-- |
-- Module      : SMSAero.Client
-- Copyright   : (c) 2015, GetShopTV
-- License     : BSD3
-- Maintainer  : nickolay@getshoptv.com
-- Stability   : experimental
--
-- SMSAero HTTP servant client and individual client functions.
module SMSAero.Client where

import Control.Monad.Trans.Either

import Data.Proxy

import Servant.API
import Servant.Client

import Network.HTTP.Client hiding (Proxy)

import SMSAero.API

-- | SMSAero client.
smsAeroClient :: Client SMSAeroAPI
smsAeroClient = client (Proxy :: Proxy SMSAeroAPI)

defaultBaseUrl :: BaseUrl
defaultBaseUrl = BaseUrl Https "gate.smsaero.ru" 443

-- | Common SMSAero client type.
type SmsAero a = Manager -> BaseUrl -> ClientM a

-- | Send a message.
smsAeroSend    :: SMSAeroAuth -> Phone -> MessageBody -> Signature -> Maybe SMSAeroDate -> Maybe SendType -> SmsAero SendResponse
-- | Check status of a previously sent message.
smsAeroStatus  :: SMSAeroAuth -> MessageId -> SmsAero MessageStatus
-- | Check balance.
smsAeroBalance :: SMSAeroAuth -> SmsAero BalanceResponse
-- | Check the list of available sender signatures.
smsAeroSenders :: SMSAeroAuth -> SmsAero SendersResponse
-- | Acquire a new signature.
smsAeroSign    :: SMSAeroAuth -> SmsAero SignResponse
(smsAeroSend    :<|>
 smsAeroStatus  :<|>
 smsAeroBalance :<|>
 smsAeroSenders :<|>
 smsAeroSign) = distributeClient smsAeroClient

