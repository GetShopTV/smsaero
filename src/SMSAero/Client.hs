module SMSAero.Client where

import Control.Monad.Trans.Either

import Data.Proxy
import Data.Text (Text)

import Servant.API
import Servant.Client

import SMSAero.API
import SMSAero.Utils

-- | SMSAero client.
smsAeroClient :: Client SMSAeroAPI
smsAeroClient = client (Proxy :: Proxy SMSAeroAPI) host
  where
    host = BaseUrl Https "gate.smsaero.ru" 443

-- | Common SMSAero client type.
type SmsAero a = EitherT ServantError IO (SmsAeroResponse a)

-- | Send a message.
smsAeroSend    :: SMSAeroAuth -> Phone -> Text -> Signature -> Maybe SMSAeroDate -> SmsAero SendResponse
-- | Check status of a previously sent message.
smsAeroStatus  :: SMSAeroAuth -> MessageId -> SmsAero StatusResponse
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

