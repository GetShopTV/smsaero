{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SMSAero.Utils where

import Servant.API.Alternative

-- | Distribute a client looking like
--
-- @
-- a -> (b :<|> ... :<|> c)
-- @
--
-- into
--
-- @
-- (a -> b) :<|> ...  :<|> (a -> c)
-- @
--
-- This is useful to bring authentication credentials to
-- individual client endpoint queries.
class DistributiveClient client client' where
  distributeClient :: client -> client'

instance DistributiveClient (a -> b) (a -> b) where
  distributeClient = id

instance (DistributiveClient (a -> b) b', DistributiveClient (a -> c) c') => DistributiveClient (a -> (b :<|> c)) (b' :<|> c') where
  distributeClient client = distributeClient (aleft <$> client) :<|> distributeClient (aright <$> client)
    where
      aleft  (l :<|> _) = l
      aright (_ :<|> r) = r
