{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SMSAero.Utils where

import Control.Applicative
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
  distributeClient client = distributeClient (left <$> client) :<|> distributeClient (right <$> client)
    where
      left  (l :<|> _) = l
      right (_ :<|> r) = r
