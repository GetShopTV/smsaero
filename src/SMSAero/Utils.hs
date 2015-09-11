{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : SMSAero.Utils
-- Copyright   : (c) 2015, GetShopTV
-- License     : BSD3
-- Maintainer  : nickolay@getshoptv.com
-- Stability   : experimental
--
-- This module defines @'DistributiveClient'@ class
-- to help distribute functions over alternatives (@':<|>'@).
module SMSAero.Utils where

import Servant.API.Alternative

-- | Distribute a client looking like
--
-- @
-- a -> (b ':<|>' ... ':<|>' c)
-- @
--
-- into
--
-- @
-- (a -> b) ':<|>' ...  ':<|>' (a -> c)
-- @
--
-- This is useful to bring authentication credentials to
-- individual client endpoint queries.
class DistributiveClient client client' where
  distributeClient :: client -> client'

-- | Base case.
instance DistributiveClient (a -> b) (a -> b) where
  distributeClient = id

-- | Distribute function over alternative.
instance (DistributiveClient (a -> b) b', DistributiveClient (a -> c) c') => DistributiveClient (a -> (b :<|> c)) (b' :<|> c') where
  distributeClient client = distributeClient (fmap left client) :<|> distributeClient (fmap right client)
    where
      left  (l :<|> _) = l
      right (_ :<|> r) = r
