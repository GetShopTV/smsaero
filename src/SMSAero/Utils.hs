{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : SMSAero.Types
-- Copyright   : (c) 2016, GetShopTV
-- License     : BSD3
-- Maintainer  : nickolay@getshoptv.com
-- Stability   : experimental
--
-- Utility functions.

module SMSAero.Utils where

import Data.List (intercalate)
import Data.Char (isUpper, isLower, toLower)

import Data.Swagger hiding (SchemaOptions(..), name, prefix)
import qualified Data.Swagger as S

import Language.Haskell.TH

-- | Derive ToSchema instance.
deriveSchema :: Name -> Q [Dec]
deriveSchema name = [d| instance ToSchema $ty where declareNamedSchema = genericDeclareNamedSchema (schemaOptions $sname) |]
  where
    ty = return (ConT name)
    strName = nameBase name
    sname = return (LitE (StringL strName))

-- | Derive ToParamSchema instance.
deriveParamSchema :: Name -> Q [Dec]
deriveParamSchema name = [d| instance ToParamSchema $ty where toParamSchema = genericToParamSchema (schemaOptions $sname) |]
  where
    ty = return (ConT name)
    strName = nameBase name
    sname = return (LitE (StringL strName))

-- | Options to derive @'ToSchema'@/@'ToParamSchema'@ instances.
--
-- @schemaOptions prefix@ drops @prefix@ for every field and converts
-- what's left to @snake_case@.
schemaOptions :: String -> S.SchemaOptions
schemaOptions prefix = defaultSchemaOptions
  { S.fieldLabelModifier      = nameModifier prefix
  , S.constructorTagModifier  = nameModifier prefix
  }

-- Transforms to @snake_case@ and cuts longest common whole-word prefix.
nameModifier :: String -> String -> String
nameModifier prefix name = brokenWord ++ suffix
  where
    (common, _, suffix) = commonPrefix (toSnakeCase prefix ++ "_") (toSnakeCase name)
    brokenWord = reverse (takeWhile (/= '_') (reverse common))

-- | Strip longest common prefix of to lists.
commonPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
commonPrefix (x:xs) (y:ys)
  | x == y    = (x : prefix, xs', ys')
  | otherwise = ([], x:xs, y:ys)
  where
    (prefix, xs', ys') = commonPrefix xs ys
commonPrefix xs ys = ([], xs, ys)

-- | Convert @CamelCase@ to @snake_case@.
toSnakeCase :: String -> String
toSnakeCase = map toLower . intercalate "_" . splitCamelWords
  where
    splitCamelWords = reverse . splitWordsReversed . reverse

    splitWordsReversed :: String -> [String]
    splitWordsReversed [] = []
    splitWordsReversed rs
      | null ls   = reverse us : splitWordsReversed urs
      | otherwise = case lrs of
                      []     -> [reverse ls]
                      (c:cs) -> (c : reverse ls) : splitWordsReversed cs
      where
        (ls, lrs) = span isLower rs
        (us, urs) = span isUpper rs

