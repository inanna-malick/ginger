{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Generic deriving for 'ToGVal' instances using GHC.Generics.
--
-- This module provides 'genericToGVal' which converts sum types to a
-- "constructor as optional field" representation, enabling ergonomic
-- pattern matching in templates:
--
-- @
-- data Status = Blocked String | Pursuing Int | Achieved
--   deriving (Generic)
--
-- instance ToGVal m Status where
--   toGVal = genericToGVal
-- @
--
-- Template usage:
--
-- @
-- {% if status.Blocked is defined %}
--   Blocked: {{ status.Blocked }}
-- {% elif status.Pursuing is defined %}
--   Progress: {{ status.Pursuing }}%
-- {% elif status.Achieved is defined %}
--   Complete!
-- {% endif %}
-- @
--
module Text.Ginger.GVal.Generic
  ( genericToGVal
  , GToGVal(..)
  ) where

import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import GHC.Generics

import Text.Ginger.GVal
  ( GVal(..)
  , ToGVal(..)
  , Pair
  , (~>)
  )
import Text.Ginger.Html (html)

-- | Convert a value with a Generic instance to GVal using sum type representation.
--
-- Sum types are represented as dictionaries where:
--
-- * Only the active constructor's field is defined
-- * 'asText' returns the constructor name (for @==@ comparisons)
-- * 'asBoolean' is 'True' (for truthiness checks)
-- * 0-field constructors map to a truthy value with constructor name as text
-- * Single-field constructors unwrap directly
-- * Multi-field record constructors create a nested dictionary
-- * Multi-field positional constructors use @_0@, @_1@, etc.
--
genericToGVal :: (Generic a, GToGVal m (Rep a)) => a -> GVal m
genericToGVal = gToGVal . from

-- | Generic representation class for converting to GVal.
class GToGVal m f where
  gToGVal :: f p -> GVal m

-- | Datatype metadata wrapper - just pass through
instance GToGVal m f => GToGVal m (M1 D c f) where
  gToGVal (M1 x) = gToGVal x

-- | Constructor wrapper - creates the constructor-as-field representation
instance (GToGValInner m f, Constructor c) => GToGVal m (M1 C c f) where
  gToGVal m@(M1 x) =
    let cName = Text.pack $ conName m
        inner = gToGValInner 0 x
    in mkSumGVal cName inner

-- | Sum type combinator - delegates to the active branch
instance (GToGVal m f, GToGVal m g) => GToGVal m (f :+: g) where
  gToGVal (L1 x) = gToGVal x
  gToGVal (R1 x) = gToGVal x

-- | Create a GVal for a sum type constructor.
-- The constructor name becomes both the asText and the only defined field.
mkSumGVal :: Text -> GVal m -> GVal m
mkSumGVal conName inner =
  def
    { asText = conName
    , asHtml = html conName
    , asBoolean = True
    , isNull = False
    , asLookup = Just $ \key ->
        if key == conName
          then Just inner
          else Nothing
    , asDictItems = Just [(conName, inner)]
    }

-- | Class for converting constructor fields to GVal.
-- Handles the inner content of a constructor.
-- The Int parameter is the field index for positional fields.
class GToGValInner m f where
  gToGValInner :: Int -> f p -> GVal m

-- | Unit type (nullary constructor)
instance GToGValInner m U1 where
  gToGValInner _ U1 =
    -- Nullary constructors are truthy with constructor name as text
    -- (the outer mkSumGVal handles asText, this is just the inner value)
    def { asBoolean = True, isNull = False, asText = "" }

-- | Single field (leaf node) - unwrap directly
instance ToGVal m a => GToGValInner m (K1 i a) where
  gToGValInner _ (K1 x) = toGVal x

-- | Selector (field with name) - record field or positional
instance (GToGValInner m f, Selector s) => GToGValInner m (M1 S s f) where
  gToGValInner idx m@(M1 x) = gToGValInner idx x

-- | Product type (multiple fields) - create a record
instance (GToGValProduct m f, GToGValProduct m g) => GToGValInner m (f :*: g) where
  gToGValInner _ prod =
    let pairs = gCollectFields 0 prod
    in mkRecordGVal pairs

-- | Create a GVal from record fields
mkRecordGVal :: [Pair m] -> GVal m
mkRecordGVal pairs =
  let hm = HashMap.fromList pairs
  in def
    { asBoolean = True
    , isNull = False
    , asLookup = Just (`HashMap.lookup` hm)
    , asDictItems = Just pairs
    }

-- | Helper class for collecting fields from product types
class GToGValProduct m f where
  gCollectFields :: Int -> f p -> [Pair m]

instance (GToGValProduct m f, GToGValProduct m g) => GToGValProduct m (f :*: g) where
  gCollectFields idx (x :*: y) =
    let leftFields = gCollectFields idx x
        leftCount = Prelude.length leftFields
    in leftFields ++ gCollectFields (idx + leftCount) y

instance (GToGValInner m f, Selector s) => GToGValProduct m (M1 S s f) where
  gCollectFields idx m@(M1 x) =
    let fieldName = Text.pack $ selName m
        name = if Text.null fieldName
               then "_" <> Text.pack (show idx)  -- Positional: _0, _1, etc.
               else fieldName                     -- Named: actual field name
    in [(name, gToGValInner idx x)]
