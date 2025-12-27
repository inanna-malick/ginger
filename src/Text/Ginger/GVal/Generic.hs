{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Generic deriving for 'ToGVal' instances using GHC.Generics.
--
-- This module re-exports 'genericToGVal' and 'GToGVal' from "Text.Ginger.GVal"
-- for backwards compatibility. New code can import directly from "Text.Ginger.GVal".
--
-- With @DefaultSignatures@, you can now derive 'ToGVal' instances directly:
--
-- @
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
--
-- data Status = Blocked String | Pursuing Int | Achieved
--   deriving stock (Generic)
--   deriving anyclass (ToGVal m)
-- @
--
-- Or use the explicit instance pattern:
--
-- @
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

-- Re-export from Text.Ginger.GVal for backwards compatibility
import Text.Ginger.GVal (genericToGVal, GToGVal(..))
