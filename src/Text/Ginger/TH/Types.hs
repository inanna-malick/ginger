{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
-- | Types for compile-time template type checking.
module Text.Ginger.TH.Types
  ( -- * Schema representation
    Schema(..)
    -- * Access paths extracted from templates
  , AccessPath(..)
  , PathSegment(..)
    -- * Validation results
  , ValidationError(..)
    -- * Typed template wrapper
  , TypedTemplate(..)
  ) where

import Data.Text (Text)
import Data.Data (Data, Typeable)
import Language.Haskell.TH.Syntax (Lift)
import Text.Parsec.Pos (SourcePos)

import Text.Ginger.AST (Template)

-- | Schema representation for compile-time type analysis.
-- Represents the structure of Haskell types that can be used as template contexts.
data Schema
  = RecordSchema [(Text, Schema)]
    -- ^ Product type (single-constructor record) with named fields
  | SumSchema [[(Text, Schema)]]
    -- ^ Sum type with multiple constructors, each having its own field set.
    -- For a field to be valid, it must exist in ALL constructors.
  | ListSchema Schema
    -- ^ List or Vector type, element schema
  | ScalarSchema
    -- ^ Leaf type (Text, Int, Bool, Scientific, etc.)
  | RecursiveRef Text
    -- ^ Reference to a recursive type. Used to handle recursive data structures.
    -- The Text is the type name for resolution during validation.
  deriving (Show, Eq, Data, Typeable, Lift)

-- | A path segment in a variable access chain.
data PathSegment
  = StaticKey Text
    -- ^ Static field access: @.fieldName@ or @[\"literal\"]@
  | DynamicKey
    -- ^ Dynamic key access: @[expr]@ where expr is not a literal.
    -- This is always a compile error in v1.
  deriving (Show, Eq, Data, Typeable)

-- | A variable access path extracted from a template.
-- Represents expressions like @{{ user.profile.name }}@.
data AccessPath = AccessPath
  { apRoot :: Text
    -- ^ Top-level variable name (e.g., \"user\")
  , apPath :: [PathSegment]
    -- ^ Chain of accesses (e.g., [StaticKey \"profile\", StaticKey \"name\"])
  , apSourcePos :: SourcePos
    -- ^ Source position for error messages
  } deriving (Show, Eq, Data, Typeable)

-- | Validation error for a single access path.
data ValidationError
  = FieldNotFound AccessPath Text
    -- ^ Field doesn't exist. Second arg is the missing field name.
  | FieldNotInAllConstructors AccessPath Text
    -- ^ For sum types: field exists in some but not all constructors.
  | DynamicAccessNotAllowed AccessPath
    -- ^ Dynamic key access @[expr]@ is not allowed.
  | AccessOnScalar AccessPath
    -- ^ Tried to access a field on a scalar type.
  | UnknownType AccessPath Text
    -- ^ Encountered a type we can't analyze.
  deriving (Show, Eq, Data, Typeable)

-- | A template that has been type-checked against a specific context type.
-- The type parameter @a@ is the required context type.
-- The type parameter @p@ is the source position type (usually 'SourcePos').
newtype TypedTemplate a p = TypedTemplate
  { unTypedTemplate :: Template p
  } deriving (Show)
