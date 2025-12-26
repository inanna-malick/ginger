{-# LANGUAGE OverloadedStrings #-}
-- | Validate access paths against schemas.
module Text.Ginger.TH.Validate
  ( validatePath
  , validatePaths
  , formatValidationError
  , formatValidationErrors
  , SchemaRegistry
  ) where

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec.Pos (SourcePos, sourceName, sourceLine, sourceColumn)

import Text.Ginger.TH.Types

-- | Registry mapping type names to their schemas.
-- Used to resolve RecursiveRef during validation.
type SchemaRegistry = Map Text Schema

-- | Validate multiple access paths against a schema.
validatePaths :: SchemaRegistry -> Schema -> [AccessPath] -> [ValidationError]
validatePaths registry schema = mapMaybe (validatePath registry schema)

-- | Validate a single access path against a schema.
-- Returns Nothing if valid, Just error if invalid.
validatePath :: SchemaRegistry -> Schema -> AccessPath -> Maybe ValidationError
validatePath registry schema access@(AccessPath root segments _) =
  case lookupRoot registry root schema of
    Nothing -> Just $ FieldNotFound access root
    Just fieldSchema -> validateSegments registry fieldSchema segments access

-- | Look up a root variable in the schema.
lookupRoot :: SchemaRegistry -> Text -> Schema -> Maybe Schema
lookupRoot registry name schema = case resolveSchema registry schema of
  RecordSchema fields -> lookup name fields
  SumSchema constructors -> lookupSumField name constructors
  _ -> Nothing  -- Can't look up in List, Scalar, or unresolved RecursiveRef

-- | Resolve a RecursiveRef to its actual schema.
resolveSchema :: SchemaRegistry -> Schema -> Schema
resolveSchema registry (RecursiveRef typeName) =
  case Map.lookup typeName registry of
    Just s -> s
    Nothing -> ScalarSchema  -- Fallback if not found (shouldn't happen)
resolveSchema _ schema = schema

-- | Look up a field in a sum type (must exist in ALL constructors).
lookupSumField :: Text -> [[(Text, Schema)]] -> Maybe Schema
lookupSumField fieldName constructors =
  let lookups = map (lookup fieldName) constructors
  in if all isJust lookups && allSame (mapMaybe id lookups)
     then head lookups
     else Nothing
  where
    isJust (Just _) = True
    isJust Nothing = False

    allSame [] = True
    allSame (x:xs) = all (schemaEq x) xs

    -- Simple structural equality for schemas
    schemaEq ScalarSchema ScalarSchema = True
    schemaEq (ListSchema a) (ListSchema b) = schemaEq a b
    schemaEq (RecordSchema a) (RecordSchema b) =
      length a == length b &&
      all (\((n1, s1), (n2, s2)) -> n1 == n2 && schemaEq s1 s2) (zip a b)
    schemaEq (SumSchema a) (SumSchema b) =
      length a == length b  -- Simplified comparison
    schemaEq _ _ = False

-- | Validate path segments against a schema.
validateSegments :: SchemaRegistry -> Schema -> [PathSegment] -> AccessPath -> Maybe ValidationError
validateSegments _ _ [] _ = Nothing  -- Reached end of path, valid

validateSegments registry schema (seg:rest) access =
  -- First resolve any RecursiveRef
  case (resolveSchema registry schema, seg) of
    -- Record with static key access
    (RecordSchema fields, StaticKey key) ->
      case lookup key fields of
        Nothing -> Just $ FieldNotFound access key
        Just fieldSchema -> validateSegments registry fieldSchema rest access

    -- Record with dynamic key access - NOT ALLOWED
    (RecordSchema _, DynamicKey) ->
      Just $ DynamicAccessNotAllowed access

    -- Sum type with static key access
    (SumSchema constructors, StaticKey key) ->
      case lookupSumField key constructors of
        Nothing -> Just $ FieldNotInAllConstructors access key
        Just fieldSchema -> validateSegments registry fieldSchema rest access

    -- Sum type with dynamic key access - NOT ALLOWED
    (SumSchema _, DynamicKey) ->
      Just $ DynamicAccessNotAllowed access

    -- List with any key access (index)
    (ListSchema elemSchema, _) ->
      validateSegments registry elemSchema rest access

    -- Scalar with any key access - NOT ALLOWED
    (ScalarSchema, _) ->
      Just $ AccessOnScalar access

    -- RecursiveRef that couldn't be resolved (shouldn't happen)
    (RecursiveRef _, _) ->
      Just $ UnknownType access "unresolved recursive reference"

-- | Format a single validation error as a string.
formatValidationError :: ValidationError -> String
formatValidationError err = case err of
  FieldNotFound access field ->
    formatLocation (apSourcePos access) ++
    "  Error: Field '" ++ Text.unpack field ++ "' not found\n" ++
    "  In access: " ++ formatAccessPath access ++ "\n"

  FieldNotInAllConstructors access field ->
    formatLocation (apSourcePos access) ++
    "  Error: Field '" ++ Text.unpack field ++
    "' does not exist in all constructors\n" ++
    "  In access: " ++ formatAccessPath access ++ "\n" ++
    "  Hint: For sum types, accessed fields must exist in ALL constructors\n"

  DynamicAccessNotAllowed access ->
    formatLocation (apSourcePos access) ++
    "  Error: Dynamic key access [expr] is not allowed\n" ++
    "  In access: " ++ formatAccessPath access ++ "\n" ++
    "  Hint: Use static field access .field or [\"literal\"] instead\n"

  AccessOnScalar access ->
    formatLocation (apSourcePos access) ++
    "  Error: Cannot access field on scalar type\n" ++
    "  In access: " ++ formatAccessPath access ++ "\n"

  UnknownType access typeName ->
    formatLocation (apSourcePos access) ++
    "  Error: Unknown type '" ++ Text.unpack typeName ++ "'\n" ++
    "  In access: " ++ formatAccessPath access ++ "\n"

-- | Format multiple validation errors.
formatValidationErrors :: [ValidationError] -> String
formatValidationErrors [] = ""
formatValidationErrors errors =
  "Template type checking failed:\n\n" ++
  concatMap formatValidationError errors

-- | Format an access path for error messages.
formatAccessPath :: AccessPath -> String
formatAccessPath (AccessPath root segments _) =
  Text.unpack root ++ concatMap formatSegment segments
  where
    formatSegment (StaticKey k) = "." ++ Text.unpack k
    formatSegment DynamicKey = "[<expr>]"

-- | Format source location for error messages.
formatLocation :: SourcePos -> String
formatLocation pos =
  sourceName pos ++ ":" ++
  show (sourceLine pos) ++ ":" ++
  show (sourceColumn pos) ++ ":\n"
