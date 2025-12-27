{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Generate Schema from Haskell types via Template Haskell reification.
module Text.Ginger.TH.Schema
  ( generateSchema
  , SchemaRegistry
  ) where

import Control.Monad (forM)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))

import Text.Ginger.TH.Types (Schema(..))

-- | Registry mapping type names to their schemas.
-- Used to resolve RecursiveRef during validation.
type SchemaRegistry = Map Text Schema

-- | Generate a Schema from a Haskell type name.
-- Returns both the schema and a registry of all type schemas encountered.
-- This is run at compile time via Template Haskell.
generateSchema :: Name -> Q (Schema, SchemaRegistry)
generateSchema typeName = do
  -- Use a memoization map to handle recursive types
  visited <- runIO $ newIORef Set.empty
  memo <- runIO $ newIORef Map.empty
  schema <- generateSchemaWithMemo visited memo typeName
  -- Convert the memo to a registry with Text keys
  memoMap <- runIO $ readIORef memo
  let registry = Map.mapKeys (Text.pack . nameBase) memoMap
  -- Also add the root type to the registry
  let rootName = Text.pack $ nameBase typeName
  let registry' = Map.insert rootName schema registry
  return (schema, registry')

-- | Generate schema with memoization for recursive types.
generateSchemaWithMemo :: IORef (Set Name) -> IORef (Map Name Schema) -> Name -> Q Schema
generateSchemaWithMemo visited memo typeName = do
  -- Check if we're already processing this type (recursion)
  inProgress <- runIO $ readIORef visited
  if typeName `Set.member` inProgress
    then do
      -- We're in a recursive type. Return a reference that can be
      -- resolved during validation by looking up the type name.
      return $ RecursiveRef (Text.pack $ nameBase typeName)
    else do
      -- Check memo
      memoized <- runIO $ readIORef memo
      case Map.lookup typeName memoized of
        Just schema -> return schema
        Nothing -> do
          -- Mark as in-progress
          runIO $ modifyIORef' visited (Set.insert typeName)
          -- Generate the schema
          schema <- generateSchemaForType visited memo typeName
          -- Cache it
          runIO $ modifyIORef' memo (Map.insert typeName schema)
          -- Remove from in-progress
          runIO $ modifyIORef' visited (Set.delete typeName)
          return schema

-- | Generate schema for a specific type.
generateSchemaForType :: IORef (Set Name) -> IORef (Map Name Schema) -> Name -> Q Schema
generateSchemaForType visited memo typeName = do
  info <- reify typeName
  case info of
    TyConI dec -> schemaFromDec visited memo dec
    _ -> fail $ "generateSchema: Expected a type, got: " ++ show typeName

-- | Generate schema from a type declaration.
schemaFromDec :: IORef (Set Name) -> IORef (Map Name Schema) -> Dec -> Q Schema
schemaFromDec visited memo dec = case dec of
  -- Single constructor data type
  DataD _ _ tvs _ [con] _ -> do
    checkNoTypeVars tvs
    schemaFromCon visited memo con

  -- Multiple constructors (sum type)
  DataD _ _ tvs _ cons _ -> do
    checkNoTypeVars tvs
    fieldSets <- mapM (fieldsFromCon visited memo) cons
    return $ SumSchema fieldSets

  -- Newtype
  NewtypeD _ _ tvs _ con _ -> do
    checkNoTypeVars tvs
    schemaFromCon visited memo con

  -- Type synonym
  TySynD _ tvs ty -> do
    checkNoTypeVars tvs
    schemaFromType visited memo ty

  _ -> fail $ "generateSchema: Unsupported declaration type: " ++ show dec

-- | Check that there are no type variables (reject polymorphic types).
checkNoTypeVars :: [TyVarBndr a] -> Q ()
checkNoTypeVars [] = return ()
checkNoTypeVars tvs = fail $
  "generateSchema: Polymorphic types are not supported. " ++
  "Type has type variables: " ++ show (map tvName tvs)
  where
    tvName (PlainTV n _) = n
    tvName (KindedTV n _ _) = n

-- | Generate schema from a constructor.
schemaFromCon :: IORef (Set Name) -> IORef (Map Name Schema) -> Con -> Q Schema
schemaFromCon visited memo con = case con of
  RecC _ fields -> do
    fieldSchemas <- forM fields $ \(fieldName, _, fieldType) -> do
      schema <- schemaFromType visited memo fieldType
      return (Text.pack $ nameBase fieldName, schema)
    return $ RecordSchema fieldSchemas

  NormalC _ _ ->
    fail "generateSchema: Only record constructors are supported. Use named fields."

  InfixC _ _ _ ->
    fail "generateSchema: Infix constructors are not supported."

  ForallC _ _ _ ->
    fail "generateSchema: Existential types are not supported."

  GadtC _ _ _ ->
    fail "generateSchema: GADTs are not supported."

  RecGadtC _ _ _ ->
    fail "generateSchema: Record GADTs are not supported."

-- | Extract fields from a constructor (for sum types).
fieldsFromCon :: IORef (Set Name) -> IORef (Map Name Schema) -> Con -> Q [(Text, Schema)]
fieldsFromCon visited memo con = case con of
  RecC _ fields -> do
    forM fields $ \(fieldName, _, fieldType) -> do
      schema <- schemaFromType visited memo fieldType
      return (Text.pack $ nameBase fieldName, schema)

  NormalC name [] ->
    -- Nullary constructor, no fields
    return []

  NormalC name _ ->
    fail $ "generateSchema: Non-record constructor '" ++ nameBase name ++
           "' with fields is not supported. Use named fields."

  _ -> fail "generateSchema: Unsupported constructor type in sum type."

-- | Generate schema from a Type.
schemaFromType :: IORef (Set Name) -> IORef (Map Name Schema) -> Type -> Q Schema
schemaFromType visited memo ty = case ty of
  -- List type [a]
  AppT ListT elemType -> do
    elemSchema <- schemaFromType visited memo elemType
    return $ ListSchema elemSchema

  -- Maybe a - treat as the inner type (accessing Maybe field might be null)
  AppT (ConT maybeName) innerType
    | nameBase maybeName == "Maybe" -> do
        schemaFromType visited memo innerType

  -- Vector a
  AppT (ConT vectorName) elemType
    | nameBase vectorName == "Vector" -> do
        elemSchema <- schemaFromType visited memo elemType
        return $ ListSchema elemSchema

  -- Known scalar types
  ConT name
    | isScalarType name -> return ScalarSchema

  -- Other named types - recurse
  ConT name -> generateSchemaWithMemo visited memo name

  -- Type variable - error
  VarT name -> fail $
    "generateSchema: Type variable '" ++ nameBase name ++
    "' found. Polymorphic fields are not supported."

  -- Type application we don't recognize
  AppT _ _ -> fail $
    "generateSchema: Unsupported type application: " ++ show ty ++
    ". Only List, Vector, and Maybe are supported as type constructors."

  -- Other type forms
  _ -> fail $ "generateSchema: Unsupported type: " ++ show ty

-- | Check if a type name is a known scalar type.
isScalarType :: Name -> Bool
isScalarType name = nameBase name `elem` scalarTypeNames

-- | List of scalar type names we recognize.
scalarTypeNames :: [String]
scalarTypeNames =
  [ "Text"
  , "String"
  , "Int"
  , "Integer"
  , "Double"
  , "Float"
  , "Bool"
  , "Scientific"
  , "Day"
  , "TimeOfDay"
  , "LocalTime"
  , "ZonedTime"
  , "UTCTime"
  , "Char"
  , "ByteString"
  ]
