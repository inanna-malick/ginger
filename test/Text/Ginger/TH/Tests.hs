{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Tests for the compile-time type-checked template system.
module Text.Ginger.TH.Tests
  ( thTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Parsec.Pos (newPos, SourcePos)

import Text.Ginger.AST
import Text.Ginger.Parse (parseGinger', mkParserOptions, ParserOptions(..))
import Text.Ginger.TH.Types
import Text.Ginger.TH.Builtins (isBuiltin, builtinNames)
import Text.Ginger.TH.Extract (extractFromTemplate, extractVariableAccesses)
import Text.Ginger.TH.Schema (generateSchema, SchemaRegistry)
import Text.Ginger.TH.Validate (validatePath, validatePaths, formatValidationError)

-- Import test types from separate module (required for TH to see them)
import Text.Ginger.TH.TestTypes

-- | All TH-related tests
thTests :: TestTree
thTests = testGroup "Template Haskell Type Checking"
  [ builtinTests
  , extractionTests
  , validationTests
  , schemaGenerationTests
  , narrowingTests
  , endToEndTests
  , propertyTests
  ]

--------------------------------------------------------------------------------
-- Builtin Tests
--------------------------------------------------------------------------------

builtinTests :: TestTree
builtinTests = testGroup "Builtins"
  [ testCase "common functions are builtins" $ do
      assertBool "length is builtin" (isBuiltin "length")
      assertBool "upper is builtin" (isBuiltin "upper")
      assertBool "filter is builtin" (isBuiltin "filter")
      assertBool "map is builtin" (isBuiltin "map")

  , testCase "loop is not a builtin (it's context-dependent)" $ do
      assertBool "loop is not a global builtin" (not $ isBuiltin "loop")

  , testCase "user variables are not builtins" $ do
      assertBool "userName is not builtin" (not $ isBuiltin "userName")
      assertBool "items is not builtin" (not $ isBuiltin "items")

  , testCase "boolean literals are builtins" $ do
      assertBool "true is builtin" (isBuiltin "true")
      assertBool "false is builtin" (isBuiltin "false")
      assertBool "null is builtin" (isBuiltin "null")
  ]

--------------------------------------------------------------------------------
-- Extraction Tests
--------------------------------------------------------------------------------

extractionTests :: TestTree
extractionTests = testGroup "Variable Extraction"
  [ testCase "simple variable" $ do
      paths <- parseAndExtract "{{ name }}"
      assertEqual "should extract one path" 1 (length paths)
      assertEqual "root should be 'name'" "name" (apRoot $ head paths)
      assertEqual "path should be empty" [] (apPath $ head paths)

  , testCase "nested field access" $ do
      paths <- parseAndExtract "{{ user.profile.name }}"
      assertEqual "should extract one path" 1 (length paths)
      assertEqual "root should be 'user'" "user" (apRoot $ head paths)
      assertEqual "path should have two segments"
        [StaticKey "profile", StaticKey "name"]
        (apPath $ head paths)

  , testCase "multiple variables" $ do
      paths <- parseAndExtract "{{ name }} - {{ email }}"
      assertEqual "should extract two paths" 2 (length paths)
      let roots = Set.fromList $ map apRoot paths
      assertBool "should have 'name'" (Set.member "name" roots)
      assertBool "should have 'email'" (Set.member "email" roots)

  , testCase "builtin functions are excluded" $ do
      paths <- parseAndExtract "{{ items | length }}"
      -- 'items' should be extracted, 'length' should not (it's a builtin)
      let userPaths = filter (not . isBuiltin . apRoot) paths
      assertEqual "should extract one user path" 1 (length userPaths)
      assertEqual "root should be 'items'" "items" (apRoot $ head userPaths)

  , testCase "for loop binds variables" $ do
      paths <- parseAndExtract "{% for item in items %}{{ item.name }}{% endfor %}"
      -- 'items' is free, 'item' is bound by the for loop
      let userPaths = filter (not . isBuiltin . apRoot) paths
      assertEqual "should extract one path (items)" 1 (length userPaths)
      assertEqual "root should be 'items'" "items" (apRoot $ head userPaths)

  , testCase "for loop binds loop variable" $ do
      paths <- parseAndExtract "{% for x in items %}{{ loop.index }}{% endfor %}"
      -- 'items' is free, 'loop' is bound by the for loop
      let userPaths = filter (not . isBuiltin . apRoot) paths
      assertEqual "should extract one path (items)" 1 (length userPaths)

  , testCase "set binds variable in subsequent statements" $ do
      -- 'value' is free, 'x' is bound by set and available in subsequent statements
      paths <- parseAndExtract "{% set x = value %}{{ x }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only 'value' should be extracted, 'x' is bound
      assertEqual "should extract only 'value'" 1 (length userPaths)
      assertEqual "should extract 'value'" "value" (apRoot $ head userPaths)

  , testCase "set binds variable for nested access" $ do
      -- Verify set binding works for nested access too
      paths <- parseAndExtract "{% set user = data %}{{ user.name }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only 'data' should be extracted, 'user.name' is bound
      assertEqual "should extract only 'data'" 1 (length userPaths)
      assertEqual "should extract 'data'" "data" (apRoot $ head userPaths)

  , testCase "set variable used before definition is free" $ do
      -- If a variable is used before set, it's still free at that point
      paths <- parseAndExtract "{{ x }}{% set x = 1 %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- 'x' used before set should be extracted
      assertEqual "should extract 'x'" 1 (length userPaths)
      assertEqual "should be 'x'" "x" (apRoot $ head userPaths)

  , testCase "lambda binds parameters" $ do
      paths <- parseAndExtract "{{ items | map((x) -> x.name) }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- 'items' is free, 'x' is bound by lambda
      assertEqual "should extract one user path" 1 (length userPaths)
      assertEqual "root should be 'items'" "items" (apRoot $ head userPaths)

  , testCase "dynamic key access is marked" $ do
      paths <- parseAndExtract "{{ data[key] }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Should have 'data' with DynamicKey, and 'key' as separate access
      let dataPaths = filter (\p -> apRoot p == "data") userPaths
      assertEqual "should have data path" 1 (length dataPaths)
      assertEqual "data path should have DynamicKey"
        [DynamicKey]
        (apPath $ head dataPaths)

  , testCase "bracket with string literal is static" $ do
      paths <- parseAndExtract "{{ data[\"field\"] }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      assertEqual "should extract one path" 1 (length userPaths)
      assertEqual "path should have StaticKey"
        [StaticKey "field"]
        (apPath $ head userPaths)

  , testCase "macro parameters are bound" $ do
      paths <- parseAndExtract "{% macro greet(name) %}Hello {{ name }}{% endmacro %}"
      -- 'name' is bound by the macro definition
      let userPaths = filter (not . isBuiltin . apRoot) paths
      assertEqual "should extract no user paths" 0 (length userPaths)

  , testCase "catch binds exception variable" $ do
      paths <- parseAndExtract "{% try %}{{ risky }}{% catch * as e %}{{ e }}{% endtry %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- 'risky' is free, 'e' is bound by catch
      assertEqual "should extract one user path" 1 (length userPaths)
      assertEqual "root should be 'risky'" "risky" (apRoot $ head userPaths)
  ]

--------------------------------------------------------------------------------
-- Validation Tests
--------------------------------------------------------------------------------

-- | Empty registry for simple tests
emptyRegistry :: SchemaRegistry
emptyRegistry = Map.empty

validationTests :: TestTree
validationTests = testGroup "Path Validation"
  [ testCase "valid field on record" $ do
      let schema = RecordSchema [("name", ScalarSchema), ("email", ScalarSchema)]
      let path = mkAccessPath "name" [] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "valid nested field" $ do
      let profileSchema = RecordSchema [("bio", ScalarSchema)]
      let schema = RecordSchema [("profile", profileSchema)]
      let path = mkAccessPath "profile" [StaticKey "bio"] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "invalid field on record" $ do
      let schema = RecordSchema [("name", ScalarSchema)]
      let path = mkAccessPath "email" [] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just (FieldNotFound _ field) -> assertEqual "field name" "email" field
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "invalid nested field" $ do
      let schema = RecordSchema [("name", ScalarSchema)]
      let path = mkAccessPath "name" [StaticKey "foo"] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid (can't access field on scalar)"
        Just (AccessOnScalar _) -> return ()
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "dynamic key on record is error" $ do
      -- Access user[<dynamic>] where user is a record
      let schema = RecordSchema [("user", RecordSchema [("name", ScalarSchema)])]
      let path = mkAccessPath "user" [DynamicKey] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just (DynamicAccessNotAllowed _) -> return ()
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "sum type - field in all constructors" $ do
      let schema = SumSchema
            [ [("tag", ScalarSchema), ("name", ScalarSchema)]
            , [("tag", ScalarSchema), ("value", ScalarSchema)]
            ]
      let path = mkAccessPath "tag" [] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "sum type - field not in all constructors (root)" $ do
      -- At root level, missing field in some constructors returns FieldNotFound
      let schema = SumSchema
            [ [("tag", ScalarSchema), ("name", ScalarSchema)]
            , [("tag", ScalarSchema), ("value", ScalarSchema)]
            ]
      let path = mkAccessPath "name" [] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just (FieldNotFound _ field) ->
          assertEqual "field name" "name" field
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "sum type - field not in all constructors (nested)" $ do
      -- Nested access returns FieldNotInAllConstructors
      let innerSum = SumSchema
            [ [("x", ScalarSchema)]
            , [("y", ScalarSchema)]
            ]
      let schema = RecordSchema [("content", innerSum)]
      let path = mkAccessPath "content" [StaticKey "x"] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just (FieldNotInAllConstructors _ field) ->
          assertEqual "field name" "x" field
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "list index access is valid" $ do
      let schema = RecordSchema [("items", ListSchema ScalarSchema)]
      let path = mkAccessPath "items" [StaticKey "0"] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "list with nested record access" $ do
      let itemSchema = RecordSchema [("name", ScalarSchema)]
      let schema = RecordSchema [("items", ListSchema itemSchema)]
      let path = mkAccessPath "items" [StaticKey "0", StaticKey "name"] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "recursive type reference is resolved" $ do
      -- Simulate a recursive type: Tree = Node { value :: Int, children :: [Tree] }
      -- The RecursiveRef "Tree" should resolve to the Tree schema
      let treeSchema = RecordSchema
            [ ("value", ScalarSchema)
            , ("children", ListSchema (RecursiveRef "Tree"))
            ]
      let registry = Map.singleton "Tree" treeSchema
      -- Access tree.children.0.value should be valid
      let path = mkAccessPath "children" [StaticKey "0", StaticKey "value"] dummyPos
      assertEqual "should be valid" Nothing (validatePath registry treeSchema path)

  , testCase "recursive type nested access works" $ do
      -- Access tree.children.0.children.0.value (two levels deep)
      let treeSchema = RecordSchema
            [ ("value", ScalarSchema)
            , ("children", ListSchema (RecursiveRef "Tree"))
            ]
      let registry = Map.singleton "Tree" treeSchema
      let path = mkAccessPath "children" [StaticKey "0", StaticKey "children", StaticKey "0", StaticKey "value"] dummyPos
      assertEqual "should be valid" Nothing (validatePath registry treeSchema path)

  , testCase "error formatting includes location" $ do
      let schema = RecordSchema [("name", ScalarSchema)]
      let path = AccessPath "email" [] (newPos "test.html" 5 10) Set.empty
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just err -> do
          let formatted = formatValidationError err
          assertBool "should contain filename" ("test.html" `isInfixOf` formatted)
          assertBool "should contain line number" ("5" `isInfixOf` formatted)
  ]
  where
    isInfixOf needle haystack = needle `elem` words haystack ||
      any (needle `isPrefixOf`) (tails haystack)
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Parse a template string and extract variable accesses.
parseAndExtract :: String -> IO [AccessPath]
parseAndExtract src = do
  let opts = (mkParserOptions nullResolver) { poSourceName = Just "test" }
  result <- parseGinger' opts src
  case result of
    Left err -> error $ "Parse error: " ++ show err
    Right tpl -> return $ extractFromTemplate tpl

-- | Null resolver for testing (no includes).
nullResolver :: Monad m => String -> m (Maybe String)
nullResolver _ = return Nothing

-- | Dummy source position for testing.
dummyPos :: SourcePos
dummyPos = newPos "test" 1 1

-- | Create an AccessPath with no narrowing context (for tests).
mkAccessPath :: Text -> [PathSegment] -> SourcePos -> AccessPath
mkAccessPath root segs pos = AccessPath root segs pos Set.empty

--------------------------------------------------------------------------------
-- Schema Generation Tests (using compile-time TH)
--------------------------------------------------------------------------------

-- Generate schemas at compile time
simpleRecordSchema :: (Schema, SchemaRegistry)
simpleRecordSchema = $( do
  (s, r) <- generateSchema ''SimpleRecord
  [| (s, r) |]
 )

nestedRecordSchema :: (Schema, SchemaRegistry)
nestedRecordSchema = $( do
  (s, r) <- generateSchema ''NestedRecord
  [| (s, r) |]
 )

contentTypeSchema :: (Schema, SchemaRegistry)
contentTypeSchema = $( do
  (s, r) <- generateSchema ''ContentType
  [| (s, r) |]
 )

animalSchema :: (Schema, SchemaRegistry)
animalSchema = $( do
  (s, r) <- generateSchema ''Animal
  [| (s, r) |]
 )

treeSchema :: (Schema, SchemaRegistry)
treeSchema = $( do
  (s, r) <- generateSchema ''Tree
  [| (s, r) |]
 )

withListSchema :: (Schema, SchemaRegistry)
withListSchema = $( do
  (s, r) <- generateSchema ''WithList
  [| (s, r) |]
 )

withMaybeSchema :: (Schema, SchemaRegistry)
withMaybeSchema = $( do
  (s, r) <- generateSchema ''WithMaybe
  [| (s, r) |]
 )

withVectorSchema :: (Schema, SchemaRegistry)
withVectorSchema = $( do
  (s, r) <- generateSchema ''WithVector
  [| (s, r) |]
 )

userIdSchema :: (Schema, SchemaRegistry)
userIdSchema = $( do
  (s, r) <- generateSchema ''UserId
  [| (s, r) |]
 )

withTypeSynonymSchema :: (Schema, SchemaRegistry)
withTypeSynonymSchema = $( do
  (s, r) <- generateSchema ''WithTypeSynonym
  [| (s, r) |]
 )

-- Schemas for opaque type tests
withOpaqueFieldSchema :: (Schema, SchemaRegistry)
withOpaqueFieldSchema = $( do
  (s, r) <- generateSchema ''WithOpaqueField
  [| (s, r) |]
 )

complexWithOpaqueSchema :: (Schema, SchemaRegistry)
complexWithOpaqueSchema = $( do
  (s, r) <- generateSchema ''ComplexWithOpaque
  [| (s, r) |]
 )

mixedContentSchema :: (Schema, SchemaRegistry)
mixedContentSchema = $( do
  (s, r) <- generateSchema ''MixedContent
  [| (s, r) |]
 )

schemaGenerationTests :: TestTree
schemaGenerationTests = testGroup "Schema Generation"
  [ testCase "simple record generates RecordSchema" $ do
      let (schema, _) = simpleRecordSchema
      case schema of
        RecordSchema fields -> do
          assertEqual "should have 2 fields" 2 (length fields)
          assertBool "should have srName" $ any ((== "srName") . fst) fields
          assertBool "should have srAge" $ any ((== "srAge") . fst) fields
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "nested record generates nested schema" $ do
      let (schema, _) = nestedRecordSchema
      case schema of
        RecordSchema fields -> do
          assertEqual "should have 2 fields" 2 (length fields)
          case lookup "nrUser" fields of
            Just (RecordSchema innerFields) ->
              assertEqual "inner should have 2 fields" 2 (length innerFields)
            Just other -> assertFailure $ "expected RecordSchema for nrUser, got: " ++ show other
            Nothing -> assertFailure "missing nrUser field"
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "sum type generates SumSchema" $ do
      let (schema, _) = contentTypeSchema
      case schema of
        SumSchema constructors -> do
          assertEqual "should have 2 constructors" 2 (length constructors)
          -- TextContent has 1 field, ImageContent has 2
          let fieldCounts = map length constructors
          assertBool "field counts should be [1,2] or [2,1]" $
            fieldCounts == [1,2] || fieldCounts == [2,1]
        _ -> assertFailure $ "expected SumSchema, got: " ++ show schema

  , testCase "sum type with shared field" $ do
      let (schema, registry) = animalSchema
      case schema of
        SumSchema constructors -> do
          assertEqual "should have 2 constructors" 2 (length constructors)
          -- animalName should be in both constructors
          let hasAnimalName = all (any ((== "animalName") . fst)) constructors
          assertBool "animalName should be in all constructors" hasAnimalName
          -- Validate that animalName access is valid
          let path = mkAccessPath "animalName" [] dummyPos
          assertEqual "animalName should be accessible" Nothing $
            validatePath registry schema path
        _ -> assertFailure $ "expected SumSchema, got: " ++ show schema

  , testCase "recursive type generates RecursiveRef" $ do
      let (schema, registry) = treeSchema
      case schema of
        RecordSchema fields -> do
          assertBool "should have treeChildren" $ any ((== "treeChildren") . fst) fields
          case lookup "treeChildren" fields of
            Just (ListSchema (RecursiveRef refName)) ->
              assertEqual "should reference Tree" "Tree" refName
            Just other -> assertFailure $ "expected ListSchema with RecursiveRef, got: " ++ show other
            Nothing -> assertFailure "missing treeChildren field"
          -- Validate deep access works
          let path = mkAccessPath "treeChildren" [StaticKey "0", StaticKey "treeValue"] dummyPos
          assertEqual "nested access should work" Nothing $
            validatePath registry schema path
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "list field generates ListSchema" $ do
      let (schema, _) = withListSchema
      case schema of
        RecordSchema fields ->
          case lookup "wlItems" fields of
            Just (ListSchema ScalarSchema) -> return ()
            Just other -> assertFailure $ "expected ListSchema ScalarSchema, got: " ++ show other
            Nothing -> assertFailure "missing wlItems field"
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "Maybe field treated as inner type" $ do
      let (schema, _) = withMaybeSchema
      case schema of
        RecordSchema fields -> do
          case lookup "wmOptional" fields of
            Just ScalarSchema -> return ()  -- Maybe Text -> Text -> ScalarSchema
            Just other -> assertFailure $ "expected ScalarSchema for Maybe, got: " ++ show other
            Nothing -> assertFailure "missing wmOptional field"
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "Vector field generates ListSchema" $ do
      let (schema, _) = withVectorSchema
      case schema of
        RecordSchema fields ->
          case lookup "wvItems" fields of
            Just (ListSchema ScalarSchema) -> return ()
            Just other -> assertFailure $ "expected ListSchema ScalarSchema, got: " ++ show other
            Nothing -> assertFailure "missing wvItems field"
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "newtype generates schema for wrapped type" $ do
      let (schema, _) = userIdSchema
      case schema of
        RecordSchema fields -> do
          assertEqual "should have 1 field" 1 (length fields)
          assertBool "should have unUserId" $ any ((== "unUserId") . fst) fields
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "type synonym is transparent" $ do
      let (schema, _) = withTypeSynonymSchema
      case schema of
        RecordSchema fields -> do
          case lookup "wtsEmail" fields of
            Just ScalarSchema -> return ()  -- Email = Text -> ScalarSchema
            Just other -> assertFailure $ "expected ScalarSchema for type synonym, got: " ++ show other
            Nothing -> assertFailure "missing wtsEmail field"
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  -- Opaque type tests: types that schema generation doesn't fully understand
  -- should become OpaqueSchema rather than causing compile failure

  , testCase "record with opaque field generates schema (not accessed)" $ do
      -- WithOpaqueField has a Status field which is a non-record sum type
      -- Schema generation should succeed, and accessing wofName should work
      let (schema, registry) = withOpaqueFieldSchema
      case schema of
        RecordSchema fields -> do
          assertBool "should have wofName" $ any ((== "wofName") . fst) fields
          assertBool "should have wofStatus" $ any ((== "wofStatus") . fst) fields
          -- wofStatus should be opaque (SumSchema with empty field lists)
          case lookup "wofStatus" fields of
            Just (SumSchema fieldSets) -> do
              -- Non-record sum types have constructors with no named fields
              assertBool "opaque sum type has empty field sets" $
                all null fieldSets
            Just other -> assertFailure $ "expected SumSchema for Status, got: " ++ show other
            Nothing -> assertFailure "missing wofStatus field"
          -- Accessing wofName should succeed
          let namePath = mkAccessPath "wofName" [] dummyPos
          assertEqual "wofName access should be valid" Nothing $
            validatePath registry schema namePath
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "accessing opaque field fails at validation" $ do
      -- Trying to access status.something should fail
      let (schema, registry) = withOpaqueFieldSchema
      let statusPath = mkAccessPath "wofStatus" [StaticKey "payload"] dummyPos
      case validatePath registry schema statusPath of
        Just (FieldNotInAllConstructors _ _) -> return ()  -- Expected: field not in all constructors
        Just err -> return ()  -- Any validation error is acceptable
        Nothing -> assertFailure "should fail when accessing into opaque type"

  , testCase "non-record single constructor becomes opaque" $ do
      -- ComplexWithOpaque has Point fields which are positional
      let (schema, registry) = complexWithOpaqueSchema
      case schema of
        RecordSchema fields -> do
          -- cwoTitle should work
          let titlePath = mkAccessPath "cwoTitle" [] dummyPos
          assertEqual "cwoTitle should be valid" Nothing $
            validatePath registry schema titlePath
          -- cwoPoint should be opaque
          case lookup "cwoPoint" fields of
            Just (OpaqueSchema reason) ->
              assertBool "should mention non-record" $ "non-record" `Text.isInfixOf` reason
            Just other -> assertFailure $ "expected OpaqueSchema for Point, got: " ++ show other
            Nothing -> assertFailure "missing cwoPoint field"
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "mixed sum type (record + nullary) works" $ do
      -- MixedContent has TextBlock/ImageBlock with records, Divider without
      let (schema, registry) = mixedContentSchema
      case schema of
        SumSchema constructors -> do
          assertEqual "should have 3 constructors" 3 (length constructors)
          -- TextBlock and ImageBlock have mcText/mcUrl, Divider has nothing
          let fieldCounts = map length constructors
          assertBool "should have 0, 1, and 1 fields" $
            sort fieldCounts == [0, 1, 1]
        _ -> assertFailure $ "expected SumSchema, got: " ++ show schema
  ]

--------------------------------------------------------------------------------
-- Narrowing Tests (is defined guards)
--------------------------------------------------------------------------------

narrowingTests :: TestTree
narrowingTests = testGroup "Narrowing (is defined guards)"
  [ testCase "guarded sum type field access" $ do
      -- {% if ctBody is defined %}{{ ctBody }}{% endif %}
      -- Should work - ctBody only in TextContent but access is guarded
      let (schema, registry) = contentTypeSchema
      paths <- parseAndExtract "{% if ctBody is defined %}{{ ctBody }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- ctBody is extracted only from the body (condition is just a query)
      let ctBodyPaths = filter (\p -> apRoot p == "ctBody") userPaths
      assertEqual "should have 1 ctBody path (body only)" 1 (length ctBodyPaths)
      -- The body access should be narrowed
      let narrowed = apNarrowed (head ctBodyPaths)
      assertBool "ctBody should be narrowed" (not $ Set.null narrowed)
      -- Validation should succeed because the body access is guarded
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors (guarded access)" [] errors

  , testCase "unguarded sum type field rejected" $ do
      -- {{ ctBody }} alone (unguarded)
      let (schema, registry) = contentTypeSchema
      paths <- parseAndExtract "{{ ctBody }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have 1 error" 1 (length errors)

  , testCase "is undefined narrows in else branch" $ do
      -- {% if x is undefined %}...{% else %}{{ x }}{% endif %}
      -- x in else branch should be narrowed
      paths <- parseAndExtract "{% if x is undefined %}no{% else %}{{ x }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only the x in the else branch ({{ x }}) is extracted (condition is just a query)
      let xPaths = filter (\p -> apRoot p == "x") userPaths
      assertEqual "should have 1 x path (else branch only)" 1 (length xPaths)
      -- The else branch x should be narrowed (x is undefined -> x is defined in else)
      let narrowed = apNarrowed (head xPaths)
      assertBool "x in else branch should be narrowed" (not $ Set.null narrowed)

  , testCase "and narrows both sides" $ do
      -- {% if a is defined and b is defined %}{{ a }}{{ b }}{% endif %}
      paths <- parseAndExtract "{% if a is defined and b is defined %}{{ a }}{{ b }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only body accesses extracted (a and b from body, not from condition)
      assertEqual "should have 2 paths (body only)" 2 (length userPaths)
      -- Both a and b in the body should be narrowed
      let aPaths = filter (\p -> apRoot p == "a") userPaths
      let bPaths = filter (\p -> apRoot p == "b") userPaths
      assertBool "a should be narrowed" (all (not . Set.null . apNarrowed) aPaths)
      assertBool "b should be narrowed" (all (not . Set.null . apNarrowed) bPaths)

  , testCase "or narrows neither" $ do
      -- {% if a is defined or b is defined %}{{ a }}{% endif %}
      -- a in body should NOT be narrowed (could be undefined)
      paths <- parseAndExtract "{% if a is defined or b is defined %}{{ a }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only body access extracted (condition is query, not use)
      assertEqual "should have 1 path (body only)" 1 (length userPaths)
      -- The body 'a' should not be narrowed (or is conservative)
      let narrowed = apNarrowed (head userPaths)
      assertBool "a should not be narrowed in or condition" (Set.null narrowed)

  , testCase "nested if accumulates narrowing" $ do
      -- {% if x is defined %}{% if y is defined %}{{ x }}{{ y }}{% endif %}{% endif %}
      paths <- parseAndExtract "{% if x is defined %}{% if y is defined %}{{ x }}{{ y }}{% endif %}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only body accesses extracted
      assertEqual "should have 2 paths (body only)" 2 (length userPaths)
      -- x and y in innermost block should both be narrowed
      let xPaths = filter (\p -> apRoot p == "x") userPaths
      let yPaths = filter (\p -> apRoot p == "y") userPaths
      assertBool "x should be narrowed in nested block" (all (not . Set.null . apNarrowed) xPaths)
      assertBool "y should be narrowed in nested block" (all (not . Set.null . apNarrowed) yPaths)

  , testCase "ternary expression narrows" $ do
      -- {{ x.field is defined ? x.field : "default" }}
      paths <- parseAndExtract "{{ x.field is defined ? x.field : \"default\" }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only true branch access extracted (condition is query)
      assertEqual "should have 1 path (true branch only)" 1 (length userPaths)
      -- The true branch x.field should be narrowed
      let narrowed = apNarrowed (head userPaths)
      assertBool "x.field in ternary true branch should be narrowed" (not $ Set.null narrowed)

  , testCase "not inverts narrowing" $ do
      -- {% if not (x is undefined) %}{{ x }}{% endif %}
      -- same as x is defined
      paths <- parseAndExtract "{% if not (x is undefined) %}{{ x }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- The body 'x' should be narrowed
      let xPaths = filter (\p -> apRoot p == "x") userPaths
      assertBool "should have at least one x path" (not $ null xPaths)
      -- At least one x should be narrowed (the one in the body)
      let hasNarrowed = any (not . Set.null . apNarrowed) xPaths
      assertBool "x should be narrowed (not undefined = defined)" hasNarrowed
  ]

--------------------------------------------------------------------------------
-- End-to-End Tests (template + type validation)
--------------------------------------------------------------------------------

endToEndTests :: TestTree
endToEndTests = testGroup "End-to-End Validation"
  [ testCase "valid template against SimpleRecord" $ do
      let (schema, registry) = simpleRecordSchema
      paths <- parseAndExtract "{{ srName }} is {{ srAge }} years old"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors

  , testCase "invalid field on SimpleRecord" $ do
      let (schema, registry) = simpleRecordSchema
      paths <- parseAndExtract "{{ srName }} {{ invalidField }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have 1 error" 1 (length errors)
      case head errors of
        FieldNotFound _ field -> assertEqual "should be invalidField" "invalidField" field
        other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "nested access on NestedRecord" $ do
      let (schema, registry) = nestedRecordSchema
      paths <- parseAndExtract "{{ nrUser.srName }} active: {{ nrActive }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors

  , testCase "sum type shared field access" $ do
      let (schema, registry) = animalSchema
      paths <- parseAndExtract "{{ animalName }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors (field in all constructors)" [] errors

  , testCase "sum type non-shared field rejected" $ do
      let (schema, registry) = animalSchema
      paths <- parseAndExtract "{{ animalBreed }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have 1 error" 1 (length errors)

  , testCase "recursive type deep access" $ do
      let (schema, registry) = treeSchema
      -- Use bracket notation for list indexing
      paths <- parseAndExtract "{{ treeChildren[0].treeChildren[0].treeValue }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors

  , testCase "list index access" $ do
      let (schema, registry) = withListSchema
      -- Use bracket notation for list indexing
      paths <- parseAndExtract "{{ wlItems[0] }} count: {{ wlCount }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors

  , testCase "for loop with type checking" $ do
      let (schema, registry) = withListSchema
      paths <- parseAndExtract "{% for item in wlItems %}{{ item }}{% endfor %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only wlItems should be extracted (item is bound by for loop)
      assertEqual "should have 1 user path" 1 (length userPaths)
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors

  , testCase "set binding with type checking" $ do
      let (schema, registry) = simpleRecordSchema
      paths <- parseAndExtract "{% set fullName = srName %}{{ fullName }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only srName should be extracted (fullName is bound by set)
      assertEqual "should have 1 user path" 1 (length userPaths)
      assertEqual "should be srName" "srName" (apRoot $ head userPaths)
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors
  ]

--------------------------------------------------------------------------------
-- Property Tests
--------------------------------------------------------------------------------

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ testGroup "Schema Validation"
      [ testProperty "record fields are findable" prop_recordFieldsFindable
      , testProperty "scalar rejects all path segments" prop_scalarRejectsSegments
      , testProperty "list accepts any key" prop_listAcceptsAnyKey
      , testProperty "dynamic key on record fails" prop_dynamicKeyOnRecordFails
      , testProperty "validation is deterministic" prop_validationDeterministic
      ]
  , testGroup "Builtins"
      [ testProperty "builtin filtering is idempotent" prop_builtinFilterIdempotent
      , testProperty "all builtins are recognized" prop_allBuiltinsRecognized
      ]
  ]

-- | All fields listed in a RecordSchema can be accessed.
prop_recordFieldsFindable :: NonEmptyList (Text, Schema) -> Bool
prop_recordFieldsFindable (NonEmpty fields) =
  let schema = RecordSchema fields
      fieldNames = map fst fields
      paths = [mkAccessPath name [] dummyPos | name <- fieldNames]
  in all (\p -> validatePath emptyRegistry schema p == Nothing) paths

-- | ScalarSchema rejects any non-empty path segment.
prop_scalarRejectsSegments :: NonEmptyList PathSegment -> Bool
prop_scalarRejectsSegments (NonEmpty segs) =
  let schema = RecordSchema [("x", ScalarSchema)]
      path = mkAccessPath "x" segs dummyPos
  in case validatePath emptyRegistry schema path of
       Just (AccessOnScalar _) -> True
       _ -> False

-- | ListSchema accepts any static key (for index access).
prop_listAcceptsAnyKey :: Text -> Bool
prop_listAcceptsAnyKey key =
  let schema = RecordSchema [("items", ListSchema ScalarSchema)]
      path = mkAccessPath "items" [StaticKey key] dummyPos
  in validatePath emptyRegistry schema path == Nothing

-- | DynamicKey on a RecordSchema always fails.
prop_dynamicKeyOnRecordFails :: NonEmptyList (Text, Schema) -> Bool
prop_dynamicKeyOnRecordFails (NonEmpty fields) =
  let schema = RecordSchema [("rec", RecordSchema fields)]
      path = mkAccessPath "rec" [DynamicKey] dummyPos
  in case validatePath emptyRegistry schema path of
       Just (DynamicAccessNotAllowed _) -> True
       _ -> False

-- | Validation of the same path against same schema is deterministic.
prop_validationDeterministic :: Schema -> AccessPath -> Bool
prop_validationDeterministic schema path =
  validatePath emptyRegistry schema path == validatePath emptyRegistry schema path

-- | Filtering builtins twice gives same result as once.
prop_builtinFilterIdempotent :: [Text] -> Bool
prop_builtinFilterIdempotent names =
  let filtered = filter (not . isBuiltin) names
      filteredAgain = filter (not . isBuiltin) filtered
  in filtered == filteredAgain

-- | All names in builtinNames are recognized by isBuiltin.
prop_allBuiltinsRecognized :: Property
prop_allBuiltinsRecognized =
  forAll (elements $ Set.toList builtinNames) isBuiltin

--------------------------------------------------------------------------------
-- Arbitrary Instances
--------------------------------------------------------------------------------

instance Arbitrary Text where
  arbitrary = Text.pack <$> listOf (elements ['a'..'z'])
  shrink t = Text.pack <$> shrink (Text.unpack t)

instance Arbitrary PathSegment where
  arbitrary = oneof
    [ StaticKey <$> arbitrary
    , pure DynamicKey
    ]
  shrink (StaticKey k) = StaticKey <$> shrink k
  shrink DynamicKey = []

instance Arbitrary Schema where
  arbitrary = sized genSchema
    where
      genSchema 0 = pure ScalarSchema
      genSchema n = oneof
        [ pure ScalarSchema
        , RecordSchema <$> resize 3 (listOf ((,) <$> fieldName <*> genSchema (n `div` 2)))
        , ListSchema <$> genSchema (n - 1)
        , SumSchema <$> resize 2 (listOf1 (resize 3 (listOf ((,) <$> fieldName <*> genSchema (n `div` 2)))))
        ]
      fieldName = Text.pack <$> listOf1 (elements ['a'..'z'])

  shrink ScalarSchema = []
  shrink (RecordSchema fields) = ScalarSchema : [RecordSchema fs | fs <- shrink fields]
  shrink (ListSchema s) = ScalarSchema : s : [ListSchema s' | s' <- shrink s]
  shrink (SumSchema cs) = ScalarSchema : [SumSchema cs' | cs' <- shrink cs, not (null cs')]

instance Arbitrary AccessPath where
  arbitrary = AccessPath
    <$> (Text.pack <$> listOf1 (elements ['a'..'z']))
    <*> resize 3 (listOf arbitrary)
    <*> pure dummyPos
    <*> pure Set.empty

  shrink (AccessPath root segs pos narrowed) =
    [AccessPath root segs' pos narrowed | segs' <- shrink segs]

