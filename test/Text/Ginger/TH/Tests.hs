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
import Text.Ginger.TH.Validate (validatePath, validatePaths, formatValidationError, SchemaRegistry)

-- | All TH-related tests
thTests :: TestTree
thTests = testGroup "Template Haskell Type Checking"
  [ builtinTests
  , extractionTests
  , validationTests
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
      let path = AccessPath "name" [] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "valid nested field" $ do
      let profileSchema = RecordSchema [("bio", ScalarSchema)]
      let schema = RecordSchema [("profile", profileSchema)]
      let path = AccessPath "profile" [StaticKey "bio"] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "invalid field on record" $ do
      let schema = RecordSchema [("name", ScalarSchema)]
      let path = AccessPath "email" [] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just (FieldNotFound _ field) -> assertEqual "field name" "email" field
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "invalid nested field" $ do
      let schema = RecordSchema [("name", ScalarSchema)]
      let path = AccessPath "name" [StaticKey "foo"] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid (can't access field on scalar)"
        Just (AccessOnScalar _) -> return ()
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "dynamic key on record is error" $ do
      -- Access user[<dynamic>] where user is a record
      let schema = RecordSchema [("user", RecordSchema [("name", ScalarSchema)])]
      let path = AccessPath "user" [DynamicKey] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just (DynamicAccessNotAllowed _) -> return ()
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "sum type - field in all constructors" $ do
      let schema = SumSchema
            [ [("tag", ScalarSchema), ("name", ScalarSchema)]
            , [("tag", ScalarSchema), ("value", ScalarSchema)]
            ]
      let path = AccessPath "tag" [] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "sum type - field not in all constructors (root)" $ do
      -- At root level, missing field in some constructors returns FieldNotFound
      let schema = SumSchema
            [ [("tag", ScalarSchema), ("name", ScalarSchema)]
            , [("tag", ScalarSchema), ("value", ScalarSchema)]
            ]
      let path = AccessPath "name" [] dummyPos
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
      let path = AccessPath "content" [StaticKey "x"] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just (FieldNotInAllConstructors _ field) ->
          assertEqual "field name" "x" field
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "list index access is valid" $ do
      let schema = RecordSchema [("items", ListSchema ScalarSchema)]
      let path = AccessPath "items" [StaticKey "0"] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "list with nested record access" $ do
      let itemSchema = RecordSchema [("name", ScalarSchema)]
      let schema = RecordSchema [("items", ListSchema itemSchema)]
      let path = AccessPath "items" [StaticKey "0", StaticKey "name"] dummyPos
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
      let path = AccessPath "children" [StaticKey "0", StaticKey "value"] dummyPos
      assertEqual "should be valid" Nothing (validatePath registry treeSchema path)

  , testCase "recursive type nested access works" $ do
      -- Access tree.children.0.children.0.value (two levels deep)
      let treeSchema = RecordSchema
            [ ("value", ScalarSchema)
            , ("children", ListSchema (RecursiveRef "Tree"))
            ]
      let registry = Map.singleton "Tree" treeSchema
      let path = AccessPath "children" [StaticKey "0", StaticKey "children", StaticKey "0", StaticKey "value"] dummyPos
      assertEqual "should be valid" Nothing (validatePath registry treeSchema path)

  , testCase "error formatting includes location" $ do
      let schema = RecordSchema [("name", ScalarSchema)]
      let path = AccessPath "email" [] (newPos "test.html" 5 10)
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
      paths = [AccessPath name [] dummyPos | name <- fieldNames]
  in all (\p -> validatePath emptyRegistry schema p == Nothing) paths

-- | ScalarSchema rejects any non-empty path segment.
prop_scalarRejectsSegments :: NonEmptyList PathSegment -> Bool
prop_scalarRejectsSegments (NonEmpty segs) =
  let schema = RecordSchema [("x", ScalarSchema)]
      path = AccessPath "x" segs dummyPos
  in case validatePath emptyRegistry schema path of
       Just (AccessOnScalar _) -> True
       _ -> False

-- | ListSchema accepts any static key (for index access).
prop_listAcceptsAnyKey :: Text -> Bool
prop_listAcceptsAnyKey key =
  let schema = RecordSchema [("items", ListSchema ScalarSchema)]
      path = AccessPath "items" [StaticKey key] dummyPos
  in validatePath emptyRegistry schema path == Nothing

-- | DynamicKey on a RecordSchema always fails.
prop_dynamicKeyOnRecordFails :: NonEmptyList (Text, Schema) -> Bool
prop_dynamicKeyOnRecordFails (NonEmpty fields) =
  let schema = RecordSchema [("rec", RecordSchema fields)]
      path = AccessPath "rec" [DynamicKey] dummyPos
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

  shrink (AccessPath root segs pos) =
    [AccessPath root segs' pos | segs' <- shrink segs]

