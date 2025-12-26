{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Tests for the compile-time type-checked template system.
module Text.Ginger.TH.Tests
  ( thTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Set as Set
import Text.Parsec.Pos (newPos, SourcePos)

import Text.Ginger.AST
import Text.Ginger.Parse (parseGinger', mkParserOptions, ParserOptions(..))
import Text.Ginger.TH.Types
import Text.Ginger.TH.Builtins (isBuiltin, builtinNames)
import Text.Ginger.TH.Extract (extractFromTemplate, extractVariableAccesses)
import Text.Ginger.TH.Validate (validatePath, validatePaths, formatValidationError)

-- | All TH-related tests
thTests :: TestTree
thTests = testGroup "Template Haskell Type Checking"
  [ builtinTests
  , extractionTests
  , validationTests
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
      -- Note: our extraction is simplified and may not perfectly handle
      -- sequential scoping of set. This test documents current behavior.
      paths <- parseAndExtract "{% set x = value %}{{ x }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- 'value' is definitely free, 'x' behavior depends on implementation
      assertBool "should extract 'value'" $
        any (\p -> apRoot p == "value") userPaths

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

validationTests :: TestTree
validationTests = testGroup "Path Validation"
  [ testCase "valid field on record" $ do
      let schema = RecordSchema [("name", ScalarSchema), ("email", ScalarSchema)]
      let path = AccessPath "name" [] dummyPos
      assertEqual "should be valid" Nothing (validatePath schema path)

  , testCase "valid nested field" $ do
      let profileSchema = RecordSchema [("bio", ScalarSchema)]
      let schema = RecordSchema [("profile", profileSchema)]
      let path = AccessPath "profile" [StaticKey "bio"] dummyPos
      assertEqual "should be valid" Nothing (validatePath schema path)

  , testCase "invalid field on record" $ do
      let schema = RecordSchema [("name", ScalarSchema)]
      let path = AccessPath "email" [] dummyPos
      case validatePath schema path of
        Nothing -> assertFailure "should be invalid"
        Just (FieldNotFound _ field) -> assertEqual "field name" "email" field
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "invalid nested field" $ do
      let schema = RecordSchema [("name", ScalarSchema)]
      let path = AccessPath "name" [StaticKey "foo"] dummyPos
      case validatePath schema path of
        Nothing -> assertFailure "should be invalid (can't access field on scalar)"
        Just (AccessOnScalar _) -> return ()
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "dynamic key on record is error" $ do
      -- Access user[<dynamic>] where user is a record
      let schema = RecordSchema [("user", RecordSchema [("name", ScalarSchema)])]
      let path = AccessPath "user" [DynamicKey] dummyPos
      case validatePath schema path of
        Nothing -> assertFailure "should be invalid"
        Just (DynamicAccessNotAllowed _) -> return ()
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "sum type - field in all constructors" $ do
      let schema = SumSchema
            [ [("tag", ScalarSchema), ("name", ScalarSchema)]
            , [("tag", ScalarSchema), ("value", ScalarSchema)]
            ]
      let path = AccessPath "tag" [] dummyPos
      assertEqual "should be valid" Nothing (validatePath schema path)

  , testCase "sum type - field not in all constructors (root)" $ do
      -- At root level, missing field in some constructors returns FieldNotFound
      let schema = SumSchema
            [ [("tag", ScalarSchema), ("name", ScalarSchema)]
            , [("tag", ScalarSchema), ("value", ScalarSchema)]
            ]
      let path = AccessPath "name" [] dummyPos
      case validatePath schema path of
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
      case validatePath schema path of
        Nothing -> assertFailure "should be invalid"
        Just (FieldNotInAllConstructors _ field) ->
          assertEqual "field name" "x" field
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "list index access is valid" $ do
      let schema = RecordSchema [("items", ListSchema ScalarSchema)]
      let path = AccessPath "items" [StaticKey "0"] dummyPos
      assertEqual "should be valid" Nothing (validatePath schema path)

  , testCase "list with nested record access" $ do
      let itemSchema = RecordSchema [("name", ScalarSchema)]
      let schema = RecordSchema [("items", ListSchema itemSchema)]
      let path = AccessPath "items" [StaticKey "0", StaticKey "name"] dummyPos
      assertEqual "should be valid" Nothing (validatePath schema path)

  , testCase "error formatting includes location" $ do
      let schema = RecordSchema [("name", ScalarSchema)]
      let path = AccessPath "email" [] (newPos "test.html" 5 10)
      case validatePath schema path of
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

