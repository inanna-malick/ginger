{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Tests for filter results in expressions (comparisons, ternary, etc.)
--
-- These tests verify that filter outputs can be used in boolean/comparison
-- contexts, addressing Jinja2 compatibility gaps.
module Text.Ginger.ExpressionTests
    ( expressionTests
    ) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.IORef
import Control.Exception (throw, catch)

import Text.Ginger
import Text.Ginger.Html

expressionTests :: TestTree
expressionTests = testGroup "Expression Tests"
    [ lengthInExpressionsTests
    , ternaryWithComparisonsTests
    , pluralizePatternTests
    , truncateFilterTests
    ]

-- | Tests for using |length filter results in comparisons
-- Addresses: "{% if items|length > 3 %}...{% endif %}"
lengthInExpressionsTests :: TestTree
lengthInExpressionsTests = testGroup "Length filter in expressions"
    [ testCase "length > comparison (true)" $
        mkTest "{% if items|length > 3 %}many{% else %}few{% endif %}"
               [("items", toGVal ([1,2,3,4,5] :: [Int]))]
               "many"
    , testCase "length > comparison (false)" $
        mkTest "{% if items|length > 3 %}many{% else %}few{% endif %}"
               [("items", toGVal ([1,2] :: [Int]))]
               "few"
    , testCase "length == comparison" $
        mkTest "{% if items|length == 3 %}three{% else %}other{% endif %}"
               [("items", toGVal ([1,2,3] :: [Int]))]
               "three"
    , testCase "length != comparison" $
        mkTest "{% if items|length != 1 %}plural{% else %}singular{% endif %}"
               [("items", toGVal ([1,2,3] :: [Int]))]
               "plural"
    , testCase "length < comparison" $
        mkTest "{% if items|length < 5 %}small{% else %}large{% endif %}"
               [("items", toGVal ([1,2,3] :: [Int]))]
               "small"
    , testCase "length >= comparison" $
        mkTest "{% if items|length >= 3 %}enough{% else %}more{% endif %}"
               [("items", toGVal ([1,2,3] :: [Int]))]
               "enough"
    , testCase "length in arithmetic" $
        mkTest "{{ items|length + 10 }}"
               [("items", toGVal ([1,2,3] :: [Int]))]
               "13"
    , testCase "string length in comparison" $
        mkTest "{% if name|length > 5 %}long{% else %}short{% endif %}"
               [("name", toGVal ("Alexander" :: Text))]
               "long"
    , testCase "dict length in comparison" $
        mkTest "{% if data|length == 2 %}pair{% else %}other{% endif %}"
               [("data", toGVal [("a" :: Text, 1 :: Int), ("b", 2)])]
               "pair"
    ]

-- | Tests for ternary expressions with comparisons
ternaryWithComparisonsTests :: TestTree
ternaryWithComparisonsTests = testGroup "Ternary with comparisons"
    [ testCase "Python-style ternary with ==" $
        mkTest "{{ \"one\" if count == 1 else \"many\" }}"
               [("count", toGVal (1 :: Int))]
               "one"
    , testCase "Python-style ternary with !=" $
        mkTest "{{ \"plural\" if count != 1 else \"singular\" }}"
               [("count", toGVal (5 :: Int))]
               "plural"
    , testCase "Python-style ternary with >" $
        mkTest "{{ \"big\" if n > 10 else \"small\" }}"
               [("n", toGVal (15 :: Int))]
               "big"
    , testCase "C-style ternary with ==" $
        mkTest "{{ count == 1 ? \"one\" : \"many\" }}"
               [("count", toGVal (1 :: Int))]
               "one"
    , testCase "C-style ternary with !=" $
        mkTest "{{ count != 1 ? \"plural\" : \"singular\" }}"
               [("count", toGVal (5 :: Int))]
               "plural"
    , testCase "Ternary with length filter" $
        mkTest "{{ \"many\" if items|length > 3 else \"few\" }}"
               [("items", toGVal ([1,2,3,4,5] :: [Int]))]
               "many"
    ]

-- | Tests for the pluralize pattern: {{ count }} item{{ "s" if count != 1 else "" }}
-- This is a common Jinja2 idiom that should work in Ginger
pluralizePatternTests :: TestTree
pluralizePatternTests = testGroup "Pluralize pattern"
    [ testCase "plural (count=0)" $
        mkTest "{{ count }} item{{ \"s\" if count != 1 else \"\" }}"
               [("count", toGVal (0 :: Int))]
               "0 items"
    , testCase "singular (count=1)" $
        mkTest "{{ count }} item{{ \"s\" if count != 1 else \"\" }}"
               [("count", toGVal (1 :: Int))]
               "1 item"
    , testCase "plural (count=5)" $
        mkTest "{{ count }} item{{ \"s\" if count != 1 else \"\" }}"
               [("count", toGVal (5 :: Int))]
               "5 items"
    , testCase "C-style pluralize" $
        mkTest "{{ count }} item{{ count != 1 ? \"s\" : \"\" }}"
               [("count", toGVal (5 :: Int))]
               "5 items"
    , testCase "pluralize with length" $
        mkTest "{{ items|length }} item{{ \"s\" if items|length != 1 else \"\" }}"
               [("items", toGVal ([1,2,3] :: [Int]))]
               "3 items"
    , testCase "complex pluralize: person/people" $
        mkTest "{{ count }} {{ \"person\" if count == 1 else \"people\" }}"
               [("count", toGVal (1 :: Int))]
               "1 person"
    , testCase "complex pluralize: person/people (plural)" $
        mkTest "{{ count }} {{ \"person\" if count == 1 else \"people\" }}"
               [("count", toGVal (3 :: Int))]
               "3 people"
    ]

-- Helper to run a template test
mkTest :: String -> [(VarName, GVal (Run SourcePos IO Html))] -> Text -> Assertion
mkTest src contextDict expected = do
    run `catch` handleParserError
  where
    run = do
        let resolver _ = return Nothing
        let options = mkParserOptions resolver
        template <- either throw return =<< parseGinger' options src
        output <- newIORef mempty
        let write h = modifyIORef output (<> h)
        let context = makeContextHtmlM
                        (\key -> return $ fromMaybe def (lookup key contextDict))
                        write
        runGingerT context (optimize template)
        actual <- htmlSource <$> readIORef output
        assertEqual "" expected actual

    handleParserError :: ParserError -> IO ()
    handleParserError err = do
        assertBool (formatParserError (Just src) err) False

-- | Tests for the truncate filter (Jinja2 compatible string truncation)
truncateFilterTests :: TestTree
truncateFilterTests = testGroup "Truncate filter"
    [ testCase "truncate short string (no change)" $
        mkTest "{{ text|truncate(20) }}"
               [("text", toGVal ("Hello" :: Text))]
               "Hello"
    , testCase "truncate long string" $
        mkTest "{{ text|truncate(10) }}"
               [("text", toGVal ("Hello, World!" :: Text))]
               "Hello, ..."
    , testCase "truncate exact length (no change)" $
        mkTest "{{ text|truncate(5) }}"
               [("text", toGVal ("Hello" :: Text))]
               "Hello"
    , testCase "truncate with custom end" $
        mkTest "{{ text|truncate(10, end='…') }}"
               [("text", toGVal ("Hello, World!" :: Text))]
               "Hello, Wo…"
    , testCase "truncate with empty end" $
        mkTest "{{ text|truncate(7, end='') }}"
               [("text", toGVal ("Hello, World!" :: Text))]
               "Hello, "
    , testCase "truncate default length 255" $
        mkTest "{{ text|truncate }}"
               [("text", toGVal ("Short text" :: Text))]
               "Short text"
    , testCase "truncate in expression context" $
        mkTest "{% if notes|truncate(10)|length < 15 %}short{% else %}long{% endif %}"
               [("notes", toGVal ("This is a very long note that exceeds the limit" :: Text))]
               "short"
    , testCase "truncate chained with other filters" $
        mkTest "{{ text|upper|truncate(8) }}"
               [("text", toGVal ("hello world" :: Text))]
               "HELLO..."
    ]
