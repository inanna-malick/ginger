{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Compile-time type-checked templates for Ginger.
--
-- This module provides Template Haskell splices that verify at compile time
-- that template variable accesses match the fields of a Haskell data type.
--
-- = Core Guarantee
--
-- If a template compiles successfully with 'typedTemplateFile', all field
-- accesses in the template are guaranteed to succeed at runtime.
--
-- = Usage
--
-- @
-- {-# LANGUAGE TemplateHaskell #-}
--
-- data User = User
--   { userName :: Text
--   , userEmail :: Text
--   }
--
-- userTemplate :: TypedTemplate User SourcePos
-- userTemplate = $(typedTemplateFile ''User "templates/user.html")
--
-- renderUser :: User -> Text
-- renderUser user = runTypedTemplate user userTemplate
-- @
--
-- = Supported Patterns
--
-- * Static field access: @{{ user.name }}@, @{{ user["name"] }}@
-- * Nested access: @{{ user.profile.bio }}@
-- * Sum types: field must exist in ALL constructors
-- * Recursive types: supported naturally
--
-- = Rejected Patterns (Compile Error)
--
-- * Dynamic key access: @{{ user[varName] }}@
-- * Fields not in all constructors of sum types
-- * Polymorphic types
-- * Unknown types
--
module Text.Ginger.TH
  ( -- * Template Haskell splices
    typedTemplateFile
  , typedTemplate
    -- * Type-safe rendering
  , runTypedTemplate
  , runTypedTemplateM
    -- * Re-exports
  , TypedTemplate(..)
  ) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import Text.Parsec.Pos (SourcePos)
import Data.Monoid ((<>))
import Control.Monad.Writer (Writer)

import Text.Ginger.AST (Template)
import Text.Ginger.GVal (ToGVal, GVal)
import Text.Ginger.Parse (parseGinger', ParserError(..), ParserOptions(..), mkParserOptions)
import Text.Ginger.Run (easyRender, easyRenderM)
import Text.Ginger.Run.Type (Run, ContextEncodable, RuntimeError)

import Text.Ginger.TH.Types
import Text.Ginger.TH.Builtins (isBuiltin)
import Text.Ginger.TH.Extract (extractFromTemplate)
import Text.Ginger.TH.Schema (generateSchema, SchemaRegistry)
import Text.Ginger.TH.Validate (validatePaths, formatValidationErrors)

-- | Load and type-check a template from a file at compile time.
--
-- The first argument is the name of the context type (use @''TypeName@ syntax).
-- The second argument is the path to the template file.
--
-- @
-- myTemplate :: TypedTemplate MyContext SourcePos
-- myTemplate = $(typedTemplateFile ''MyContext "templates/my-template.html")
-- @
--
-- This will:
--
-- 1. Read the template file at compile time
-- 2. Parse the template
-- 3. Extract all variable accesses from the template
-- 4. Generate a schema from the Haskell type
-- 5. Validate that all accesses match the schema
-- 6. Fail compilation with clear error messages if validation fails
--
typedTemplateFile :: Name -> FilePath -> Q Exp
typedTemplateFile typeName templatePath = do
  -- Register the template file as a dependency for recompilation
  addDependentFile templatePath

  -- Read template source at compile time
  src <- runIO $ readFile templatePath

  -- Parse and validate
  validateAndEmbed typeName (Just templatePath) src

-- | Type-check an inline template string at compile time.
--
-- @
-- myTemplate :: TypedTemplate MyContext SourcePos
-- myTemplate = $(typedTemplate ''MyContext "Hello, {{ userName }}!")
-- @
--
typedTemplate :: Name -> String -> Q Exp
typedTemplate typeName src = validateAndEmbed typeName Nothing src

-- | Internal: validate template and emit code.
validateAndEmbed :: Name -> Maybe FilePath -> String -> Q Exp
validateAndEmbed typeName mPath src = do
  -- Parse the template
  let sourceName = mPath
  let opts = (mkParserOptions nullResolver) { poSourceName = sourceName }
  parseResult <- runIO $ parseGinger' opts src

  template <- case parseResult of
    Left err -> fail $ formatParserError mPath err
    Right tpl -> return tpl

  -- Generate schema from the Haskell type
  (schema, registry) <- generateSchema typeName

  -- Extract variable accesses from template
  let accesses = extractFromTemplate template

  -- Filter out builtins
  let userAccesses = filter (not . isBuiltin . apRoot) accesses

  -- Validate accesses against schema
  let errors = validatePaths registry schema userAccesses

  -- Report errors or generate code
  when (not $ null errors) $
    fail $ formatValidationErrors errors

  -- Generate code that parses the template at runtime
  -- We embed the source and parse at module load time
  -- (A more sophisticated version could add Lift instances to embed directly)
  let srcLit = litE (stringL src)
  let pathLit = maybe [| Nothing |] (\p -> [| Just p |]) mPath

  [| unsafeParseTemplate $pathLit $srcLit |]

-- | Null include resolver (no includes supported in type-checked templates for now).
nullResolver :: Monad m => String -> m (Maybe String)
nullResolver _ = return Nothing

-- | Format a parser error for display.
formatParserError :: Maybe FilePath -> ParserError -> String
formatParserError mPath err =
  "Template parse error" ++ pathSuffix ++ ":\n" ++ peErrorMessage err
  where
    pathSuffix = maybe "" (" in " ++) mPath

-- | Parse a template at runtime. Used internally by generated code.
-- This should not fail since we already validated at compile time.
unsafeParseTemplate :: Maybe String -> String -> TypedTemplate a SourcePos
unsafeParseTemplate mPath src =
  let opts = (mkParserOptions nullResolver) { poSourceName = mPath }
      result = runIdentity $ parseGinger' opts src
  in case result of
       Left err -> error $ "BUG: Template that passed compile-time validation failed to parse at runtime: " ++ peErrorMessage err
       Right tpl -> TypedTemplate tpl
  where
    runIdentity :: Identity a -> a
    runIdentity (Identity a) = a

newtype Identity a = Identity { unIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= f = f a

-- | Render a typed template with the correct context type.
-- Pure version that returns the rendered output directly.
--
-- @
-- let html = runTypedTemplate userData userTemplate
-- @
--
runTypedTemplate :: ( ContextEncodable h
                    , Monoid h
                    , ToGVal (Run SourcePos (Writer h) h) a
                    , ToGVal (Run SourcePos (Writer h) h) h
                    , ToGVal (Run SourcePos (Writer h) h) SourcePos
                    )
                 => a
                 -> TypedTemplate a SourcePos
                 -> h
runTypedTemplate context (TypedTemplate tpl) =
  easyRender context tpl

-- | Render a typed template in a monadic context.
--
-- @
-- runTypedTemplateM putStr userData userTemplate
-- @
--
runTypedTemplateM :: ( Monad m
                     , ContextEncodable h
                     , Monoid h
                     , ToGVal (Run SourcePos m h) a
                     , ToGVal (Run SourcePos m h) h
                     , ToGVal (Run SourcePos m h) SourcePos
                     )
                  => (h -> m ())
                  -> a
                  -> TypedTemplate a SourcePos
                  -> m (Either (RuntimeError SourcePos) (GVal (Run SourcePos m h)))
runTypedTemplateM emit context (TypedTemplate tpl) =
  easyRenderM emit context tpl
