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
    -- * QuasiQuoter
  , jinja
    -- * Type-safe rendering
  , runTypedTemplate
  , runTypedTemplateM
    -- * Re-exports
  , TypedTemplate(..)
  ) where

import Control.Monad (when)
import Data.IORef
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import Text.Parsec.Pos (SourcePos)
import Data.Monoid ((<>))
import Control.Monad.Writer (Writer)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))

import Text.Ginger.AST (Template)
import Text.Ginger.GVal (ToGVal, GVal)
import Text.Ginger.Parse (parseGinger', ParserError(..), ParserOptions(..), mkParserOptions, sourceLine, sourceColumn)
import Text.Ginger.Run (easyRender, easyRenderM)
import Text.Ginger.Run.Type (Run, ContextEncodable, RuntimeError)

import Text.Ginger.TH.Types
import Text.Ginger.TH.Builtins (isBuiltin)
import Text.Ginger.TH.Extract (extractFromTemplate)
import Text.Ginger.TH.QuasiQuote (jinja, jinjaRender)
import Text.Ginger.TH.Schema (generateSchema, SchemaRegistry)
import Text.Ginger.TH.Validate (validatePaths, formatValidationErrorsWithSource)

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
-- 2. Parse the template (including any @{% include %}@ directives)
-- 3. Extract all variable accesses from the template and included templates
-- 4. Generate a schema from the Haskell type
-- 5. Validate that all accesses match the schema
-- 6. Fail compilation with clear error messages if validation fails
-- 7. Register all included files as dependencies (recompile if they change)
--
typedTemplateFile :: Name -> FilePath -> Q Exp
typedTemplateFile typeName templatePath = do
  -- Register the template file as a dependency for recompilation
  addDependentFile templatePath

  -- Read template source at compile time
  src <- runIO $ readFile templatePath

  -- Track included files for dependency registration
  includedFilesRef <- runIO $ newIORef []

  -- Create a resolver that reads files relative to the template directory
  let templateDir = takeDirectory templatePath
  let resolver = ioResolver templateDir includedFilesRef

  -- Parse and validate
  result <- validateAndEmbedWithResolver typeName (Just templatePath) src resolver

  -- Register all included files as dependencies
  includedFiles <- runIO $ readIORef includedFilesRef
  mapM_ addDependentFile includedFiles

  return result

-- | Type-check an inline template string at compile time.
--
-- Note: Inline templates do not support @{% include %}@ directives.
-- Use 'typedTemplateFile' for templates with includes.
--
-- @
-- myTemplate :: TypedTemplate MyContext SourcePos
-- myTemplate = $(typedTemplate ''MyContext "Hello, {{ userName }}!")
-- @
--
typedTemplate :: Name -> String -> Q Exp
typedTemplate typeName src = validateAndEmbedWithResolver typeName Nothing src nullResolver

-- | Internal: validate template and emit code with a custom resolver.
validateAndEmbedWithResolver :: Name -> Maybe FilePath -> String -> (String -> IO (Maybe String)) -> Q Exp
validateAndEmbedWithResolver typeName mPath src resolver = do
  -- Parse the template
  let sourceName = mPath
  let opts = (mkParserOptions resolver) { poSourceName = sourceName }
  parseResult <- runIO $ parseGinger' opts src

  template <- case parseResult of
    Left err -> fail $ formatParserErrorWithSource mPath src err
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
    fail $ formatValidationErrorsWithSource src errors

  -- Generate code that parses the template at runtime
  -- We embed the source and parse at module load time
  -- (A more sophisticated version could add Lift instances to embed directly)
  let srcLit = litE (stringL src)
  let pathLit = maybe [| Nothing |] (\p -> [| Just p |]) mPath

  [| unsafeParseTemplate $pathLit $srcLit |]

-- | Null include resolver (no includes supported for inline templates).
nullResolver :: Monad m => String -> m (Maybe String)
nullResolver _ = return Nothing

-- | File-based include resolver for typedTemplateFile.
-- Resolves includes relative to the base directory and tracks which files were included.
ioResolver :: FilePath -> IORef [FilePath] -> String -> IO (Maybe String)
ioResolver baseDir includedFilesRef path = do
  let fullPath = baseDir </> path
  exists <- doesFileExist fullPath
  if exists
    then do
      -- Track this file for dependency registration
      modifyIORef includedFilesRef (fullPath :)
      content <- readFile fullPath
      return $ Just content
    else return Nothing

-- | Format a parser error with source context (Rust-style).
formatParserErrorWithSource :: Maybe FilePath -> String -> ParserError -> String
formatParserErrorWithSource mPath src err =
  unlines $ catMaybes
    [ Just $ "error: Template parse error" ++ pathSuffix
    , locationLine
    , Just "   |"
    , sourceLine'
    , caretLine
    , Just "   |"
    , Just $ "   = " ++ peErrorMessage err
    ]
  where
    pathSuffix = maybe "" (" in " ++) mPath
    sourceLines = lines src

    locationLine = do
      pos <- peSourcePosition err
      return $ "  --> " ++ maybe "<unknown>" id mPath ++ ":" ++
               show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)

    sourceLine' = do
      pos <- peSourcePosition err
      let lineNum = sourceLine pos
      line <- listToMaybe $ drop (lineNum - 1) sourceLines
      return $ padLineNum lineNum ++ " | " ++ line

    caretLine = do
      pos <- peSourcePosition err
      let col = sourceColumn pos
      return $ "   | " ++ replicate (col - 1) ' ' ++ "^"

    padLineNum n = let s = show n in replicate (3 - length s) ' ' ++ s

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
