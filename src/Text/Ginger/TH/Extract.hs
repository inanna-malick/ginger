{-# LANGUAGE OverloadedStrings #-}
-- | Extract variable access paths from ginger template AST.
-- Performs scope-aware traversal to distinguish free variables
-- (that must come from context) from locally bound variables.
module Text.Ginger.TH.Extract
  ( extractVariableAccesses
  , extractFromTemplate
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Control.Monad (foldM)
import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Maybe (catMaybes, maybeToList)
import Text.Parsec.Pos (SourcePos)

import Text.Ginger.AST
import Text.Ginger.TH.Types (AccessPath(..), PathSegment(..))

-- | Local scope: set of variable names that are bound locally.
type LocalScope = Set Text

-- | Extract all variable access paths from a template.
-- Returns paths for free variables only (not locally bound ones).
extractFromTemplate :: Template SourcePos -> [AccessPath]
extractFromTemplate tpl = extractVariableAccesses (templateBody tpl)

-- | Extract variable accesses from a statement.
extractVariableAccesses :: Statement SourcePos -> [AccessPath]
extractVariableAccesses stmt = execWriter (walkStatement Set.empty stmt)

-- | Walk a statement, collecting access paths.
-- The scope parameter tracks locally bound variables.
-- Returns the updated scope (for sequential binding via SetVarS).
walkStatement :: LocalScope -> Statement SourcePos -> Writer [AccessPath] LocalScope
walkStatement scope stmt = case stmt of
  MultiS _ stmts ->
    -- Process statements sequentially, threading scope through
    -- so that SetVarS bindings are visible to subsequent statements
    foldM walkStatement scope stmts

  ScopedS _ body -> do
    -- ScopedS creates isolated scope - bindings don't escape
    _ <- walkStatement scope body
    return scope

  IndentS _ expr body -> do
    walkExpression scope expr
    walkStatement scope body

  LiteralS _ _ ->
    return scope

  InterpolationS _ expr -> do
    walkExpression scope expr
    return scope

  ExpressionS _ expr -> do
    walkExpression scope expr
    return scope

  IfS _ cond trueBranch falseBranch -> do
    walkExpression scope cond
    -- Both branches see the same scope; bindings don't escape branches
    _ <- walkStatement scope trueBranch
    _ <- walkStatement scope falseBranch
    return scope

  SwitchS _ expr cases defaultCase -> do
    walkExpression scope expr
    mapM_ (\(caseExpr, caseBody) -> do
      walkExpression scope caseExpr
      walkStatement scope caseBody) cases
    _ <- walkStatement scope defaultCase
    return scope

  ForS _ mIndex varName iterExpr body -> do
    -- The iterable expression is evaluated in outer scope
    walkExpression scope iterExpr
    -- For loop binds: index (optional), value, and implicit "loop"
    let scope' = scope
          `Set.union` Set.singleton varName
          `Set.union` maybe Set.empty Set.singleton mIndex
          `Set.union` Set.singleton "loop"
    -- Bindings inside loop body don't escape the loop
    _ <- walkStatement scope' body
    return scope

  SetVarS _ varName expr -> do
    -- RHS is evaluated in current scope (before binding)
    walkExpression scope expr
    -- Return scope with the new binding for subsequent statements
    return $ Set.insert varName scope

  DefMacroS _ macroName (Macro argNames body) -> do
    -- Macro body has its own scope with just the arguments
    let macroScope = Set.fromList argNames
          `Set.union` Set.singleton "varargs"
          `Set.union` Set.singleton "kwargs"
    _ <- walkStatement macroScope body
    -- The macro name is now available in scope
    return $ Set.insert macroName scope

  BlockRefS _ _ ->
    return scope

  PreprocessedIncludeS _ includedTpl -> do
    -- Included templates share scope and can add bindings
    walkStatement scope (templateBody includedTpl)

  NullS _ ->
    return scope

  TryCatchS _ tryBody catches finallyBody -> do
    -- Bindings in try/catch/finally don't escape
    _ <- walkStatement scope tryBody
    mapM_ (walkCatch scope) catches
    _ <- walkStatement scope finallyBody
    return scope

-- | Walk a catch block.
walkCatch :: LocalScope -> CatchBlock SourcePos -> Writer [AccessPath] ()
walkCatch scope (Catch _ captureVar body) = do
  -- Catch block may bind the exception variable
  let scope' = scope `Set.union` maybe Set.empty Set.singleton captureVar
  _ <- walkStatement scope' body
  return ()

-- | Walk an expression, collecting access paths.
walkExpression :: LocalScope -> Expression SourcePos -> Writer [AccessPath] ()
walkExpression scope expr = case expr of
  StringLiteralE _ _ -> return ()
  NumberLiteralE _ _ -> return ()
  BoolLiteralE _ _ -> return ()
  NullLiteralE _ -> return ()

  VarE pos varName
    | varName `Set.member` scope -> return ()  -- Locally bound
    | otherwise -> tell [AccessPath varName [] pos]  -- Free variable

  ListE _ exprs ->
    mapM_ (walkExpression scope) exprs

  ObjectE _ pairs ->
    mapM_ (\(k, v) -> walkExpression scope k >> walkExpression scope v) pairs

  MemberLookupE pos _ _ ->
    -- Try to collect the full access path
    case collectAccessPath expr of
      Just (rootVar, segments, rootPos)
        | rootVar `Set.notMember` scope ->
            tell [AccessPath rootVar segments rootPos]
        | otherwise ->
            -- Root is locally bound, but we should still walk
            -- for any dynamic key expressions
            walkMemberChainForDynamicKeys scope expr
      Nothing ->
        -- Complex base expression, walk recursively
        walkMemberChainRecursive scope expr

  CallE _ funcExpr args -> do
    walkExpression scope funcExpr
    mapM_ (walkExpression scope . snd) args

  LambdaE _ argNames body -> do
    -- Lambda params are bound in body
    let scope' = scope `Set.union` Set.fromList argNames
    walkExpression scope' body

  TernaryE _ cond true false -> do
    walkExpression scope cond
    walkExpression scope true
    walkExpression scope false

  DoE _ stmt -> do
    -- Do expressions contain statements; bindings don't escape
    _ <- walkStatement scope stmt
    return ()

-- | Try to collect a chain of member lookups into a single AccessPath.
-- Returns (rootVarName, pathSegments, rootPosition) if successful.
collectAccessPath :: Expression SourcePos -> Maybe (Text, [PathSegment], SourcePos)
collectAccessPath expr = go expr []
  where
    go (VarE pos varName) segments =
      Just (varName, segments, pos)
    go (MemberLookupE _ baseExpr keyExpr) segments = do
      segment <- toSegment keyExpr
      go baseExpr (segment : segments)
    go _ _ = Nothing

    toSegment (StringLiteralE _ s) = Just (StaticKey s)
    toSegment _ = Just DynamicKey  -- Any non-literal is dynamic

-- | Walk a member lookup chain recursively when we can't collect it as a path.
walkMemberChainRecursive :: LocalScope -> Expression SourcePos -> Writer [AccessPath] ()
walkMemberChainRecursive scope (MemberLookupE _ baseExpr keyExpr) = do
  walkExpression scope baseExpr
  walkExpression scope keyExpr
walkMemberChainRecursive scope expr =
  walkExpression scope expr

-- | Walk member chain looking for dynamic key expressions to traverse.
walkMemberChainForDynamicKeys :: LocalScope -> Expression SourcePos -> Writer [AccessPath] ()
walkMemberChainForDynamicKeys scope (MemberLookupE _ baseExpr keyExpr) = do
  walkMemberChainForDynamicKeys scope baseExpr
  -- Walk the key expression if it's not a literal
  case keyExpr of
    StringLiteralE _ _ -> return ()
    _ -> walkExpression scope keyExpr
walkMemberChainForDynamicKeys _ (VarE _ _) = return ()
walkMemberChainForDynamicKeys scope expr = walkExpression scope expr
