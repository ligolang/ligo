-- This code is copypasted from Morley.Debugger.DAP.Variables
module Language.LIGO.DAP.Variables
  ( createVariables
  , runBuilder
  , buildVariable
  , insertToIndex
  ) where

import Control.Lens
import Data.Map qualified as M
import Data.Singletons (demote)
import Fmt (pretty)
import Language.LIGO.Debugger.CLI.Types
  (LigoExposedStackEntry (LigoExposedStackEntry, leseDeclaration), LigoStackEntry (LigoStackEntry))
import Language.LIGO.Debugger.Snapshots (StackItem (StackItem))
import Morley.Debugger.Core (DebugPrintMode (DpmEvaluated, DpmNormal), debugBuild)
import Morley.Debugger.Protocol.DAP (Variable)
import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Michelson.Typed
  (SomeConstrainedValue (SomeValue), Value, Value' (..), withValueTypeSanity)

-- | For a given stack generate its representation as a tree of 'DAP.Variable's.
--
-- This creates a map @varaibles references -> [variable]@, where root always has
-- largest reference.
createVariables :: [StackItem] -> VariableBuilder Int
createVariables st = do
  topVarsMb <-
    forM st \(StackItem desc (SomeValue v)) -> do
      case desc of
        LigoStackEntry LigoExposedStackEntry{..} -> do
          let name = maybe "?" pretty leseDeclaration
          Just <$> buildVariable v name
        _ -> pure Nothing
  let topVars = catMaybes topVarsMb
  insertVars topVars

type VariableBuilder a = State (Int, Map Int [DAP.Variable]) a

runBuilder :: VariableBuilder a -> (a, Map Int [DAP.Variable])
runBuilder act = (res, vars)
  where
    (res, (_, vars)) = usingState (1, mempty) act

insertToIndex :: Int -> [DAP.Variable] -> VariableBuilder Int
insertToIndex idx vars = do
  _2 %= M.insertWith (<>) idx vars
  pure idx

insertVars :: [DAP.Variable] -> VariableBuilder Int
insertVars vars = do
  -- <<%= modifies state and returns previous value
  nextIdx <- _1 <<%= (+1)
  _2 %= M.insert nextIdx vars
  return nextIdx

buildVariable :: forall t. Value t -> String -> VariableBuilder Variable
buildVariable v name = do
  let
    varText = pretty $ debugBuild DpmNormal v
    evaluatedText = pretty $ debugBuild DpmEvaluated v
    varType = withValueTypeSanity v $ pretty $ demote @t
    var = DAP.defaultVariable
      { DAP.nameVariable = name
      , DAP.valueVariable = varText
      , DAP.typeVariable = varType
      , DAP.__vscodeVariableMenuContextVariable = case v of
          VAddress  {} -> Just "address"
          VContract {} -> Just "contract"
          _            -> Nothing
      , DAP.evaluateNameVariable = Just evaluatedText
      }

  subVars <- buildSubVars v

  case subVars of
    [] -> return var
    _ -> do
      idx <- insertVars subVars
      return $ var
        { DAP.variablesReferenceVariable = idx
        }

buildSubVars :: Value t -> VariableBuilder [Variable]
buildSubVars = \case
  VOption Nothing -> return []
  VOption (Just v) ->
    (:[]) <$> buildVariable v "Some"
  VList lst -> do
    zipWithM buildVariable lst (show <$> [1 :: Int ..])
  VSet s ->
    zipWithM buildVariable (toList s) (show <$> [1 :: Int ..])
  VMap m -> do
    mapM (\(k, v) -> buildVariable v $ pretty $
      debugBuild DpmNormal k) $ toPairs m
  VBigMap _id m -> do
    mapM (\(k, v) -> buildVariable v $ pretty $
      debugBuild DpmNormal k) $ toPairs m
  -- Other value types do not have nested structure
  _ -> return []
