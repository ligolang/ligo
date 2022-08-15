-- This code is copypasted from Morley.Debugger.DAP.Variables
module Language.LIGO.DAP.Variables
  ( createVariables
  , runBuilder
  , buildVariable
  , insertToIndex
  ) where

import Control.Lens
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as M
import Data.Text qualified as T
import Fmt (pretty)
import Morley.Debugger.Core (DebugPrintMode (DpmEvaluated, DpmNormal), debugBuild)
import Morley.Debugger.Protocol.DAP (Variable)
import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Michelson.Typed (SomeConstrainedValue (SomeValue), Value, Value' (..))

import Language.LIGO.Debugger.CLI.Types
  (LigoExposedStackEntry (LigoExposedStackEntry, leseDeclaration, leseType),
  LigoStackEntry (LigoStackEntry), LigoType (LTApp, LTRecord, LTUnresolved),
  LigoTypeApp (LigoTypeApp, ltaArguments))
import Language.LIGO.Debugger.Snapshots (StackItem (StackItem))

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
          Just <$> buildVariable leseType v name
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

createVariable :: String -> String -> LigoType -> Maybe String -> Maybe String -> Variable
createVariable name varText typ menuContext evaluateName = DAP.defaultVariable
  { DAP.nameVariable = name
  , DAP.valueVariable = varText
  , DAP.typeVariable = pretty typ
  , DAP.__vscodeVariableMenuContextVariable = menuContext
  , DAP.evaluateNameVariable = evaluateName
  }

buildVariable :: forall t. LigoType -> Value t -> String -> VariableBuilder Variable
buildVariable typ v name = do
  let
    varText = pretty $ debugBuild DpmNormal v
    evaluatedText = pretty $ debugBuild DpmEvaluated v
    menuContext = case v of
      VAddress  {} -> Just "address"
      VContract {} -> Just "contract"
      _            -> Nothing
    var = createVariable name varText typ menuContext (Just evaluatedText)

  subVars <- buildSubVars typ v

  case subVars of
    [] -> return var
    _ -> do
      idx <- insertVars subVars
      return $ var
        { DAP.variablesReferenceVariable = idx
        }

getInnerTypeFromApp :: Int -> LigoType -> LigoType
getInnerTypeFromApp i = \case
  LTApp LigoTypeApp{..} -> fromMaybe LTUnresolved (ltaArguments ^? ix i)
  _ -> LTUnresolved

getInnerTypeFromRecord :: Text -> LigoType -> LigoType
getInnerTypeFromRecord name = \case
  LTRecord hm -> fromMaybe LTUnresolved (hm HM.!? name)
  _ -> LTUnresolved

buildSubVars :: LigoType -> Value t -> VariableBuilder [Variable]
buildSubVars typ = \case
  VOption Nothing -> return []
  VOption (Just v) -> do
    (:[]) <$> buildVariable (getInnerTypeFromApp 0 typ) v "Some"
  VList lst -> do
    zipWithM (buildVariable (getInnerTypeFromApp 0 typ)) lst (show <$> [1 :: Int ..])
  VSet s -> do
    zipWithM (buildVariable (getInnerTypeFromApp 0 typ)) (toList s) (show <$> [1 :: Int ..])
  VMap m -> do
    forM (toPairs m) \(k, v) -> do
      let name = pretty $ debugBuild DpmNormal k
      buildVariable (getInnerTypeFromRecord name typ) v (T.unpack name)
  VBigMap _id m -> do
    forM (toPairs m) \(k, v) -> do
      let name = pretty $ debugBuild DpmNormal k
      buildVariable (getInnerTypeFromRecord name typ) v (T.unpack name)
  VContract address entrypoint -> do
    let addr = createVariable "address" (pretty address) LTUnresolved Nothing Nothing
    let ep = createVariable "entrypoint" (pretty entrypoint) LTUnresolved Nothing Nothing
    pure [addr, ep]
  -- Other value types do not have nested structure
  _ -> return []
