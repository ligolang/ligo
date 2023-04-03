-- This code is copypasted from Morley.Debugger.DAP.Variables
module Language.LIGO.DAP.Variables
  ( createVariables
  , createLigoVariablesDummy
  , runBuilder
  , buildVariable
  , insertToIndex
  ) where

import Control.Lens hiding ((...))
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as M
import Fmt (pretty)

import Morley.Debugger.Core (DebugPrintMode (DpmEvaluated, DpmNormal), debugBuild)
import Morley.Debugger.Protocol.DAP (Variable)
import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Michelson.Typed
  (Constrained (SomeValue), EntrypointCallT (..), EpAddress (..), SingI,
  SomeEntrypointCallT (SomeEpc), Value, Value' (..))
import Morley.Michelson.Untyped.Entrypoints (isDefEpName)

import AST (Lang)
import Cli.Json
  (LigoTableField, LigoTypeApp (..), LigoTypeContent (..), LigoTypeExpression (..),
  LigoTypeTable (..), _ltfAssociatedType)

import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.CLI.Types.LigoValue
import Language.LIGO.Debugger.Snapshots (StackItem (StackItem))

-- | For a given stack generate its representation as a tree of 'DAP.Variable's.
--
-- This creates a map @varaibles references -> [variable]@, where root always has
-- largest reference.
createVariables :: (SingI u) => Lang -> [StackItem u] -> VariableBuilder Int
createVariables lang st = do
  topVarsMb <-
    forM st \(StackItem desc ligoOrMichValue) -> do
      case desc of
        LigoStackEntry LigoExposedStackEntry{..} -> do
          let name = maybe (pretty unknownVariable) pretty leseDeclaration
          Just <$> buildVariable lang leseType ligoOrMichValue name
        _ -> pure Nothing
  let topVars = catMaybes topVarsMb
  insertVars topVars

createLigoVariablesDummy :: Lang -> [(Text, Text)] -> VariableBuilder Int
createLigoVariablesDummy lang vars = do
  insertVars $ vars <&> \(toString -> name, toString -> val) ->
    createVariable name val lang (LigoType Nothing) Nothing Nothing

type VariableBuilder a = State (Int, Map Int [DAP.Variable]) a

runBuilder :: VariableBuilder a -> (a, Map Int [DAP.Variable])
runBuilder act = (res, vars)
  where
    (res, (_, vars)) = usingState (1, mempty) act

insertToIndex :: Int -> [DAP.Variable] -> VariableBuilder Int
insertToIndex idx vars = do
  _2 %= M.insertWith (\added cur -> cur <> added) idx vars
  pure idx

insertVars :: [DAP.Variable] -> VariableBuilder Int
insertVars vars = do
  -- <<%= modifies state and returns previous value
  nextIdx <- _1 <<%= (+1)
  _2 %= M.insert nextIdx vars
  return nextIdx

createVariable :: String -> String -> Lang -> LigoType -> Maybe String -> Maybe String -> Variable
createVariable name varText lang typ menuContext evaluateName = DAP.defaultVariable
  { DAP.nameVariable = name
  , DAP.valueVariable = varText
  , DAP.typeVariable = pretty (buildType lang typ)
  , DAP.__vscodeVariableMenuContextVariable = menuContext
  , DAP.evaluateNameVariable = evaluateName
  }

buildVariable :: Lang -> LigoType -> LigoOrMichValue -> String -> VariableBuilder Variable
buildVariable lang typ v name = do
  let
    varText = pretty $ debugBuild DpmNormal v
    evaluatedText = pretty $ debugBuild DpmEvaluated v
    menuContext = case v of
      LigoValue _ ligoVal -> case ligoVal of
        LVCt LCAddress{} -> Just "address"
        LVCt LCContract{} -> Just "contract"
        _ -> Nothing
      MichValue (SomeValue michValue) -> case michValue of
        VAddress  {} -> Just "address"
        VContract {} -> Just "contract"
        _            -> Nothing
    var = createVariable name varText lang typ menuContext (Just evaluatedText)

  subVars <- buildSubVars lang typ v

  case subVars of
    [] -> return var
    _ -> do
      idx <- insertVars subVars
      return $ var
        { DAP.variablesReferenceVariable = idx
        }

getInnerTypeFromApp :: Int -> LigoType -> LigoType
getInnerTypeFromApp i = \case
  LigoTypeResolved LigoTypeExpression
    { _lteTypeContent = LTCApp LigoTypeApp{..}
    } -> LigoType (_ltaArguments ^? ix i)
  _ -> LigoType Nothing

getInnerFieldFromRecord :: Text -> LigoType -> Maybe LigoTableField
getInnerFieldFromRecord name = \case
  LigoTypeResolved LigoTypeExpression
    { _lteTypeContent = LTCRecord
        ( LigoTypeTable
            { _lttFields = hm
            }
        )
    } -> hm HM.!? name
  _ -> Nothing

getInnerTypeFromRecord :: Text -> LigoType -> LigoType
getInnerTypeFromRecord = LigoType ... fmap _ltfAssociatedType ... getInnerFieldFromRecord

getEpAddressChildren :: Lang -> EpAddress -> [Variable]
getEpAddressChildren lang EpAddress'{..} =
  if isDefEpName eaEntrypoint
  then []
  else [addr, ep]
  where
    addr = createVariable "address" (pretty eaAddress) lang (LigoType Nothing) Nothing Nothing
    ep = createVariable "entrypoint" (pretty eaEntrypoint) lang (LigoType Nothing) Nothing Nothing

buildSubVars :: Lang -> LigoType -> LigoOrMichValue -> VariableBuilder [Variable]
buildSubVars lang typ = \case
  MichValue (SomeValue michValue) -> case michValue of
    VOption Nothing -> return []
    VOption (Just v) -> do
      (:[]) <$> buildVariable lang (getInnerTypeFromApp 0 typ) (toLigoValue v) "Some"
    VList lst -> do
      zipWithM (buildVariable lang (getInnerTypeFromApp 0 typ) . toLigoValue) lst (show <$> [1 :: Int ..])
    VSet s -> do
      zipWithM (buildVariable lang (getInnerTypeFromApp 0 typ) . toLigoValue) (toList s) (show <$> [1 :: Int ..])
    VMap m -> do
      forM (toPairs m) \(k, v) -> do
        let name = pretty $ debugBuild DpmNormal k
        buildVariable lang (getInnerTypeFromRecord name typ) (toLigoValue v) (toString name)
    VBigMap _id m -> do
      forM (toPairs m) \(k, v) -> do
        let name = pretty $ debugBuild DpmNormal k
        buildVariable lang (getInnerTypeFromRecord name typ) (toLigoValue v) (toString name)
    VContract eaAddress (SomeEpc EntrypointCall{ epcName = eaEntrypoint }) -> do
      pure $ getEpAddressChildren lang EpAddress'{..}
    VAddress epAddress -> pure $ getEpAddressChildren lang epAddress
    -- Other value types do not have nested structure
    _ -> return []
  LigoValue _ ligoValue -> case ligoValue of
    LVCt (LCContract LigoContract{..})
      | Just entrypoint <- lcEntrypoint -> do
          let addr = createVariable "address" (toString lcAddress) lang (LigoType Nothing) Nothing Nothing
          let ep = createVariable "entrypoint" (toString entrypoint) lang (LigoType Nothing) Nothing Nothing
          return [addr, ep]
      | otherwise -> return []
    LVList lst ->
      let innerType = getInnerTypeFromApp 0 typ in
      zipWithM (buildVariable lang innerType . LigoValue innerType) lst (show <$> [1 :: Int ..])
    value@(LVRecord record) -> case toTupleMaybe value of
      Just values ->
        zipWithM
          do \val n ->
              let innerType = getInnerTypeFromRecord n typ in
              buildVariable lang innerType (LigoValue innerType val) (toString n)
          values
          (show <$> [1 :: Int ..])
      Nothing -> do
        case getRecordOrderMb typ value of
          Just order -> do
            forM order \(name, (t, v)) -> do
              buildVariable lang t (LigoValue t v) (toString name)
          Nothing -> do
            forM (toPairs record) \(name, v) -> do
              let innerType = getInnerTypeFromRecord name typ
              buildVariable lang innerType (LigoValue innerType v) (toString name)
    LVConstructor (ctor, value) ->
      let innerType = getInnerTypeFromApp 0 typ in
      (:[]) <$> buildVariable lang innerType (LigoValue innerType value) (toString ctor)
    LVSet s ->
      let innerType = getInnerTypeFromApp 0 typ in
      zipWithM (buildVariable lang innerType . LigoValue innerType) s (show <$> [1 :: Int ..])
    LVMap m -> do
      forM m \(k, v) -> do
        let keyType = getInnerTypeFromApp 0 typ
        let valueType = getInnerTypeFromApp 1 typ

        let name = pretty @_ @String $ debugBuild DpmNormal (LigoValue keyType k)
        buildVariable lang valueType (LigoValue valueType v) name
    _ -> return []
  where
    toLigoValue :: (SingI t) => Value t -> LigoOrMichValue
    toLigoValue = MichValue . SomeValue
