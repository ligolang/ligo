{-# LANGUAGE DuplicateRecordFields #-}

-- This code is copypasted from Morley.Debugger.DAP.Variables
module Language.LIGO.DAP.Variables
  ( PreConvertVariable (..)
  , PreConvertAppliedArguments (..)
  , createVariables
  , runBuilder
  , buildVariable
  , insertToIndex
  ) where

import Control.Lens hiding (children, (...))
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as M
import Fmt.Buildable (pretty)
import Named (defaults, paramF, (!))
import Text.Interpolation.Nyan hiding (rmode')
import Util

import Protocol.DAP qualified as DAP

import Morley.Debugger.Core (DebugPrintMode (DpmEvaluated, DpmNormal), debugBuild)
import Morley.Michelson.Typed
  (Constrained (SomeValue), EntrypointCallT (..), EpAddress (..), SingI,
  SomeEntrypointCallT (SomeEpc), Value, Value' (..))
import Morley.Michelson.Untyped.Entrypoints (isDefEpName)

import Language.LIGO.AST (Lang)
import Language.LIGO.Debugger.CLI

-- | A representation of applied arguments meta before
-- converting it into a tree.
data PreConvertAppliedArguments = PreConvertAppliedArguments
  { pcaaFunctionName :: Name 'Concise -- ^ A curried function name.
  , pcaaArguments :: [(LigoOrMichValue, Maybe PreConvertAppliedArguments)]
      -- ^ Applied arguments with their possible application metas.
  , pcaaPreviousMeta :: Maybe PreConvertAppliedArguments
      -- ^ A previous application meta if the function is curried not for the first time.
  }

-- | A representation of a variable before converting
-- it into @DAP.Variable@.
data PreConvertVariable = PreConvertVariable
  { pcvName :: Text -- ^ A variable's name.
  , pcvValue :: LigoOrMichValue -- ^ A variable's value.
  , pcvApplicationMeta :: Maybe PreConvertAppliedArguments
      -- ^ Possible application meta for curried function.
  }

-- | For a given stack generate its representation as a tree of 'DAP.Variable's.
--
-- This creates a map @varaibles references -> [variable]@, where root always has
-- largest reference.
createVariables :: Lang -> [PreConvertVariable] -> VariableBuilder DAP.VariableId
createVariables lang varsAndNames = do
  topVars <- traverse (buildVariable lang) varsAndNames
  insertVars topVars

-- | A convenient type alias of @State@ monad which stores the last free
-- variables reference and @Map DAP.VariableId [DAP.Variable]@ that resolves
-- variables reference into actual @DAP.Variable@ list.
type VariableBuilder a = State (Int, Map DAP.VariableId [DAP.Variable]) a

-- | Runs @VariableBuilder@.
runBuilder :: VariableBuilder a -> (a, Map DAP.VariableId [DAP.Variable])
runBuilder act = (res, vars)
  where
    (res, (_, vars)) = usingState (1, mempty) act

-- | @insertToIndex idx vars@ adds new variables @vars@
-- to @VariableBuilder@'s map at reference key @idx@.
insertToIndex :: DAP.VariableId -> [DAP.Variable] -> VariableBuilder DAP.VariableId
insertToIndex idx vars = do
  _2 %= M.insertWith (\added cur -> cur <> added) idx vars
  pure idx

-- | @insertVars vars@ allocates new variables reference and resolves it into @vars@.
insertVars :: [DAP.Variable] -> VariableBuilder DAP.VariableId
insertVars vars = do
  -- <<%= modifies state and returns previous value
  (DAP.VariableId -> nextIdx) <- _1 <<%= (+1)
  _2 %= M.insert nextIdx vars
  return nextIdx

-- | Creates a new @DAP.Variable@.
createVariable
  :: Text -- ^ Variable's name.
  -> Text -- ^ Variable's value.
  -> Lang -- ^ The LIGO dialect that would be used to prettify variable's type.
  -> LigoType -- ^ Variable's type.
  -> Maybe Text -- ^ Menu context.
  -> Maybe Text -- ^ Evaluate name. This name would be copy-pasted
                -- if you click "Copy Value" in the pane with variables.
  -> DAP.Variable
createVariable name varText lang typ menuContext evaluateName = DAP.mk @DAP.Variable
  ! #name name
  ! #value varText
  ! paramF #type (pretty . buildTypeExpr lang <$> unLigoType typ)
  ! paramF #__vscodeVariableMenuContext menuContext
  ! paramF #evaluateName evaluateName
  ! defaults

-- | Creates the next tree:
-- 1. @func: func_name@. If @func_name@ is a partially applied function
--    then it'll create the same tree for it.
-- 2. A list of @arg#{n}@ fields which represents applied arguments.
--    If an argument is a partially applied function
--    then it'll create the same tree for it.
buildAppliedArguments :: Lang -> PreConvertAppliedArguments -> VariableBuilder [DAP.Variable]
buildAppliedArguments lang PreConvertAppliedArguments{..} = do
  let functionName' =
        createVariable
          "func"
          (pretty pcaaFunctionName)
          lang
          LigoTypeUnresolved
          Nothing
          Nothing

  prevFuncArguments <- maybe (pure []) (buildAppliedArguments lang) pcaaPreviousMeta

  functionName <- insertChildren functionName' prevFuncArguments
  arguments <- zipWithM buildArgument [1..] pcaaArguments

  pure (functionName : arguments)
  where
    buildArgument :: Int -> (LigoOrMichValue, Maybe PreConvertAppliedArguments) -> VariableBuilder DAP.Variable
    buildArgument n (val, applicationMeta) = do
      let pcvName = [int||arg#{n}|]
      let pcvValue = val
      let pcvApplicationMeta = Nothing

      var <- buildVariable lang PreConvertVariable{..}
      arguments <- maybe (pure []) (buildAppliedArguments lang) applicationMeta
      insertChildren var arguments

-- | @insertChildren var vars@ will allocate a new variables reference with @vars@
-- and update @variablesReference@ field in @var@ with it.
--
-- Does nothing if @vars@ list is empty.
insertChildren :: DAP.Variable -> [DAP.Variable] -> VariableBuilder DAP.Variable
insertChildren var = \case
  [] -> pure var
  children -> do
    idx <- insertVars children
    pure $ (var :: DAP.Variable)
      { DAP.variablesReference = idx
      }

-- | Converts @PreConvertVariable@ into @DAP.Variable@.
buildVariable :: Lang -> PreConvertVariable -> VariableBuilder DAP.Variable
buildVariable lang PreConvertVariable{pcvValue = v, ..} = do
  let
    varText = pretty $ debugBuild DpmNormal (lang, v)
    evaluatedText = pretty $ debugBuild DpmEvaluated (lang, v)
    menuContext = case v of
      LigoValue _ ligoVal -> case ligoVal of
        LVCt LCAddress{} -> Just "address"
        LVCt LCContract{} -> Just "contract"
        _ -> Nothing
      MichValue _ (SomeValue michValue) -> case michValue of
        VAddress  {} -> Just "address"
        VContract {} -> Just "contract"
        _            -> Nothing
      _ -> Nothing

    typ = getLigoType v
    var = createVariable pcvName varText lang typ menuContext (Just evaluatedText)

  subVars <- buildSubVars lang v
  children <- maybe (pure []) (buildAppliedArguments lang) pcvApplicationMeta

  insertChildren var (subVars <> children)

-- | A helper function that extracts an inner type at
-- specific position from the constant's type content.
--
-- Returns @LigoType Nothing@ if the type is not constant
-- of the index is out of bounds.
getInnerTypeFromConstant :: Int -> LigoType -> LigoType
getInnerTypeFromConstant i = \case
  LigoTypeResolved LigoTypeExpression
    { _lteTypeContent = LTCConstant LigoTypeConstant{..}
    } -> LigoType (_ltcParameters ^? ix i)
  _ -> LigoType Nothing

-- | A helper function that extracts an inner type from record type.
--
-- Returns @Nothing@ if the type is not a record or if the field is missing.
getInnerFieldFromRecord :: Text -> LigoType -> Maybe LigoTypeExpression
getInnerFieldFromRecord name = \case
  LigoTypeResolved LigoTypeExpression
    { _lteTypeContent = LTCRecord
        ( LigoTypeTable
            { _lttFields = hm
            }
        )
    } -> hm HM.!? name
  _ -> Nothing

-- | Same as @getInnerFieldFromRecord@ but wraps
-- the result with @LigoType@ constructor.
getInnerTypeFromRecord :: Text -> LigoType -> LigoType
getInnerTypeFromRecord = LigoType ... getInnerFieldFromRecord

-- | A helper function that extracts an inner type from sum type.
--
-- Returns @Nothing@ if the type is not a sum or if the constructor is missing.
getInnerTypeFromSum :: Text -> LigoType -> LigoType
getInnerTypeFromSum name = \case
  LigoTypeResolved LigoTypeExpression
    { _lteTypeContent = LTCSum
        ( LigoTypeTable
            { _lttFields = hm
            }
        )
    } -> LigoType $ hm HM.!? name
  _ -> LigoType Nothing

-- | Extracts address and entry point from @EpAddress@.
-- Does nothing if entry point is the default one.
getEpAddressChildren :: Lang -> EpAddress -> [DAP.Variable]
getEpAddressChildren lang EpAddress'{..} =
  if isDefEpName eaEntrypoint
  then []
  else [addr, ep]
  where
    addr = createVariable "address" (pretty eaAddress) lang (LigoType Nothing) Nothing Nothing
    ep = createVariable "entrypoint" (pretty eaEntrypoint) lang (LigoType Nothing) Nothing Nothing

-- | Tries to build variable's children if the provided value
-- has some structure (e.g. it's a list, map, set, etc).
buildSubVars :: Lang -> LigoOrMichValue -> VariableBuilder [DAP.Variable]
buildSubVars lang = \case
  MichValue typ (SomeValue michValue) -> case michValue of
    VOption Nothing -> return []
    VOption (Just v) -> do
      -- Inner type is wrong here. It's hard to extract it here properly.
      (:[]) <$> buildVariableWithoutChildren (toLigoValue (getInnerTypeFromConstant 0 typ) v) "Some"
    VList lst -> do
      zipWithM (buildVariableWithoutChildren . toLigoValue (getInnerTypeFromConstant 0 typ)) lst (show <$> [1 :: Int ..])
    VSet s -> do
      zipWithM (buildVariableWithoutChildren . toLigoValue (getInnerTypeFromConstant 0 typ)) (toList s) (show <$> [1 :: Int ..])
    VMap m -> do
      forM (toPairs m) \(k, v) -> do
        let name = pretty $ debugBuild DpmNormal k
        buildVariableWithoutChildren (toLigoValue (getInnerTypeFromRecord name typ) v) name
    VBigMap _id m -> do
      forM (toPairs m) \(k, v) -> do
        let name = pretty $ debugBuild DpmNormal k
        buildVariableWithoutChildren (toLigoValue (getInnerTypeFromRecord name typ) v) name
    VContract eaAddress (SomeEpc EntrypointCall{ epcName = eaEntrypoint }) -> do
      pure $ getEpAddressChildren lang EpAddress'{..}
    VAddress epAddress -> pure $ getEpAddressChildren lang epAddress
    -- Other value types do not have nested structure
    _ -> return []
  LigoValue typ ligoValue -> case ligoValue of
    LVCt (LCContract LigoContract{..})
      | Just entrypoint <- lcEntrypoint -> do
          let addr = createVariable "address" lcAddress lang (LigoType Nothing) Nothing Nothing
          let ep = createVariable "entrypoint" entrypoint lang (LigoType Nothing) Nothing Nothing
          return [addr, ep]
      | otherwise -> return []
    LVList lst ->
      let innerType = getInnerTypeFromConstant 0 typ in
      zipWithM (buildVariableWithoutChildren . LigoValue innerType) lst (show <$> [1 :: Int ..])
    value@(LVRecord record) -> case toTupleMaybe value of
      Just values ->
        zipWithM
          do \val n ->
              let innerType = getInnerTypeFromRecord n typ in
              buildVariableWithoutChildren (LigoValue innerType val) n
          values
          (show <$> [1 :: Int ..])
      Nothing -> do
        forM (toPairs record) \(LLabel name, v) -> do
          let innerType = getInnerTypeFromRecord name typ
          buildVariableWithoutChildren (LigoValue innerType v) name
    LVConstructor (ctor, value) ->
      let innerType = getInnerTypeFromSum ctor typ in
      (:[]) <$> buildVariableWithoutChildren (LigoValue innerType value) ctor
    LVSet s ->
      let innerType = getInnerTypeFromConstant 0 typ in
      zipWithM (buildVariableWithoutChildren . LigoValue innerType) s (show <$> [1 :: Int ..])
    LVMap m -> do
      forM m \(k, v) -> do
        let keyType = getInnerTypeFromConstant 0 typ
        let valueType = getInnerTypeFromConstant 1 typ

        let name = pretty @_ @Text $ debugBuild DpmNormal (lang, LigoValue keyType k)
        buildVariableWithoutChildren (LigoValue valueType v) name
    _ -> return []
  _ -> return []
  where
    toLigoValue :: (SingI t) => LigoType -> Value t -> LigoOrMichValue
    toLigoValue typ = MichValue typ . SomeValue

    buildVariableWithoutChildren :: LigoOrMichValue -> Text -> VariableBuilder DAP.Variable
    buildVariableWithoutChildren val name = buildVariable lang (PreConvertVariable name val Nothing)
