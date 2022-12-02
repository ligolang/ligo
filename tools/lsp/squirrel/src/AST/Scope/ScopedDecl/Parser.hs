module AST.Scope.ScopedDecl.Parser
  ( parseType
  , parseTypeDeclSpecifics
  , parseQuotedTypeParams
  , parseParameters
  , parseModule
  ) where

import Prelude hiding (Type)

import Control.Lens ((??))
import Data.Coerce (coerce)
import Duplo.Tree (layer, match)
import Witherable (wither)

import AST.Pretty (PPableLIGO, lppDialect, ppToText)
import AST.Scope.ScopedDecl
import AST.Skeleton (LIGO, Lang)
import AST.Skeleton qualified as LIGO
import Product (Contains)
import Range (Range, getRange)

-- * Parsers for type.

type Parser = Reader Lang

parseTypeDeclSpecifics :: PPableLIGO info => LIGO info -> Parser (TypeDeclSpecifics Type)
parseTypeDeclSpecifics node = do
  _tdsInit <- parseType node
  pure TypeDeclSpecifics
    { _tdsInitRange = getRange node
    , _tdsInit
    }

-- The node is _always_ parsed as some type. In the worst case — if the node is
-- not a type, it's parsed as an alias type with the node textual representation
-- as its content.
--
-- Also see 'parseAliasType'.
parseType :: PPableLIGO info => LIGO info -> Parser Type
parseType node = do
  typM <- runMaybeT $ asum $ coerce @_ @[MaybeT _ _] (parsers ?? node)
  maybe (parseAliasType node) pure typM
  where
    parsers =
      [ parseRecordType
      , parseVariantType
      , parseTupleType
      , parseApplyType
      , parseArrowType
      , parseVariableType
      , parseParenType
      ]

parseParenType :: PPableLIGO info => LIGO info -> Parser (Maybe Type)
parseParenType node = runMaybeT do
  LIGO.TParen typ <- hoistMaybe $ layer node
  typ' <- lift $ parseTypeDeclSpecifics typ
  pure $ ParenType typ'

parseArrowType :: PPableLIGO info => LIGO info -> Parser (Maybe Type)
parseArrowType node = runMaybeT do
  LIGO.TArrow left right <- hoistMaybe $ layer node
  left'  <- lift $  parseType left
  right' <- lift $  parseType right
  pure $ ArrowType left' right'

parseApplyType :: PPableLIGO info => LIGO info -> Parser (Maybe Type)
parseApplyType node = runMaybeT do
  LIGO.TApply name types <- hoistMaybe $ layer node
  name' <- lift $ parseType name
  types' <- lift $ traverse parseType types
  pure $ ApplyType name' types'

parseRecordType :: PPableLIGO info => LIGO info -> Parser (Maybe Type)
parseRecordType node = runMaybeT do
  LIGO.TRecord fieldNodes <- hoistMaybe $ layer node
  typeFields <- lift $ wither parseTypeField fieldNodes
  pure (RecordType typeFields)

parseTypeField :: PPableLIGO info => LIGO info -> Parser (Maybe TypeField)
parseTypeField node = runMaybeT do
  LIGO.TField nameNode typNode <- hoistMaybe $ layer node
  LIGO.FieldName _tfName <- hoistMaybe $ layer nameNode
  _tfTspec <- lift $ traverse parseTypeDeclSpecifics typNode
  pure TypeField{ .. }

parseVariantType :: PPableLIGO info => LIGO info -> Parser (Maybe Type)
parseVariantType node = runMaybeT do
  LIGO.TSum conNodes <- hoistMaybe $ layer node
  cons <- MaybeT $ fmap nonEmpty $ wither parseTypeConstructor $ toList conNodes
  pure (VariantType cons)

parseTypeConstructor :: PPableLIGO info => LIGO info -> Parser (Maybe TypeConstructor)
parseTypeConstructor node = runMaybeT do
  LIGO.Variant conNameNode conTypNode <- hoistMaybe $ layer node
  LIGO.Ctor _tcName <- hoistMaybe $ layer conNameNode
  _tcTspec <- lift $ traverse parseTypeDeclSpecifics conTypNode
  pure TypeConstructor{ .. }

parseTupleType :: PPableLIGO info => LIGO info -> Parser (Maybe Type)
parseTupleType node = runMaybeT do
  LIGO.TProduct elementNodes <- hoistMaybe $ layer node
  elements <- lift $ traverse parseTypeDeclSpecifics elementNodes
  pure (TupleType elements)

parseVariableType :: Contains Range info => LIGO info -> Parser (Maybe Type)
parseVariableType node = runMaybeT do
  var <- MaybeT $ parseTypeVariable node
  pure $ VariableType var

-- Since we don't care right now about distinguishing functions or whatever, we
-- just treat the whole node as a type name. It _is_ possible that the node is
-- not even a type: it could be an error node. However we choose to fail, we'll
-- lose the whole type structure instead of this one leaf.
parseAliasType :: PPableLIGO info => LIGO info -> Parser Type
parseAliasType node = do
  dialect <- ask
  pure $ AliasType $ ppToText $ lppDialect dialect node

-- * Parsers for type variables.

parseQuotedTypeParams :: Contains Range info => LIGO info -> Parser (Maybe QuotedTypeParams)
parseQuotedTypeParams node = runMaybeT $ asum
  [ do
      LIGO.QuotedTypeParam t <- hoistMaybe $ layer node
      QuotedTypeParam <$> MaybeT (parseTypeVariableSpecifics t)
  , do
      LIGO.QuotedTypeParams ts <- hoistMaybe $ layer node
      QuotedTypeParams <$> lift (wither parseTypeVariableSpecifics ts)
  ]

parseTypeVariableSpecifics
  :: Contains Range info
  => LIGO info
  -> Parser (Maybe (TypeDeclSpecifics TypeVariable))
parseTypeVariableSpecifics node = runMaybeT do
  LIGO.TVariable var <- hoistMaybe $ layer node
  (r, LIGO.TypeVariableName name) <- hoistMaybe $ match var
  pure TypeDeclSpecifics
    { _tdsInitRange = getRange r
    , _tdsInit = TypeVariable name
    }

parseTypeVariable :: Contains Range info => LIGO info -> Parser (Maybe TypeVariable)
parseTypeVariable = fmap (fmap _tdsInit) . parseTypeVariableSpecifics

-- * Parsers for parameters.

parseParameter :: PPableLIGO info => LIGO info -> Parser (Maybe Parameter)
parseParameter node = runMaybeT $ asum $ coerce @_ @[MaybeT _ _] (parsers ?? node)
  where
    parsers =
      [ fmap (fmap ParameterPattern) . parsePattern
      , parseBParameter
      ]

parseBParameter :: PPableLIGO info => LIGO info -> Parser (Maybe Parameter)
parseBParameter node = runMaybeT do
  LIGO.BParameter name typM <- hoistMaybe $ layer node
  pat <- MaybeT $ parsePattern name
  case typM of
    Nothing -> pure $ ParameterBinding pat Nothing
    Just typ -> ParameterBinding pat <$> lift (Just <$> parseType typ)

parseParameters :: PPableLIGO info => [LIGO info] -> Parser [Parameter]
parseParameters = wither parseParameter

-- * Parsers for patterns.

parseConstant :: LIGO info -> Parser (Maybe Constant)
parseConstant node = pure $ layer node <&> \case
  LIGO.CInt    i -> CInt    i
  LIGO.CNat    n -> CNat    n
  LIGO.CString s -> CString s
  LIGO.CFloat  f -> CFloat  f
  LIGO.CBytes  b -> CBytes  b
  LIGO.CTez    t -> CTez    t

parsePattern :: PPableLIGO info => LIGO info -> Parser (Maybe Pattern)
parsePattern node = do
  dialect <- ask
  runMaybeT $ hoistMaybe (layer node) >>= \case
    LIGO.IsConstr name patM -> do
      let name' = ppToText $ lppDialect dialect name
      case patM of
        Nothing -> pure $ IsConstr name' Nothing
        Just pat -> lift $ IsConstr name' <$> parsePattern pat
    LIGO.IsConstant constant -> IsConstant <$> MaybeT (parseConstant constant)
    LIGO.IsVar name -> pure $ IsVar $ ppToText $ lppDialect dialect name
    LIGO.IsCons left right -> IsCons <$> MaybeT (parsePattern left) <*> MaybeT (parsePattern right)
    LIGO.IsAnnot pat typ -> IsAnnot <$> MaybeT (parsePattern pat) <*> lift (parseType typ)
    LIGO.IsWildcard -> pure IsWildcard
    LIGO.IsSpread pat -> IsSpread <$> MaybeT (parsePattern pat)
    LIGO.IsList pats -> IsList <$> lift (wither parsePattern pats)
    LIGO.IsTuple pats -> IsTuple <$> lift (wither parsePattern pats)
    LIGO.IsRecord pats -> IsRecord <$> lift (wither parseRecordFieldPattern pats)
    LIGO.IsParen pat -> IsParen <$> MaybeT (parsePattern pat)

parseRecordFieldPattern :: PPableLIGO info => LIGO info -> Parser (Maybe RecordFieldPattern)
parseRecordFieldPattern node = do
  dialect <- ask
  runMaybeT $ hoistMaybe (layer node) >>= \case
    LIGO.IsRecordField name body -> IsRecordField (ppToText $ lppDialect dialect name) <$> MaybeT (parsePattern body)
    LIGO.IsRecordCapture name -> pure $ IsRecordCapture (ppToText $ lppDialect dialect name)

-- * Parsers for modules.

-- TODO: We only parse aliases for now.
parseModule :: [LIGO info] -> Parser Module
parseModule =
  pure
  . maybe ModuleDecl (ModuleAlias . Namespace . fromList . coerce)
  . traverse (layer @LIGO.ModuleName)
