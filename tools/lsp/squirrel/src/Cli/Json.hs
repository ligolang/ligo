-- | ligo version: 0.51.0
-- | The definition of type as is represented in ligo JSON output

{-# LANGUAGE DeriveGeneric #-}

module Cli.Json
  ( LigoError (..)
  , LigoErrorContent (..)
  , LigoMessages (..)
  , LigoScope (..)
  , LigoDefinitions (..)
  , LigoDefinitionsInner (..)
  , LigoVariableDefinitionScope (..)
  , LigoTypeDefinitionScope (..)
  , LigoTypeFull (..)
  , LigoTypeContent (..)
  , LigoTypeExpression (..)
  , LigoTableField (..)
  , LigoRange (..)
  , LigoRangeInner (..)
  , LigoByte (..)
  , mbFromLigoRange
  , fromLigoRangeOrDef
  , fromLigoErrorToMsg
  , fromLigoTypeFull
  , mkLigoError
  )
where

import Control.Monad.State
import Data.Aeson.Types hiding (Error)
import Data.Aeson.KeyMap (toAscList)
import Data.Char (isUpper, toLower)
import Data.Foldable (toList)
import Data.Function
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits (Nat, KnownNat, natVal)
import Language.LSP.Types qualified as J
import Prelude hiding (sum)

import AST.Skeleton hiding (CString)
import Diagnostic (Message (..), MessageDetail (FromLIGO), Severity (..))
import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree
import Parser (CodeSource (..), Info)
import Product
import Range hiding (startLine)

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- | Node representing ligo error with additional meta
data LigoError = LigoError
  { -- | `"status"`
    _leStatus  :: Text
    -- | Stage on where the error appeared (parser/typechecker)
    -- `"stage"`
  , _leStage   :: Text
    -- | Error message block
    -- `"content"`
  , _leContent :: LigoErrorContent
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON) via LigoJSON 2 LigoError

-- | An actual ligo error
data LigoErrorContent = LigoErrorContent
  { -- | Error message
    -- `"message"`
    _lecMessage  :: Text
    -- | Location of the error
    -- `"location"`
  , _lecLocation :: Maybe LigoRange
  }
  deriving stock (Eq, Generic, Show)

-- | The output for 'ligo info get-scope' may return with a list of errors and a
-- list of warnings.
data LigoMessages = LigoMessages
  { -- | `"errors"`
    _lmErrors   :: NonEmpty LigoError
    -- | `"warnings"`
  , _lmWarnings :: [LigoError]
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON) via LigoJSON 2 LigoMessages

-- | Whole successfull ligo `get-scope` output
data LigoDefinitions = LigoDefinitions
  { -- | Errors produced by LIGO
    -- `"errors"`
    _ldErrors      :: [LigoError]
    -- | Warnings produced by LIGO
    -- `"warnings"`
  , _ldWarnings    :: [LigoError]
    -- | All the definitions
    -- `"definitions"`
  , _ldDefinitions :: LigoDefinitionsInner -- it is optional
    -- | Scopes
    -- `"scopes"`
  , _ldScopes      :: [LigoScope] -- it is optional
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 2 LigoDefinitions

-- | First part under `"variables"` constraint
data LigoDefinitionsInner = LigoDefinitionsInner
  { -- | `"variables"`
    _ldiVariables :: HM.HashMap Text LigoVariableDefinitionScope
    -- | `"types"`
  , _ldiTypes     :: HM.HashMap Text LigoTypeDefinitionScope
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 3 LigoDefinitionsInner

-- | Scope that goes as a member of the list under `"scopes"` constraint
-- ```
-- { "scopes" : [LigoScope] }
-- ```
data LigoScope = LigoScope
  { -- | We parse it by a chunks of 2, each odd element of array is a name for
    -- the corresponding element which is `LigoRangeInner`.
    -- ```
    -- { "range": [ "<scope>", LigoRangeInner ] }
    -- ```
    _lsRange                 :: LigoRange
    -- | `"expression_environment"`
  , _lsExpressionEnvironment :: [Text]
    -- | `"type_environment"`
  , _lsTypeEnvironment       :: [Text]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 2 LigoScope

-- | Definition declaration that goes from `"definitions"` constraint

data LigoVariableDefinitionScope = LigoVariableDefinitionScope
  { -- | `"name"`
    _lvdsName       :: Text
    -- | Binding location
    -- `"location"`
  , _lvdsRange      :: LigoRange
    -- | Definition body location
    -- `"body_location"`
  , _lvdsBodyRange  :: LigoRange
    -- | The type itself
    -- `"t"`
  , _lvdsT          :: LigoTypeFull
    -- | We parse it in chunks of 2, each odd element of the array is a name for
    -- the corresponding element which is `LigoRangeInner`.
    -- ```
    -- { "references": [ ["<scope>", LigoRangeInner] ] }
    -- ```
    -- `"references"`
  , _lvdsReferences :: [LigoRange]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 4 LigoVariableDefinitionScope

data LigoTypeDefinitionScope = LigoTypeDefinitionScope
  { -- | `"name"`
    _ltdsName      :: Text
    -- | Binding location
    -- `"location"`
  , _ltdsRange     :: LigoRange
    -- | Definition body location
    -- `"body_location"`
  , _ltdsBodyRange :: LigoRange
  , _ltdsContent   :: LigoTypeExpression
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 4 LigoTypeDefinitionScope

-- | Parameter of a type
-- ```
-- { "parameters": [LigoTypeParameter] }
-- ```
data LigoTypeExpression = LigoTypeExpression
  { -- | We parse it by a chunks of 2, each odd element of array is a name for
    -- even element which is `LigoTypeContentInner`.
  ---- Common for 4th and 5th stage
    -- ```
    -- { "type_content": [ <name>, LigoTypeContentInner ] }
    -- ```
    _lteTypeContent :: LigoTypeContent
    -- | `"location"`
  , _lteLocation    :: LigoRange
  ---- 4th stage specific
  , _lteSugar       :: Maybe Value
  ---- 5th stage specific
      -- | `"type_meta"`
  , _lteTypeMeta    :: Maybe LigoTypeExpression
    -- | `"orig_var"`
  , _lteOrigVar     :: Maybe LigoTypeVariable
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 3 LigoTypeExpression

-- | Whole ligo type.
-- ```
-- { "t" : LigoTypeFull }
-- ```
data LigoTypeFull
  = LTFCore LigoTypeExpression
  | LTFResolved LigoTypeExpression
  | LTFUnresolved
  deriving stock (Generic, Show)

-- | Inner object representing type content that depends on `name` in `LigoTypeContent`.
-- ```
-- { "type_content": [ <type>, LigoTypeContentInner ] }
-- ```
data LigoTypeContent
  = -- | Type call represented by the list of arguments and its constructor.
  ---- Common for 4th and 5th stage
    -- | `"t_variable"`
    LTCVariable LigoTypeVariable
  |
    LTCSum LigoTypeSum
  | -- | `"t_record"`
    LTCRecord LigoTypeRecord
  | -- | `"t_arrow"`
    LTCArrow LigoTypeArrow
  |
    LTCSingleton Value -- TODO not used
  |
    LTCAbstraction LigoTypeForAll
  |
    LTCForAll LigoTypeForAll
  ---- 4th stage specific
  |
    LTCApp LigoTypeApp
  |
    LTCModuleAccessor LigoTypeModuleAccessor
  ---- 5th stage specific
  | -- `"t_constant"`
    LTCConstant LigoTypeConstant
  deriving stock (Generic, Show)

data LigoTypeApp = LigoTypeApp
  { _ltaTypeOperator :: LigoTypeVariable
  , _ltaArguments    :: [LigoTypeExpression]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 3 LigoTypeApp

data LigoTypeModuleAccessor = LigoTypeModuleAccessor
  { _ltmaModuleName :: Value -- TODO not used
  , _ltmaElement    :: Value -- TODO not used
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 4 LigoTypeModuleAccessor

type LigoTypeSum = LigoTypeTable
type LigoTypeRecord = LigoTypeTable

data LigoTypeTable = LigoTypeTable
  { _lttFields :: HM.HashMap Text LigoTableField
  , _lttLayout  :: Value -- TODO not used
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 3 LigoTypeTable

data LigoTypeConstant = LigoTypeConstant
  { _ltcParameters :: [LigoTypeExpression]
  , _ltcLanguage   :: Text
  , _ltcInjection  :: NonEmpty Text
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 3 LigoTypeConstant

data LigoTypeArrow = LigoTypeArrow
  -- "type2" -> "type1"
  { _ltaType2 :: LigoTypeExpression
  , _ltaType1 :: LigoTypeExpression
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 3 LigoTypeArrow

data LigoTypeVariable = LigoTypeVariable
  { _ltvName      :: Text
  , _ltvCounter   :: Int
  , _ltvGenerated :: Bool
  , _ltvLocation  :: LigoRange
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 3 LigoTypeVariable

data LigoTypeForAll = LigoTypeForAll
  { _ltfaTyBinder :: LigoTypeVariable
  , _ltfaType_    :: LigoTypeExpression
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 4 LigoTypeForAll

-- | Record field type value.
-- ```
-- { "type_content": ["T_record", { "key": LigoTableField } ] }
-- ```
data LigoTableField = LigoTableField
  { -- | Declaration position (don't ask me I too don't know what actual
    -- position is this since from all the example it's somewhat always 0).
    _ltfDeclPos        :: Int
    -- | How the value is represented in michelson, currently ignored
    -- during parsing.
  , _lrfMichelsonAnnotation :: Value
  , -- | The type itself.
    _ltfAssociatedType :: LigoTypeExpression
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 3 LigoTableField

-- | Location of definition.
-- ```
-- { "location": LigoRange }
-- ```
data LigoRange
  = LRVirtual Text
  | LRFile LigoFileRange
  deriving stock (Eq, Generic, Show)

data LigoFileRange = LigoFileRange
  { _lfrStart :: LigoRangeInner
  , _lfrStop  :: LigoRangeInner
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON) via LigoJSON 3 LigoFileRange

-- | Insides of ligo location.
-- ```
-- { ["start" | "stop"]: LigoRangeInner }
-- ```
data LigoRangeInner = LigoRangeInner
  { _lriByte     :: LigoByte
  , _lriPointNum :: J.UInt
  , _lriPointBol :: J.UInt
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON) via LigoJSON 3 LigoRangeInner

-- | Byte representation of ligo location.
-- ```
-- { "byte": LigoByte }
-- ```
data LigoByte = LigoByte
  { _lbPosFname :: FilePath
  , _lbPosLnum  :: J.UInt
  , _lbPosBol   :: J.UInt
  , _lbPosCnum  :: J.UInt
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON) via LigoJSON 2 LigoByte

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

newtype LigoJSON (n :: Nat) a = LigoJSON a

instance forall n a. (Generic a, GFromJSON Zero (Rep a), KnownNat n) => FromJSON (LigoJSON n a) where
  parseJSON = fmap LigoJSON . genericParseJSON defaultOptions
    { fieldLabelModifier =
      drop (fromInteger (natVal $ Proxy @n) + 2)
      . toSnakeCase
    }

-- Sometimes LigoErrorContent is just a String
instance FromJSON LigoErrorContent where
  parseJSON (String t) = pure $ LigoErrorContent t Nothing
  parseJSON o = genericParseJSON defaultOptions{fieldLabelModifier = drop 5 . toSnakeCase} o

-- { "core" : ... }
instance FromJSON LigoTypeFull where
  parseJSON = withObject "LigoTypeFull" \o -> case toAscList o of
    [("core"      , value)] -> LTFCore     <$> parseJSON value
    [("resolved"  , value)] -> LTFResolved <$> parseJSON value
    [("unresolved", Null )] -> pure LTFUnresolved
    _ -> fail "wrong `LigoTypeFull` format"

-- [ "t_variable", ...]
instance FromJSON LigoTypeContent where
  parseJSON = genericParseJSON defaultOptions
    { sumEncoding = TwoElemArray
    -- "LTCVariable" -> "t_variable"
    , constructorTagModifier = ('T' :) . toSnakeCase . drop 3
    }

-- [ "Virtual", ...]
instance FromJSON LigoRange where
  parseJSON = genericParseJSON defaultOptions
    { sumEncoding = TwoElemArray
    -- "LRVirtual" -> "Virtual"
    , constructorTagModifier = drop 2
    }

----------------------------------------------------------------------------
-- Pretty
----------------------------------------------------------------------------

instance Pretty LigoError where
  pp (LigoError status stage (LigoErrorContent msg at)) = mconcat
    [ pp status <+> " in ", text $ show stage
    , case at of
        Nothing -> mempty
        Just at' -> text "\n\nat: " <> pp (fromLigoRangeOrDef at')
    , text "\n\n" <> pp msg
    ]

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Converts string to snake_case
toSnakeCase :: String -> String
toSnakeCase = foldr helper []
  where
    helper c acc
      | isUpper c = '_' : toLower c : acc
      | otherwise = c : acc

-- | Convert ligo error to its corresponding internal representation.
fromLigoErrorToMsg :: LigoError -> Message
fromLigoErrorToMsg LigoError
  { _leContent = LigoErrorContent
      { _lecMessage = err
      , _lecLocation = fmap fromLigoRangeOrDef -> at
      }
  , _leStatus
  } = Message (FromLIGO err) status (fromMaybe (point 0 0) at)
  where
    status = case _leStatus of
      "error"   -> SeverityError
      "warning" -> SeverityWarning
      _         -> SeverityError

-- | Converts ligo ranges to our internal ones.
-- Note: ligo team allows for start file of a range be different from end file.
-- Either if this intentional or not we throw an error if they are so.
-- >>> :{
-- mbFromLigoRange
--   (LigoRange
--     (LigoRangeInner (LigoByte "contracts/test.ligo" 2 undefined undefined) 3 6)
--     (LigoRangeInner (LigoByte "contracts/test.ligo" 5 undefined undefined) 11 12)
--   )
-- :}
-- contracts/test.ligo:2:4-5:2
mbFromLigoRange :: LigoRange -> Maybe Range
mbFromLigoRange (LRVirtual _) = Nothing
mbFromLigoRange
  (LRFile
    (LigoFileRange
      (LigoRangeInner LigoByte { _lbPosLnum = startLine , _lbPosFname = startFilePath } startCNum startBol)
      (LigoRangeInner LigoByte { _lbPosLnum = endLine   , _lbPosFname = endFilePath   } endCNum   endBol)
    )
  )
  | startFilePath /= endFilePath = error "start file of a range does not equal to its end file"
  | otherwise = Just Range
      { _rStart = (startLine, abs (startCNum - startBol) + 1, 0)
      , _rFinish = (endLine, abs (endCNum - endBol) + 1, 0)
      , _rFile = startFilePath
      }

fromLigoRangeOrDef :: LigoRange -> Range
fromLigoRangeOrDef = fromMaybe (point 0 0) . mbFromLigoRange

data FieldKind = FieldSum | FieldProduct

-- | Reconstruct `LIGO` tree out of `LigoTypeFull`.
fromLigoTypeFull :: LigoTypeFull -> LIGO Info
fromLigoTypeFull = enclose . \case
  LTFCore lte     -> fromLigoTypeExpression lte
  LTFResolved lte -> fromLigoTypeExpression lte
  LTFUnresolved   -> mkErr "unresolved type given"
  where

    fromLigoPrimitive :: Maybe FieldKind -> Text -> State (Product Info) (LIGO Info)
    fromLigoPrimitive fieldKind p = do
      st <- get
      return case fieldKind of
        Just FieldSum     -> make' (st, Ctor p)
        Just FieldProduct -> make' (st, FieldName p)
        Nothing           -> make' (st, TypeName p)

    fromLigoTypeExpression
      LigoTypeExpression {..} = do
        modify . putElem . fromLigoRangeOrDef $ _lteLocation
        fromLigoType _lteTypeContent

    fromLigoConstant name [] = fromLigoPrimitive Nothing name
    fromLigoConstant name params = do
      st <- get
      n <- fromLigoPrimitive Nothing name
      p <- sequence $ fromLigoTypeExpression <$> params
      return $ make' (st, TApply n p)

    fromLigoType
      :: LigoTypeContent
      -> State (Product Info) (LIGO Info)
    fromLigoType = \case
      LTCConstant LigoTypeConstant {..} ->
        fromLigoConstant (NE.head _ltcInjection) _ltcParameters

      LTCVariable variable ->
        fromLigoPrimitive Nothing $ _ltvName variable

      LTCRecord record -> do
        st <- get
        record' <- fromLigoTable FieldProduct record
        return $ make' (st, TRecord record')

      LTCSum sum -> do
        st <- get
        sum' <- fromLigoTable FieldSum sum
        case sum' of
          [] -> mkErr "malformed sum type, please report this as a bug"
          v : vs -> pure $ make' (st, TSum (v :| vs))

      LTCSingleton      _ -> mkErr "unsupported type `Singleton`"      -- TODO not used
      LTCAbstraction    _ -> mkErr "unsupported type `Abstraction`"    -- TODO not used
      LTCForAll         _ -> mkErr "unsupported type `ForAll`"         -- TODO not used
      LTCModuleAccessor _ -> mkErr "unsupported type `ModuleAccessor`" -- TODO not used

      LTCApp LigoTypeApp{..} -> do
        st <- get
        p <- fromLigoPrimitive Nothing (_ltvName _ltaTypeOperator)
        return . make' . (st,) $
          TApply p (enclose . fromLigoTypeExpression <$> _ltaArguments)

      LTCArrow LigoTypeArrow {..} -> do
        st <- get
        let mkArrow = TArrow `on` (enclose . fromLigoTypeExpression)
        return $ make' (st, mkArrow _ltaType1 _ltaType2)

    fromLigoTable fieldKind =
      traverse (uncurry (fromLigoTableField fieldKind)) . HM.toList . _lttFields

    fromLigoTableField
      :: FieldKind
      -> Text
      -> LigoTableField
      -> State (Product Info) (LIGO Info)
    fromLigoTableField fieldKind name LigoTableField {..} = do
      st <- get
      n <- fromLigoPrimitive (Just fieldKind) name
      -- FIXME: Type annotation is optional.
      let type' = Just $ enclose $ fromLigoTypeExpression _ltfAssociatedType
      return case fieldKind of
        FieldSum     -> make' (st, Variant n type')
        FieldProduct -> make' (st, TField  n type')

    mkErr = gets . flip mkLigoError

    enclose
      :: State (Product Info) (LIGO Info)
      -> LIGO Info
    enclose = flip evalState defaultState

    defaultState :: Product Info
    defaultState = [] :> [] :> point 1 1 :> CodeSource "" :> Nil

mkLigoError :: Product Info -> Text -> LIGO Info
mkLigoError p msg = make' . (p,) $ Error (FromLIGO msg) []

-- | Variant of `make` that constructs a tree out of annotation and node
-- that recovers range from previous subnodes by merging them, this helps to
-- reconstruct `["Virtual", "generated"]` types out of their subnodes which
-- by some onorthodox opportunity may have proper ranges.
make'
  :: forall fs f .
     ( Element f fs
     , Foldable f
     , Apply Functor fs
     ) => (Product Info, f (Tree fs (Product Info)))
       -> Tree fs (Product Info)
make' (i, f)
  | null ges = i :< inject f
  | otherwise = i' :< inject f
  where
    ges = List.filter (not . (`leq` i)) (extract <$> toList f)
    r = getElem (List.minimum ges) `merged` getElem (List.maximum ges)
    i' = putElem r i
