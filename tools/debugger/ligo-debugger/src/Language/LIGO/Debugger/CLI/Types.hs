{-# LANGUAGE DeriveDataTypeable, StandaloneKindSignatures, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types coming from @ligo@ executable.
module Language.LIGO.Debugger.CLI.Types
  ( module Language.LIGO.Debugger.CLI.Types
  ) where

import Prelude hiding (Element, Product (..), sum)

import Control.Lens (AsEmpty (..), Each (each), _head, makePrisms, prism)
import Control.Lens.Prism (_Just)
import Control.MessagePack
  (asumMsg, guardMsg, withMsgArray, withMsgMap, withMsgText, withMsgVariant, wrapMonadFail, (.!=),
  (.:!), (.:), (.:?))
import Control.Monad.Validate (MonadValidate, refute)
import Data.Aeson
  (FromJSON (..), Options (constructorTagModifier, fieldLabelModifier, sumEncoding),
  SumEncoding (TwoElemArray), Value (..), defaultOptions, genericParseJSON, withArray)
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.Data (Data)
import Data.Default (Default (..))
import Data.Foldable qualified
import Data.HashMap.Strict qualified as HM
import Data.List (nub)
import Data.List qualified as List
import Data.Map qualified as M
import Data.MessagePack
  (DecodeError, MessagePack (fromObjectWith, toObject), Object (..), decodeError)
import Data.Singletons.TH (SingI (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug qualified
import Fmt.Buildable (Buildable, blockListF, build, nameF, pretty, unlinesF)
import Fmt.Utils (Doc)
import System.Console.ANSI
  (Color (Red), ColorIntensity (Dull), ConsoleIntensity (BoldIntensity), ConsoleLayer (Foreground),
  SGR (Reset, SetColor, SetConsoleIntensity))
import Text.Hex (decodeHex)
import Text.Interpolation.Nyan (int)
import Util

import Morley.Micheline
  (Annotation, MichelinePrimAp (..), MichelinePrimitive, StringEncode (..), TezosBigNum,
  annotFromText)
import Morley.Micheline.Expression (Exp (..))
import Morley.Micheline.Expression qualified as Micheline
import Morley.Util.ByteString (HexJSONByteString (..))
import Morley.Util.Lens

import Duplo
  (Apply, Cofree ((:<)), Comonad (extract), Element, Lattice (leq), Tree, inject, layer, text)

import Language.LIGO.AST.Pretty hiding (Doc)
import Language.LIGO.AST.Skeleton hiding (ModuleName, Name)
import Language.LIGO.AST.Skeleton qualified as AST
import Language.LIGO.Debugger.CLI.Helpers
import Language.LIGO.Range
import Language.LIGO.Scope

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- | Describes a variable.
newtype LigoVariable (u :: NameType) = LigoVariable
  { lvName :: Name u -- ^ Variable's name
  } deriving stock (Show, Generic, Data)
    deriving newtype (Buildable)
    deriving anyclass (NFData)

deriving stock instance Eq (LigoVariable 'Concise)

-- | Reference to type description in the types map.
newtype LigoTypeRef = LigoTypeRef
  { unLigoTypeRef :: Int -- ^ Position in @LigoTypesVec@.
  }
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (NFData)

-- | Inner object representing type content that depends on `name` in `LigoTypeContent`.
-- ```
-- { "type_content": [ <type>, LigoTypeContentInner ] }
-- ```
data LigoTypeContent
    -- | Type call represented by the list of arguments and its constructor.
    -- Common for 4th and 5th stage
    --
    -- @T_variable@
  = LTCVariable Text
  | LTCSum LigoTypeSum
    -- | @T_record@
  | LTCRecord LigoTypeRecord
    -- | @T_arrow@
  | LTCArrow LigoTypeArrow
    -- | @T_singleton@
  | LTCSingleton LigoTypeLiteralValue
    -- | 5th stage specific
    -- @T_constant@
  | LTCConstant LigoTypeConstant
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

-- | A singleton LIGO type.
data LigoTypeLiteralValue
  = LTLVUnit
    -- ^ Unit type.
  | LTLVInt Int
    -- ^ Int type.
  | LTLVNat Int
    -- ^ Nat type.
  | LTLVTimestamp Int
    -- ^ Timestamp type.
  | LTLVMutez Int
    -- ^ Mutez type.
  | LTLVString LigoString
    -- ^ String type.
  | LTLVBytes Text
    -- ^ Bytes type.
  | LTLVAddress Text
    -- ^ Address type.
  | LTLVSignature Text
    -- ^ Signature type.
  | LTLVKey Text
    -- ^ Key type.
  | LTLVKeyHash Text
    -- ^ Key hash type.
  | LTLVChainId Text
    -- ^ Chain ID type.
  | LTLVOperation Text
    -- ^ Operation type.
  | LTLVBls_381G1 Text
    -- ^ Bls12_381_g1 type.
  | LTLVBls_381G2 Text
    -- ^ Bls12_381_g2 type.
  | LTLVBls_381Fr Text
    -- ^ Bls12_381_fr type.
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

-- | Represents a LIGO string.
data LigoString
  = LSStandard Text
    -- ^ Standard string (e.g. @"abacaba"@).
  | LSVerbatim Text
    -- ^ Verbatim string (e.g @{|abacaba|}@).
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

-- | A type alias for LIGO sum types.
type LigoTypeSum = LigoTypeTable

-- | A type alias for LIGO record types.
type LigoTypeRecord = LigoTypeTable

-- | A common type that represents LIGO sum/record types.
data LigoTypeTable = LigoTypeTable
  { _lttFields  :: HM.HashMap Text LigoTypeExpression
    -- ^ Map from constructor/field name to its type.
  , _lttLayout  :: LigoLayout
    -- ^ A sum/record type's layout.
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

-- | A type's layout represented by tree.
data LigoLayout
  = LLInner [LigoLayout]
    -- ^ Branch node with arbitrary amount of children.
  | LLField Text
    -- ^ Leaf node with constructor's/field's name.
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

-- | An inner field of @LTCConstant@.
data LigoTypeConstant = LigoTypeConstant
  { _ltcParameters :: [LigoTypeExpression]
    -- ^ Type parameters (e.g. in @(int, nat) map@
    -- these parameters would be @[int, nat]@).
  , _ltcInjection  :: Text
    -- ^ A type's injection. Usually it's Michelson.
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

-- | An inner field of @LTCArrow@.
data LigoTypeArrow = LigoTypeArrow
  -- "type2" -> "type1"
  { _ltaType2 :: LigoTypeExpression
    -- ^ Right-hand side type.
  , _ltaType1 :: LigoTypeExpression
    -- ^ Left-hand side type.
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

-- | Type that represents LIGO's variable.
data LigoVar = LigoVar
  { _lvName      :: Text
    -- ^ Variable's name.
  , _lvCounter   :: Int
    -- ^ Variable's counter.
  , _lvGenerated :: Bool
    -- ^ Is this variable generated?
  , _lvLocation  :: LigoRange
    -- ^ Variable's location in the source code.
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON) via LigoJSON 2 LigoVar

-- | Location of definition.
-- ```
-- { "location": LigoRange }
-- ```
data LigoRange
  = LRVirtual Text
    -- ^ Virtual location.
  | LRFile LigoFileRange
    -- ^ Concrete location.
  deriving stock (Eq, Generic, Show, Data)
  deriving anyclass (NFData, Hashable)

-- | An inner field of @LRFile@.
data LigoFileRange = LigoFileRange
  { _lfrStart :: LigoRangeInner
    -- ^ A starting point.
  , _lfrStop  :: LigoRangeInner
    -- ^ A finishing point.
  }
  deriving stock (Eq, Generic, Show, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON) via LigoJSON 3 LigoFileRange

-- | Insides of ligo location.
-- ```
-- { ["start" | "stop"]: LigoRangeInner }
-- ```
data LigoRangeInner = LigoRangeInner
  { _lriByte     :: LigoByte
    -- ^ A lexing byte-oriented position.
  , _lriPointNum :: Int
    -- ^ A number of code points since the beginning of the file.
  , _lriPointBol :: Int
    -- ^ A number of code points since the beginning of the current line.
  }
  deriving stock (Eq, Generic, Show, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON) via LigoJSON 3 LigoRangeInner

-- | Byte representation of ligo location.
-- ```
-- { "byte": LigoByte }
-- ```
data LigoByte = LigoByte
  { _lbPosFname :: FilePath
    -- ^ An origin file path.
  , _lbPosLnum  :: Int
    -- ^ A line number.
  , _lbPosBol   :: Int
    -- ^ An offset of the beginning of the line.
  , _lbPosCnum  :: Int
    -- ^ An offset of the position.
    -- The difference between @_lbPosCnum@ and @_lbPosBol@ is the column number.
  }
  deriving stock (Eq, Generic, Show, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON) via LigoJSON 2 LigoByte

-- | Parameter of a type
-- ```
-- { "parameters": [LigoTypeParameter] }
-- ```
data LigoTypeExpression = LigoTypeExpression
  { -- We parse it by a chunks of 2, each odd element of array is a name for
    -- even element which is `LigoTypeContentInner`.
  ---- Common for 4th and 5th stage
    -- ```
    -- { "type_content": [ <name>, LigoTypeContentInner ] }
    -- ```
    _lteTypeContent :: LigoTypeContent
    -- ^ Type expression's content.
  ---- 4th stage specific
  , _lteOrigVar     :: Maybe Text
    -- ^ Original name of the type, if exists.
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

-- | A convenient wrapper for the vector of LIGO types.
newtype LigoTypesVec = LigoTypesVec
  { unLigoTypesVec :: Vector LigoTypeExpression
    -- ^ LIGO types.
  }
  deriving stock (Data, Generic)
  deriving newtype (Show, NFData)

-- | A convenient wrapper for the LIGO type.
newtype LigoType = LigoType
  { unLigoType :: Maybe LigoTypeExpression
    -- ^ A LIGO type. Could be @Nothing@ if the meta
    -- from the source mapper is omitted.
  }
  deriving stock (Data)
  deriving newtype (Show, Generic, NFData, Hashable)

-- | A pattern for resolved LIGO types.
pattern LigoTypeResolved :: LigoTypeExpression -> LigoType
pattern LigoTypeResolved typ = LigoType (Just typ)

-- | A pattern for unresolved LIGO types.
pattern LigoTypeUnresolved :: LigoType
pattern LigoTypeUnresolved = LigoType Nothing

{-# COMPLETE LigoTypeResolved, LigoTypeUnresolved #-}

-- | A type family for LIGO types:
-- 1. For @Unique@ values we want to store only a @LigoTypeRef@
--    because we don't want to make them bloated.
--    Using @LigoTypesVec@ we could resolve them.
-- 2. For @Concise@ values we'll store a concrete type
--    since these values will only appear during the contract's interpretation.
type family LigoTypeF (u :: NameType) where
  LigoTypeF 'Unique  = Maybe LigoTypeRef
  LigoTypeF 'Concise = LigoType

-- | Prints type in Cameligo.
--
-- This is restricted for internal use only to avoid using it for producing
-- user-visible output, for such purpose 'buildType' with an appropriate
-- dialect must be used.
instance ForInternalUse => Buildable LigoType where
  build = allowedForInternalUseOnly $ buildType Caml

-- | An element of the stack with some information interesting for us.
data LigoExposedStackEntry u = LigoExposedStackEntry
  { leseDeclaration :: Maybe (LigoVariable u)
    -- ^ A variable's name.
  , leseType        :: LigoTypeF u
    -- ^ A variable's type.
  , leseFileName    :: Maybe FilePath
    -- ^ A variable's origin.
  } deriving stock (Generic)

deriving stock instance (Typeable u, Data (LigoTypeF u)) => Data (LigoExposedStackEntry u)
deriving stock instance (Show (LigoTypeF u)) => Show (LigoExposedStackEntry u)
deriving anyclass instance (NFData (LigoTypeF u)) => NFData (LigoExposedStackEntry u)
deriving stock instance Eq (LigoExposedStackEntry 'Concise)

-- | An element of the stack.
data LigoStackEntry u
  = LigoStackEntry (LigoExposedStackEntry u)
    -- ^ Stack entry with known details.
  | LigoHiddenStackEntry
    -- ^ Stack entry that is unknown.
    -- This can denote some auxiliary entry added by LIGO, like
    -- reusable functions or part of sum type when unfolding via @IF_LEFT@s.
  deriving stock (Generic)

deriving stock instance (Typeable u, Data (LigoTypeF u)) => Data (LigoStackEntry u)
deriving stock instance (Show (LigoTypeF u)) => Show (LigoStackEntry u)
deriving anyclass instance (NFData (LigoTypeF u)) => NFData (LigoStackEntry u)
deriving stock instance Eq (LigoStackEntry 'Concise)

-- | A pattern for @LigoStackEntry@ where only the type is known.
pattern LigoStackEntryNoVar :: LigoTypeF u -> LigoStackEntry u
pattern LigoStackEntryNoVar ty = LigoStackEntry LigoExposedStackEntry
  { leseDeclaration = Nothing
  , leseType = ty
  , leseFileName = Nothing
  }

-- | A pattern for @LigoStackEntry 'Concise@ where everything is known.
pattern LigoStackEntryVar :: Text -> LigoType -> Maybe FilePath -> LigoStackEntry 'Concise
pattern LigoStackEntryVar name ty fileName = LigoStackEntry LigoExposedStackEntry
  { leseDeclaration = Just LigoVariable{ lvName = Name name }
  , leseType = ty
  , leseFileName = fileName
  }

-- | Variables associated with each element on stack.
--
-- Entries are listed in top-to-bottom order.
type LigoStack u = [LigoStackEntry u]

-- | An application meta from the source mapper.
data LigoApplication u = LigoApplication
  { laAppliedFunction :: Maybe (LigoVariable u)
    -- ^ A name of applied function.
  , laArguments :: [LigoApplicationArgument u]
    -- ^ Applied arguments.
  } deriving stock (Generic)

deriving stock instance (Typeable u, Data (LigoTypeF u)) => Data (LigoApplication u)
deriving stock instance (Show (LigoTypeF u)) => Show (LigoApplication u)
deriving anyclass instance (NFData (LigoTypeF u)) => NFData (LigoApplication u)
deriving stock instance Eq (LigoApplication 'Concise)

-- | An applied argument.
data LigoApplicationArgument u = LigoApplicationArgument
  { laaArgumentType :: LigoTypeF u
    -- ^ A type of argument.
  , laaArgument :: LigoArgumentKind u
    -- ^ A way how argument is represented.
  } deriving stock (Generic)

deriving stock instance (Typeable u, Data (LigoTypeF u)) => Data (LigoApplicationArgument u)
deriving stock instance (Show (LigoTypeF u)) => Show (LigoApplicationArgument u)
deriving anyclass instance (NFData (LigoTypeF u)) => NFData (LigoApplicationArgument u)
deriving stock instance Eq (LigoApplicationArgument 'Concise)

-- | A kind of the applied argument.
data LigoArgumentKind u
  = LAKVar (LigoVariable u, FilePath)
    -- ^ A name of _bound_ variable with its file path.
  | LAKExpressionLocation Range
    -- ^ A full range of expression argument.
  deriving stock (Data, Show, Generic)
  deriving anyclass (NFData)

deriving stock instance Eq (LigoArgumentKind 'Concise)

-- | All the information provided for specific point of a Michelson program.
data LigoIndexedInfo u = LigoIndexedInfo
  { liiLocation    :: Maybe Range
    {- ^ Info about some expression (or sub-expression).

      For instance, if I have

      @
      let b = a + 1 in
      let c = 2 * b * b in
      @

      and this gets compiled to Michelson

      @
      DUPN x; PUSH 1; ADD;
      DUP; PUSH 2; MUL; MUL;
      @

      then @ADD@ and two @MUL@s will have metadata attached to them with0
      @location@ being set and pointing to @a + 1@, @2 * b@, and @2 * b * b@
      expressions respectively.

    -}

  , liiEnvironment :: Maybe (LigoStack u)
    {- ^ Info about LIGO stack at the current point.

      For each value in /Michelson stack/ it contains a corresponding LIGO
      variable (if such is associated). But this is limited to the part of stack
      related in the current stack frame.

      This metadata is usually set between LIGO statements.

    -}

  , liiSourceType :: Maybe (LigoTypeF u)
    {- ^ A type for the last computed value.

      Comes with location meta.

    -}

  , liiApplication :: Maybe (LigoApplication u)
    {- ^ This meta is tied to partial applications
      (i.e. the result type is arrow and an expression has @f a (b + 42)@ pattern)

    -}
  } deriving stock (Generic)

deriving stock instance (Typeable u, Data (LigoTypeF u)) => Data (LigoIndexedInfo u)
deriving stock instance (Show (LigoTypeF u)) => Show (LigoIndexedInfo u)
deriving anyclass instance (NFData (LigoTypeF u)) => NFData (LigoIndexedInfo u)
deriving stock instance Eq (LigoIndexedInfo 'Concise)

-- | A pattern for empty meta.
pattern LigoEmptyLocationInfo :: LigoIndexedInfo u
pattern LigoEmptyLocationInfo = LigoIndexedInfo Nothing Nothing Nothing Nothing

-- | A pattern for location meta.
pattern LigoMereLocInfo :: Range -> LigoTypeF u -> LigoIndexedInfo u
pattern LigoMereLocInfo loc typ = LigoIndexedInfo
  { liiLocation = Just loc
  , liiEnvironment = Nothing
  , liiSourceType = Just typ
  , liiApplication = Nothing
  }

-- | A pattern for environment meta.
pattern LigoMereEnvInfo :: LigoStack u -> LigoIndexedInfo u
pattern LigoMereEnvInfo env = LigoIndexedInfo
  { liiLocation = Nothing
  , liiEnvironment = Just env
  , liiSourceType = Nothing
  , liiApplication = Nothing
  }

-- | The debug output produced by LIGO.
data LigoMapper u = LigoMapper
  { lmLocations :: [LigoIndexedInfo u]
    -- ^ Metas that source mapper produced.
  , lmTypes :: LigoTypesVec
    -- ^ A type environment of the contract.
  , lmMichelsonCode :: Micheline.Expression
    -- ^ A compiled LIGO contract.
  }

-- | A convenient wrapper for LIGO module names.
newtype ModuleName = ModuleName
  { enModule :: Text
    -- ^ A full module name with dots.
  } deriving stock (Show, Eq, Ord, Generic, Data)
    deriving anyclass (NFData)

-- | A wrapper for list of modules with entry points.
newtype ModuleNamesList = ModuleNamesList
  { unModuleNamesList :: [ModuleName]
    -- ^ A list of modules with entry points.
  }
  deriving newtype (Buildable)

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
    _ldiVariables     :: HM.HashMap Text LigoVariableDefinitionScope
  , -- | `"types"`
    _ldiTypes         :: HM.HashMap Text LigoTypeDefinitionScope
  , -- | `"modules"
    _ldiModules       :: HM.HashMap Text LigoModuleDefinitionScope
  , -- | `"module_aliases"
    _ldiModuleAliases :: HM.HashMap Text LigoModuleAliasDefinitionScope
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
  , -- | `"expression_environment"`
    _lsExpressionEnvironment :: [Text]
  , -- | `"type_environment"`
    _lsTypeEnvironment       :: [Text]
  , -- | `"module_environment"`
    _lsModuleEnvironment     :: [Text]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 2 LigoScope

-- | Definition declaration that goes from `"definitions"` constraint
data LigoVariableDefinitionScope = LigoVariableDefinitionScope
  { -- | `"name"`
    _lvdsName       :: Text
    -- | Binding location
    -- `"range"`
  , _lvdsRange      :: LigoRange
    -- | The type itself
    -- `"t"`
  , _lvdsT          :: Value
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

-- | Definition of a module from '_ldiModules'.
data LigoModuleDefinitionScope = LigoModuleDefinitionScope
  { -- | `"definition"`
    _lmdsDefinition :: LigoVariableDefinitionScope
  , -- | `"members"`
    _lmdsMembers    :: LigoDefinitionsInner
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 4 LigoModuleDefinitionScope

-- | Definition of a module from '_ldiModuleAliases'.
data LigoModuleAliasDefinitionScope = LigoModuleAliasDefinitionScope
  { -- | `"definition"`
    _lmadsDefinition :: LigoVariableDefinitionScope
  , -- | `"members"`
    _lmadsAlias      :: [Text]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 5 LigoModuleAliasDefinitionScope

data LigoTypeDefinitionScope = LigoTypeDefinitionScope
  { -- | `"name"`
    _ltdsName      :: Text
    -- | Binding location
    -- `"range"`
  , _ltdsRange     :: LigoRange
  , _ltdsContent   :: Value
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via LigoJSON 4 LigoTypeDefinitionScope

-- | Whole ligo type.
-- ```
-- { "t" : LigoTypeFull }
-- ```
data LigoTypeFull
  = LTFCore LigoTypeExpression
  | LTFResolved LigoTypeExpression
  deriving stock (Generic, Show)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

-- Sometimes LigoErrorContent is just a String
instance FromJSON LigoErrorContent where
  parseJSON (String t) = pure $ LigoErrorContent t Nothing
  parseJSON o = genericParseJSON defaultOptions{fieldLabelModifier = drop 5 . toSnakeCase} o

instance MessagePack LigoTypeTable where
  toObject _ = const ObjectNil
  fromObjectWith cfg = withMsgMap "LigoTypeTable" \o -> do
    _lttFields <- parseFields =<< o .: "fields"
    _lttLayout <- o .: "layout"
    pure LigoTypeTable{..}
    where
      parseFields :: (MonadValidate DecodeError m) => [Object] -> m (HashMap Text LigoTypeExpression)
      parseFields = pure . HM.fromList <=< mapM parseElem

      parseElem :: (MonadValidate DecodeError m) => Object -> m (Text, LigoTypeExpression)
      parseElem = withMsgArray "TypeTableField" \arr -> do
        case V.toList arr of
          [ctor, content] -> do
            (_ :: Object, ctorName, _ :: Object) <- fromObjectWith cfg ctor
            typeExpr <- fromObjectWith cfg content
            pure (ctorName, typeExpr)
          _ -> refute "Expected two element array"

-- [ "Virtual", ...]
instance FromJSON LigoRange where
  parseJSON = genericParseJSON defaultOptions
    { sumEncoding = TwoElemArray
    -- "LRVirtual" -> "Virtual"
    , constructorTagModifier = drop 2
    }

instance MessagePack LigoLayout where
  toObject _ = const ObjectNil
  fromObjectWith cfg = withMsgArray "LigoLayout" \arr -> do
    case V.toList arr of
      [ObjectStr ctor, val]
        | ctor == "Inner" -> LLInner <$> fromObjectWith cfg val
        | ctor == "Field" ->
            case val of
              ObjectMap (M.fromList . toList -> obj) -> do
                (_ :: Object, name, _ :: Object) <- obj .: "name"
                pure $ LLField name
              _ -> refute $ decodeError [int||Expected object in constructor "Field"|]
        | otherwise -> refute $ decodeError [int||Unexpected constructor #{ctor}|]
      _ -> refute "Expected a two element array"

instance MessagePack (LigoVariable u) where
  toObject _ = const ObjectNil
  fromObjectWith cfg = fmap LigoVariable . fromObjectWith cfg

instance Buildable LigoTypeRef where
  build (LigoTypeRef i) =  [int||type##{i}|]

-- We're writing this instance because sometimes
-- it's hard to construct @LigoType@ by hand (e.g. in tests).
-- So, we'll treat that types are equal if only their prettified representations
-- are equal.
instance Eq LigoType where
  lhs == rhs = pretty @_ @Text (buildType Caml lhs) == pretty @_ @Text (buildType Caml rhs)

instance (SingI u) => Buildable (LigoExposedStackEntry u) where
  build (LigoExposedStackEntry decl ty fileName) =
    let
      declB = maybe (build unknownVariable) build decl
      fileNameB = maybe "#generated" build fileName
    in [int||elem #{declB} of #{buildLigoTypeF @u ty} in #{fileNameB}|]

instance MessagePack LigoTypeRef where
  toObject _ = const ObjectNil
  fromObjectWith cfg val = fromObjectWith cfg val >>= \(TextualNumber n) -> pure $ LigoTypeRef n

instance MessagePack (LigoExposedStackEntry 'Unique) where
  toObject _ = const ObjectNil
  fromObjectWith _ = withMsgMap "LIGO exposed stack entry" \o -> do
    leseType <- o .:! "source_type"
    leseDeclaration <- o .:! "name"
    leseFileName <- o .:! "file_name"
    return LigoExposedStackEntry{..}

instance (SingI u) => Buildable (LigoStackEntry u) where
  build = \case
    LigoStackEntry ese   -> build ese
    LigoHiddenStackEntry -> "<hidden elem>"

instance MessagePack (LigoStackEntry 'Unique) where
  toObject _ = const ObjectNil
  fromObjectWith cfg v = case v of
    ObjectNil     -> pure LigoHiddenStackEntry
    ObjectMap o
      | V.null o  -> pure LigoHiddenStackEntry
      | otherwise -> LigoStackEntry <$> fromObjectWith cfg v
    _             -> refute "Unexpected value"

instance Default (LigoIndexedInfo u) where
  def = LigoEmptyLocationInfo

instance (SingI u) => Buildable (LigoIndexedInfo u) where
  build = \case
    LigoEmptyLocationInfo -> "none"
    LigoIndexedInfo mLoc mEnv typ mApp -> unlinesF $ catMaybes
      [ mLoc <&> \loc ->  [int||location: #{loc}|]
      , mEnv <&> \env -> nameF "environment stack" $ blockListF env
      , case typ of { Just ty -> Just [int||source type: #{buildLigoTypeF @u ty}|] ; _ -> Nothing }
      , mApp <&> \app -> [int||Application: #{app}|]
      ]

instance (SingI u) => Buildable (LigoApplication u) where
  build LigoApplication{..} =
    [int||
    Applied function: #{laAppliedFunction}
    Arguments: #{blockListF laArguments}
    |]

instance (SingI u) => Buildable (LigoApplicationArgument u) where
  build LigoApplicationArgument{..} =
    [int||
    Argument type: #{buildLigoTypeF @u laaArgumentType}
    Argument: #{laaArgument}
    |]

instance (SingI u) => Buildable (LigoArgumentKind u) where
  build = \case
    LAKVar v -> [int||Variable: #{v}|]
    LAKExpressionLocation loc -> [int||Expression location: #{loc}|]

instance MessagePack (LigoIndexedInfo 'Unique) where
  toObject _ = const ObjectNil
  fromObjectWith _ = withMsgMap "location info" \o -> do
    liiLocation <- o .:! "location"
    liiEnvironment <- o .:! "environment"
    liiSourceType <-  o .:! "source_type"
    liiApplication <- o .:! "application"
    return LigoIndexedInfo{..}

instance MessagePack (LigoApplication 'Unique) where
  toObject _ = const ObjectNil
  fromObjectWith _ = withMsgMap "LigoApplication" \o -> do
    laAppliedFunction <- o .:! "applied_function"
    laArguments <- o .: "arguments"
    pure LigoApplication{..}

instance MessagePack (LigoApplicationArgument 'Unique) where
  toObject _ = const ObjectNil
  fromObjectWith _ = withMsgMap "LigoApplicationArgument" \o -> do
    laaArgumentType <- o .:! "argument_type"
    laaArgument <- o .: "argument"
    pure LigoApplicationArgument{..}

instance MessagePack (LigoArgumentKind 'Unique) where
  toObject _ = const ObjectNil
  fromObjectWith cfg = \case
    tuple@ObjectArray{} -> LAKVar <$> fromObjectWith cfg tuple
    loc -> LAKExpressionLocation <$> fromObjectWith cfg loc

instance AsEmpty (LigoIndexedInfo u) where
  _Empty = prism
    do \() -> LigoEmptyLocationInfo
    do \case LigoEmptyLocationInfo -> Right (); other -> Left other

instance {-# OVERLAPPING #-} (SingI u) => Buildable [LigoIndexedInfo u] where
  build = blockListF

instance MessagePack Micheline.Expression where
  toObject _ = const ObjectNil
  fromObjectWith cfg v = asumMsg
    [ ExpSeq () <$> fromObjectWith cfg v
    , ExpPrim () <$> fromObjectWith cfg v
    , ExpString () <$> withMsgMap "ExpressionString" (.: "string") v
    , ExpInt () . unStringEncode <$> withMsgMap "ExpressionInt" (.: "int") v
    , ExpBytes () . unHexJSONByteString <$> withMsgMap "ExpressionBytes" (.: "bytes") v
    ]

instance MessagePack (Exp x) => MessagePack (MichelinePrimAp x) where
  toObject _ = const ObjectNil
  fromObjectWith _ = withMsgMap "Prim" \v -> MichelinePrimAp
    <$> v .: "prim"
    <*> v .:? "args"   .!= []
    <*> v .:? "annots" .!= []

instance MessagePack MichelinePrimitive where
  toObject _ = const ObjectNil
  fromObjectWith _ = withMsgText "MichelinePrimitive" \t ->
    either (refute . decodeError . toString) pure $ readEither ("Prim_" <> t)

instance MessagePack Annotation where
  toObject _ = const ObjectNil
  fromObjectWith _ = withMsgText "Annotation" (wrapMonadFail "Cannot parse annotation" . annotFromText)

instance MessagePack TezosBigNum where
  toObject _ = const ObjectNil
  fromObjectWith _ = fmap StringEncode . withMsgText "TezosBigNum" \txt ->
    maybe (refute "Failed to parse string") pure $ readMaybe (toString txt)

instance MessagePack HexJSONByteString where
  toObject _ = const ObjectNil
  fromObjectWith _ =
    withMsgText "Hex-encoded bytestring" \t ->
      case decodeHex t of
        Nothing -> refute "Invalid hex encoding"
        Just res -> pure (HexJSONByteString res)

instance MessagePack (LigoMapper 'Unique) where
  toObject _ = const ObjectNil
  fromObjectWith _ = withMsgMap "ligo output" \o -> do
    mich <- o .: "michelson"
    lmMichelsonCode <- mich .: "expression"
    lmTypes <- o .: "types"
    lmLocations <- mich .: "locations"
    return LigoMapper{..}

instance MessagePack LigoTypesVec where
  toObject _ = const ObjectNil
  fromObjectWith cfg = fmap LigoTypesVec . fromObjectWith cfg

instance MessagePack LigoTypeExpression where
  toObject _ = const ObjectNil
  fromObjectWith _ = withMsgMap "LigoTypeExpression" \o -> do
    _lteTypeContent <- o .: "type_content"
    _lteOrigVar <- o .:? "orig_var"
    pure LigoTypeExpression{..}

instance MessagePack LigoTypeContent where
  toObject _ = const ObjectNil
  fromObjectWith cfg = withMsgVariant "LigoTypeContent" \(name, arg) -> asumMsg
    [ LTCVariable    <$> (guardMsg (name == "T_variable"   ) >> fromObjectWith cfg arg)
    , LTCSum         <$> (guardMsg (name == "T_sum"        ) >> fromObjectWith cfg arg)
    , LTCRecord      <$> (guardMsg (name == "T_record"     ) >> fromObjectWith cfg arg)
    , LTCArrow       <$> (guardMsg (name == "T_arrow"      ) >> fromObjectWith cfg arg)
    , LTCSingleton   <$> (guardMsg (name == "T_singleton"  ) >> fromObjectWith cfg arg)
    , LTCConstant    <$> (guardMsg (name == "T_constant"   ) >> fromObjectWith cfg arg)
    ]

instance MessagePack LigoTypeArrow where
  toObject _ = const ObjectNil
  fromObjectWith _ = withMsgMap "LigoTypeArrow" \o -> do
    _ltaType1 <- o .: "type1"
    _ltaType2 <- o .: "type2"
    pure LigoTypeArrow{..}

instance MessagePack LigoTypeLiteralValue where
  toObject _ = const ObjectNil
  fromObjectWith cfg = withMsgVariant "LigoTypeLiteralValue" \(name, arg) -> asumMsg
    [ LTLVUnit                        <$   guardMsg (name == "Literal_unit"        )
    , LTLVInt       . unTextualNumber <$> (guardMsg (name == "Literal_int"         ) >> fromObjectWith cfg arg)
    , LTLVNat       . unTextualNumber <$> (guardMsg (name == "Literal_nat"         ) >> fromObjectWith cfg arg)
    , LTLVTimestamp . unTextualNumber <$> (guardMsg (name == "Literal_timestamp"   ) >> fromObjectWith cfg arg)
    , LTLVMutez     . unTextualNumber <$> (guardMsg (name == "Literal_mutez"       ) >> fromObjectWith cfg arg)
    , LTLVString                      <$> (guardMsg (name == "Literal_string"      ) >> fromObjectWith cfg arg)
    , LTLVBytes                       <$> (guardMsg (name == "Literal_bytes"       ) >> fromObjectWith cfg arg)
    , LTLVAddress                     <$> (guardMsg (name == "Literal_address"     ) >> fromObjectWith cfg arg)
    , LTLVSignature                   <$> (guardMsg (name == "Literal_signature"   ) >> fromObjectWith cfg arg)
    , LTLVKey                         <$> (guardMsg (name == "Literal_key"         ) >> fromObjectWith cfg arg)
    , LTLVKeyHash                     <$> (guardMsg (name == "Literal_key_hash"    ) >> fromObjectWith cfg arg)
    , LTLVChainId                     <$> (guardMsg (name == "Literal_chain_id"    ) >> fromObjectWith cfg arg)
    , LTLVOperation                   <$> (guardMsg (name == "Literal_operation"   ) >> fromObjectWith cfg arg)
    , LTLVBls_381G1                   <$> (guardMsg (name == "Literal_bls12_381_g1") >> fromObjectWith cfg arg)
    , LTLVBls_381G2                   <$> (guardMsg (name == "Literal_bls12_381_g2") >> fromObjectWith cfg arg)
    , LTLVBls_381Fr                   <$> (guardMsg (name == "Literal_bls12_381_fr") >> fromObjectWith cfg arg)
    ]

instance MessagePack LigoString where
  toObject _ = const ObjectNil
  fromObjectWith cfg = withMsgVariant "LigoString" \(name, arg) -> asumMsg
    [ LSStandard <$> (guardMsg (name == "Standard") >> fromObjectWith cfg arg)
    , LSVerbatim <$> (guardMsg (name == "Verbatim") >> fromObjectWith cfg arg)
    ]

instance MessagePack LigoTypeConstant where
  toObject _ = const ObjectNil
  fromObjectWith _ = withMsgMap "LigoTypeConstant" \o -> do
    _ltcParameters <- o .: "parameters"
    _ltcInjection <- o .: "injection"
    pure LigoTypeConstant{..}

instance Buildable ModuleName where
  build ModuleName{..}
    | not (T.null enModule) = build enModule
    | otherwise = [int||Current module|]

instance IsString ModuleName where
  fromString = ModuleName . toText

instance MessagePack LigoFileRange where
  fromObjectWith _ = withMsgMap "LigoFileRange" \o -> do
    _lfrStart <- o .: "start"
    _lfrStop <- o .: "stop"
    pure LigoFileRange{..}

instance MessagePack LigoRangeInner where
  fromObjectWith _ = withMsgMap "LigoRangeInner" \o -> do
    _lriByte <- o .: "byte"
    _lriPointNum <- o .: "point_num"
    _lriPointBol <- o .: "point_bol"
    pure LigoRangeInner{..}

instance MessagePack LigoByte where
  fromObjectWith _ = withMsgMap "LigoByte" \o -> do
    _lbPosFname <- o .: "pos_fname"
    _lbPosLnum <- o .: "pos_lnum"
    _lbPosBol <- o .: "pos_bol"
    _lbPosCnum <- o .: "pos_cnum"
    pure LigoByte{..}

----------------------------------------------------------------------------
-- Pretty
----------------------------------------------------------------------------

instance Pretty LigoError where
  pp (LigoError status stage (LigoErrorContent msg at)) = mconcat
    [ pp status <+> "in" <+> text (Debug.show stage)
    , case at of
        Nothing -> ":\n"
        Just at' -> " at" <+> pp (fromLigoRangeOrDef at') <.> ":\n"
    , pp msg
    ]

-- TODO: replace @Pretty@ instance with @Buildable@
instance Buildable LigoError where
  build = Debug.show . pp

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Ensures that the JSON value is a list with only one element.
-- Fails otherwise.
guardOneElemList :: Text -> Value -> Parser ()
guardOneElemList expected = withArray (toString expected) \arr -> do
  case V.length arr of
    1 -> do
      String ctor <- pure $ V.unsafeIndex arr 0
      guard (ctor == expected)
    len -> fail $ "Expected array of size 1, got " <> show len

-- | Converts ligo ranges to our internal ones.
-- Note: ligo team allows for start file of a range be different from end file.
-- Either if this intentional or not we throw an error if they are so.
-- >>> :{
-- mbFromLigoRange
--   (LigoRange
--     (LigoRangeInner (LigoByte "test.jsligo" 2 undefined undefined) 3 6)
--     (LigoRangeInner (LigoByte "test.jsligo" 5 undefined undefined) 11 12)
--   )
-- :}
-- test.jsligo:2:4-5:2
mbFromLigoRange :: LigoRange -> Maybe Range
mbFromLigoRange (LRVirtual _) = Nothing
mbFromLigoRange
  (LRFile
    (LigoFileRange
      (LigoRangeInner LigoByte{_lbPosLnum = startLn, _lbPosFname = startFilePath} startNum startBol)
      (LigoRangeInner LigoByte{_lbPosLnum = endLn  , _lbPosFname = endFilePath  } endNum   endBol)
    )
  )
  | startFilePath /= endFilePath = error "start file of a range does not equal to its end file"
  | otherwise = Just Range
      { _rStart = LigoPosition startLn (startNum - startBol + 1)
      , _rFinish = LigoPosition endLn (endNum - endBol + 1)
      , _rFile = startFilePath
      }

-- | Converts @LigoRange@ to @Range@.
-- Returns a dummy @point 0 0@ if the passed @LigoRange@ is virtual.
fromLigoRangeOrDef :: LigoRange -> Range
fromLigoRangeOrDef = fromMaybe (point 0 0) . mbFromLigoRange

-- | A convenient type for @fromLigoType@ function.
-- Represents an origin of the textual representation.
data NameKind
  = NameType
    -- ^ A type name.
  | NameField FieldKind
    -- ^ A constructor/field name.
  | NameModule
    -- TODO: unused.
    -- ^ A module name.

-- | An inner field of @NameField@.
data FieldKind
  = FieldSum
    -- ^ A constructor name.
  | FieldProduct
    -- ^ A record field name.

-- | Reconstruct LIGO tree out of @LigoTypeFull@.
fromLigoTypeFull :: LigoTypeFull -> LIGO Info
fromLigoTypeFull = \case
  LTFCore lte     -> fromLigoTypeExpression lte
  LTFResolved lte -> fromLigoTypeExpression lte

-- | Reconstruct LIGO tree out of @LigoTypeExpression@.
fromLigoTypeExpression :: LigoTypeExpression -> LIGO Info
fromLigoTypeExpression
  LigoTypeExpression {..} =
    fromLigoType defaultState _lteTypeContent

-- | Reconstruct LIGO tree out of @LigoTypeContent@.
fromLigoType
  :: Info
  -> LigoTypeContent
  -> LIGO Info
fromLigoType st = \case
  LTCConstant LigoTypeConstant {..} ->
    -- See: https://gitlab.com/ligolang/ligo/-/issues/1478
    fromLigoConstant (_ltcInjection & over _head toLower) _ltcParameters

  LTCVariable variable -> fromLigoVariable variable

  LTCRecord record ->
    case tryConvertToTuple record of
      Just tupleTypes ->
        make' (st, TProduct (fromLigoTypeExpression <$> tupleTypes))
      Nothing ->
        let record' = fromLigoTable FieldProduct record in
        let ligoLayout = _lttLayout record in
        make' (st, TRecord (ligoLayoutToLayout ligoLayout) record')

  LTCSum sum ->
    case fromLigoTable FieldSum sum of
      [] -> error "malformed sum type, please report this as a bug"
      v : vs ->
        let ligoLayout = _lttLayout sum in
        make' (st, TSum (ligoLayoutToLayout ligoLayout) (v :| vs))

  LTCSingleton literalValue -> fromLigoTypeLiteralValue literalValue

  LTCArrow LigoTypeArrow {..} ->
    make' (st, TArrow (fromLigoTypeExpression _ltaType1) (fromLigoTypeExpression _ltaType2))
  where
    tryConvertToTuple :: LigoTypeTable -> Maybe [LigoTypeExpression]
    tryConvertToTuple LigoTypeTable{..} =
      forM [0..HM.size _lttFields - 1] \i ->
        _lttFields HM.!? show i

    ligoLayoutToLayout :: LigoLayout -> Layout
    ligoLayoutToLayout layoutTree
      | depth layoutTree > 1 || (length ctorOrder <= 2 && isSorted ctorOrder) = Tree
      | otherwise = Comb
      where
        depth :: LigoLayout -> Int
        depth = \case
          LLField{} -> 0
          LLInner children -> 1 + maximum (0 :| map depth children)

        isSorted = \case
          [] -> True
          [_] -> True
          x : xs@(y : _) -> x <= y && isSorted xs

        ctorOrder = getOrderFromLayoutTree layoutTree

    getOrderFromLayoutTree :: LigoLayout -> [Text]
    getOrderFromLayoutTree = \case
      LLField ctor -> [ctor]
      LLInner children -> foldMap getOrderFromLayoutTree children

    fromLigoTypeLiteralValue :: LigoTypeLiteralValue -> LIGO Info
    fromLigoTypeLiteralValue = \case
      LTLVUnit -> make' (st, AST.Name "()")
      LTLVInt n -> make' (st, CInt $ show n)
      LTLVNat n -> make' (st, CNat $ show n)
      LTLVTimestamp n -> make' (st, CInt $ show n)
      LTLVMutez n -> make' (st, CTez $ show n)
      LTLVString str -> fromLigoString str
      LTLVBytes str -> make' (st, CBytes str)
      LTLVAddress str -> make' (st, CString str)
      LTLVSignature str -> make' (st, CString str)
      LTLVKey str -> make' (st, CString str)
      LTLVKeyHash str -> make' (st, CString str)
      LTLVChainId str -> make' (st, CString str)
      LTLVOperation bts -> make' (st, CBytes bts)
      LTLVBls_381G1 bts -> make' (st, CBytes bts)
      LTLVBls_381G2 bts -> make' (st, CBytes bts)
      LTLVBls_381Fr bts -> make' (st, CBytes bts)

    fromLigoString :: LigoString -> LIGO Info
    fromLigoString = \case
      LSStandard str -> make' (st, CString str)
      LSVerbatim str -> make' (st, Verbatim str)

    fromLigoVariable :: Text -> LIGO Info
    fromLigoVariable = fromLigoPrimitive NameType

    fromLigoPrimitive :: NameKind -> Text -> LIGO Info
    fromLigoPrimitive = \case
      NameType -> make' . (st,) . TypeName
      NameField FieldSum -> make' . (st,) . Ctor
      NameField FieldProduct -> make' . (st,) . FieldName
      NameModule -> make' . (st,) . AST.ModuleName

    fromLigoConstant name [] = fromLigoPrimitive NameType name
    fromLigoConstant name params =
      let n = fromLigoPrimitive NameType name in
      let p = fromLigoTypeExpression <$> params in
      make' (st, TApply n p)

    fromLigoTable fieldKind x =
      map (uncurry (fromLigoTableField fieldKind)) ctorsAndFields
      where
        ctorsAndFields :: [(Text, LigoTypeExpression)]
        ctorsAndFields = _lttLayout x
          & getOrderFromLayoutTree
          <&> do \ctor -> (ctor,) <$> HM.lookup ctor (_lttFields x)
          & catMaybes

    fromLigoTableField
      :: FieldKind
      -> Text
      -> LigoTypeExpression
      -> LIGO Info
    fromLigoTableField fieldKind name typeExpr =
      let n = fromLigoPrimitive (NameField fieldKind) name in
      -- FIXME: Type annotation is optional.
      let type' = Just $ fromLigoTypeExpression typeExpr in
      case fieldKind of
        FieldSum     -> make' (st, Variant n $ maybeToList type')
        FieldProduct -> make' (st, TField  n type')

-- | Converts @LigoDefinitions@ to our internal representation of @Scope@s.
fromLigoDefinitions :: LigoDefinitions -> [Scope]
fromLigoDefinitions LigoDefinitions{..} = mapMaybe fromLigoScope _ldScopes
  where
    fromLigoScope :: LigoScope -> Maybe Scope
    fromLigoScope LigoScope{..} = do
      sRange <- mbFromLigoRange _lsRange
      let sVariables = _lsExpressionEnvironment
      pure Scope{..}

-- | A default @Info@.
defaultState :: Info
defaultState = point 1 1

-- | Variant of `make` that constructs a tree out of annotation and node
-- that recovers range from previous subnodes by merging them, this helps to
-- reconstruct `["Virtual", "generated"]` types out of their subnodes which
-- by some onorthodox opportunity may have proper ranges.
make'
  :: forall fs f .
     ( Element f fs
     , Foldable f
     , Apply Functor fs
     ) => (Info, f (Tree fs Info))
       -> Tree fs Info
make' (i, f)
  | null ges = i :< inject f
  | otherwise = i' :< inject f
  where
    ges = List.filter (not . (`leq` i)) (extract <$> Data.Foldable.toList f)
    r = List.minimum ges `merged` List.maximum ges
    i' = r

-- | Since we have @Maybe LigoVariable@ in @LigoExposedStackEntry@
-- we need to have some default variable for unknown variables.
unknownVariable :: Text
unknownVariable = "?"

-- | This constant is used in cases when we can't find a name for a stack frame.
-- E.g. we're going into lambda function or in cycle.
--
-- We're using @<internal>@ name because these stack frames are related
-- to some internal @Michelson@ lambdas that @ligo@ can produce.
internalStackFrameName :: Text
internalStackFrameName = "<internal>"

-- | Make a concise @LigoVariable@ from the unique one.
stripSuffixHashVariable :: LigoVariable 'Unique -> LigoVariable 'Concise
stripSuffixHashVariable var = LigoVariable $ pretty var

-- | Creates @LigoTypeExpression@ from its type content.
mkTypeExpression :: LigoTypeContent -> LigoTypeExpression
mkTypeExpression content = LigoTypeExpression
  { _lteTypeContent = content
  , _lteOrigVar = Nothing
  }

-- | Creates a constant @LigoTypeExpression@ from its injection
-- and type parameters.
mkConstantType :: Text -> [LigoTypeExpression] -> LigoTypeExpression
mkConstantType typeName parameters = mkTypeExpression $ LTCConstant $
  LigoTypeConstant
    { _ltcParameters = parameters
    , _ltcInjection = typeName
    }

-- | Creates an arrow @LigoTypeExpression@ from its domain and codomain.
mkArrowType :: LigoTypeExpression -> LigoTypeExpression -> LigoTypeExpression
mkArrowType domain codomain = mkTypeExpression $ LTCArrow $
  LigoTypeArrow
    { _ltaType2 = codomain
    , _ltaType1 = domain
    }

-- | An infix version of @mkArrowType@.
(~>) :: LigoTypeExpression -> LigoTypeExpression -> LigoTypeExpression
(~>) = mkArrowType

infixr 2 ~>

-- | Creates a @LigoTypeTable@ from its layout and associative list
-- of constructor/field names to their types.
mkTypeTable :: LigoLayout -> [(Text, LigoTypeExpression)] -> LigoTypeTable
mkTypeTable layout keyValues = LigoTypeTable (HM.fromList keyValues) layout

-- | Creates a record @LigoTypeExpression@ from its layout and associative list
-- of field names to their types.
mkRecordType :: LigoLayout -> [(Text, LigoTypeExpression)] -> LigoTypeExpression
mkRecordType layout keyValues = mkTypeTable layout keyValues
  & LTCRecord
  & mkTypeExpression

-- | Creates a sum @LigoTypeExpression@ from its layout and associative list
-- of constructor names to their types.
mkSumType :: LigoLayout -> [(Text, LigoTypeExpression)] -> LigoTypeExpression
mkSumType layout keyValues = mkTypeTable layout keyValues
  & LTCSum
  & mkTypeExpression

-- | Creates a simple constant @LigoTypeExpression@
-- without any type parameters.
mkSimpleConstantType :: Text -> LigoTypeExpression
mkSimpleConstantType typ = mkConstantType typ []

-- | Creates a pair.
mkPairType :: LigoTypeExpression -> LigoTypeExpression -> LigoTypeExpression
mkPairType fstElem sndElem = mkRecordType pairLayout
  [ ("0", fstElem)
  , ("1", sndElem)
  ]
  where
    pairLayout = LLInner
      [ LLField "0"
      , LLField "1"
      ]

-- | Prettify 'LigoType' in provided dialect.
buildType :: Lang -> LigoType -> Doc
buildType lang LigoType{..} = maybe "" (buildTypeExpr lang) unLigoType

-- | Prettify 'LigoTypeExpression' in provided dialect.
buildTypeExpr :: Lang -> LigoTypeExpression -> Doc
buildTypeExpr lang typExpr =
  let
    tree = fromLigoTypeFull $ LTFResolved typExpr
    ppr = fromMaybe "" $ asum
      [ lppDialect lang <$> layer @AST.Type tree
      , lppDialect lang <$> layer @AST.TypeName tree
      ]
  in show ppr

-- | If we have malformed LIGO contract then we'll see
-- in error @ligo@ binary output a red-colored text
-- which represents a place where error occurred.
replaceANSI :: Text -> Text
replaceANSI =
    T.replace [int||#ansi{[Reset]}|] "<--"
  . T.replace
      [int||#ansi{[SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Red]}|] "-->"

-- | We can't define type class instances for type families.
-- This function is a workaround for @Buildable (LigoTypeF u)@.
buildLigoTypeF :: forall u. (SingI u) => LigoTypeF u -> Doc
buildLigoTypeF typ = case sing @u of
  SUnique -> build typ
  SConcise -> buildType Caml typ

-- | Creates a @ModuleName@.
mkModuleName :: Text -> ModuleName
mkModuleName txt =
  let enModule = T.dropEnd 1 $ T.dropWhileEnd (/= '.') txt in
  ModuleName{..}

makeLensesWith postfixLFields ''LigoExposedStackEntry
makeLensesWith postfixLFields ''LigoIndexedInfo
makeLensesWith postfixLFields ''LigoApplication
makeLensesWith postfixLFields ''LigoApplicationArgument
makePrisms ''LigoStackEntry
makePrisms ''LigoArgumentKind

-- | Make a concise @LigoStackEntry@ from the unique one.
makeConciseLigoStackEntry :: LigoTypesVec -> LigoStackEntry 'Unique -> LigoStackEntry 'Concise
makeConciseLigoStackEntry vec = \case
  LigoStackEntry lese@LigoExposedStackEntry{..} -> LigoStackEntry lese
    { leseDeclaration = stripSuffixHashVariable <$> leseDeclaration
    , leseType = vec `readLigoType` leseType
    }
  LigoHiddenStackEntry -> LigoHiddenStackEntry

-- | Make a concise @LigoApplication@ from the unique one.
makeConciseLigoApplication :: LigoTypesVec -> LigoApplication 'Unique -> LigoApplication 'Concise
makeConciseLigoApplication vec LigoApplication{..} = LigoApplication
  { laAppliedFunction = stripSuffixHashVariable <$> laAppliedFunction
  , laArguments = makeConciseAppliedArgument <$> laArguments
  }
  where
    makeConciseAppliedArgument :: LigoApplicationArgument 'Unique -> LigoApplicationArgument 'Concise
    makeConciseAppliedArgument LigoApplicationArgument{..} =
      let
        conciseArgument =
          case laaArgument of
            LAKVar (v, filepath) -> LAKVar (stripSuffixHashVariable v, filepath)
            LAKExpressionLocation loc -> LAKExpressionLocation loc
      in
        LigoApplicationArgument
          { laaArgumentType = readLigoType vec laaArgumentType
          , laaArgument = conciseArgument
          }

-- | Make a concise @LigoIndexedInfo@ from the unique one.
makeConciseLigoIndexedInfo :: LigoTypesVec -> LigoIndexedInfo 'Unique -> LigoIndexedInfo 'Concise
makeConciseLigoIndexedInfo vec indexedInfo =
  indexedInfo
    { liiEnvironment = liiEnvironment indexedInfo & _Just . each %~ makeConciseLigoStackEntry vec
    , liiSourceType = readLigoType vec <$> liiSourceType indexedInfo
    , liiApplication = makeConciseLigoApplication vec <$> liiApplication indexedInfo
    }

-- | Parses modules with entry points from LIGO compiler's
-- textual output.
parseModuleNamesList :: Text -> Maybe ModuleNamesList
parseModuleNamesList (lines -> parts) = do
  moduleNames <- safeTail >=> safeInit $ parts
  pure $ ModuleNamesList (nub $ mkModuleName <$> moduleNames)
  where
    safeTail :: [a] -> Maybe [a]
    safeTail = fmap tail . nonEmpty

    safeInit :: [a] -> Maybe [a]
    safeInit = fmap init . nonEmpty

-- | Resolves a @LigoTypeRef@ into @LigoType@ using @LigoTypesVec@.
readLigoType :: LigoTypesVec -> Maybe LigoTypeRef -> LigoType
readLigoType (LigoTypesVec v) = \case
  Nothing -> LigoType Nothing
  Just (LigoTypeRef n) -> LigoType $ v V.!? n
