{-# LANGUAGE DeriveDataTypeable, StandaloneKindSignatures, UndecidableInstances #-}

-- | Types coming from @ligo@ executable.
module Language.LIGO.Debugger.CLI.Types
  ( module Language.LIGO.Debugger.CLI.Types
  ) where

import Prelude hiding (Element, Product (..), sum)

import Control.Lens (AsEmpty (..), Each (each), _head, makePrisms, prism)
import Control.Lens.Prism (_Just)
import Data.Aeson
  (FromJSON (..), Options (constructorTagModifier, fieldLabelModifier, sumEncoding),
  SumEncoding (TwoElemArray), Value (..), defaultOptions, genericParseJSON, withArray, withObject,
  (.:!), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap (toAscList)
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.Types qualified as Aeson
import Data.Char (toLower)
import Data.Data (Data)
import Data.Default (Default (..))
import Data.Foldable qualified
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.List.NonEmpty (singleton)
import Data.Singletons.TH (SingI (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug qualified
import Fmt.Buildable (Buildable, blockListF, build, nameF, pretty, unlinesF)
import Fmt.Utils (Doc)
import System.Console.ANSI
  (Color (Red), ColorIntensity (Dull), ConsoleIntensity (BoldIntensity), ConsoleLayer (Foreground),
  SGR (Reset, SetColor, SetConsoleIntensity))
import Text.Interpolation.Nyan (int)
import Util

import Morley.Micheline.Expression qualified as Micheline
import Morley.Util.Lens

import Duplo
  (Apply, Cofree ((:<)), Comonad (extract), Element, Lattice (leq), Tree, inject, layer, text)

import Language.LIGO.AST.Pretty hiding (Doc)
import Language.LIGO.AST.Skeleton hiding (Name)
import Language.LIGO.AST.Skeleton qualified as AST
import Language.LIGO.Debugger.CLI.Helpers
import Language.LIGO.Diagnostic
import Language.LIGO.Parser
import Language.LIGO.Product
import Language.LIGO.Range

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- | Describes a variable.
newtype LigoVariable (u :: NameType) = LigoVariable
  { lvName :: Name u
  } deriving stock (Show, Generic, Data)
    deriving newtype (Buildable)
    deriving anyclass (NFData)

deriving stock instance Eq (LigoVariable 'Concise)

-- | Reference to type description in the types map.
newtype LigoTypeRef = LigoTypeRef { unLigoTypeRef :: Int }
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
  = LTCVariable LigoTypeVariable
  | LTCSum LigoTypeSum
    -- | @T_record@
  | LTCRecord LigoTypeRecord
    -- | @T_arrow@
  | LTCArrow LigoTypeArrow
    -- | @T_singleton@
  | LTCSingleton LigoTypeLiteralValue
  | LTCAbstraction LigoTypeForAll
  | LTCForAll LigoTypeForAll
    -- 4th stage specific
  | LTCApp LigoTypeApp
  | LTCModuleAccessor LigoTypeModuleAccessor
    -- | 5th stage specific
    -- @T_constant@
  | LTCConstant LigoTypeConstant
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

data LigoTypeLiteralValue
  = LTLVUnit
  | LTLVInt Int
  | LTLVNat Int
  | LTLVTimestamp Int
  | LTLVMutez Int
  | LTLVString LigoString
  | LTLVBytes Text
  | LTLVAddress Text
  | LTLVSignature Text
  | LTLVKey Text
  | LTLVKeyHash Text
  | LTLVChainId Text
  | LTLVOperation Text
  | LTLVBls_381G1 Text
  | LTLVBls_381G2 Text
  | LTLVBls_381Fr Text
  | LTLVChest Text
  | LTLVChestKey Text
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

data LigoString
  = LSStandard Text
  | LSVerbatim Text
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

data LigoTypeApp = LigoTypeApp
  { _ltaTypeOperator :: LigoTypeVariable
  , _ltaArguments    :: [LigoTypeExpression]
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON) via LigoJSON 3 LigoTypeApp

data LigoTypeModuleAccessor = LigoTypeModuleAccessor
  { _ltmaModulePath :: [LigoModuleVariable]
  , _ltmaElement    :: LigoTypeVariable
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON) via LigoJSON 4 LigoTypeModuleAccessor

type LigoTypeSum = LigoTypeTable
type LigoTypeRecord = LigoTypeTable

data LigoTypeTable = LigoTypeTable
  { _lttFields  :: HM.HashMap Text LigoTypeExpression
  , _lttLayout  :: LigoLayout
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

data LigoLayout
  = LLInner [LigoLayout]
  | LLField Text
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)

data LigoTypeConstant = LigoTypeConstant
  { _ltcParameters :: [LigoTypeExpression]
  , _ltcLanguage   :: Text
  , _ltcInjection  :: NonEmpty Text
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON) via LigoJSON 3 LigoTypeConstant

data LigoTypeArrow = LigoTypeArrow
  -- "type2" -> "type1"
  { _ltaType2 :: LigoTypeExpression
  , _ltaType1 :: LigoTypeExpression
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON) via LigoJSON 3 LigoTypeArrow

data LigoVar = LigoVar
  { _lvName      :: Text
  , _lvCounter   :: Int
  , _lvGenerated :: Bool
  , _lvLocation  :: LigoRange
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON) via LigoJSON 2 LigoVar

type LigoTypeVariable = LigoVar
type LigoModuleVariable = LigoVar

data LigoTypeForAll = LigoTypeForAll
  { _ltfaTyBinder :: LigoTypeVariable
  , _ltfaType_    :: LigoTypeExpression
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON) via LigoJSON 4 LigoTypeForAll

-- | Location of definition.
-- ```
-- { "location": LigoRange }
-- ```
data LigoRange
  = LRVirtual Text
  | LRFile LigoFileRange
  deriving stock (Eq, Generic, Show, Data)
  deriving anyclass (NFData, Hashable)

data LigoFileRange = LigoFileRange
  { _lfrStart :: LigoRangeInner
  , _lfrStop  :: LigoRangeInner
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
  , _lriPointNum :: Int
  , _lriPointBol :: Int
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
  , _lbPosLnum  :: Int
  , _lbPosBol   :: Int
  , _lbPosCnum  :: Int
  }
  deriving stock (Eq, Generic, Show, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON) via LigoJSON 2 LigoByte

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
  , _lteOrigVar     :: Maybe LigoTypeVariable
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON) via LigoJSON 3 LigoTypeExpression

newtype LigoTypesVec = LigoTypesVec { unLigoTypesVec :: Vector LigoTypeExpression }
  deriving stock (Data, Generic)
  deriving newtype (Show, NFData, FromJSON)

newtype LigoType = LigoType { unLigoType :: Maybe LigoTypeExpression }
  deriving stock (Data)
  deriving newtype (Show, Generic, NFData, Hashable)

pattern LigoTypeResolved :: LigoTypeExpression -> LigoType
pattern LigoTypeResolved typ = LigoType (Just typ)

pattern LigoTypeUnresolved :: LigoType
pattern LigoTypeUnresolved = LigoType Nothing

{-# COMPLETE LigoTypeResolved, LigoTypeUnresolved #-}

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
  , leseType        :: LigoTypeF u
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

pattern LigoStackEntryNoVar :: LigoTypeF u -> LigoStackEntry u
pattern LigoStackEntryNoVar ty = LigoStackEntry LigoExposedStackEntry
  { leseDeclaration = Nothing
  , leseType = ty
  }

pattern LigoStackEntryVar :: Text -> LigoType -> LigoStackEntry 'Concise
pattern LigoStackEntryVar name ty = LigoStackEntry LigoExposedStackEntry
  { leseDeclaration = Just LigoVariable{ lvName = Name name }
  , leseType = ty
  }

-- | Variables associated with each element on stack.
--
-- Entries are listed in top-to-bottom order.
type LigoStack u = [LigoStackEntry u]

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
  } deriving stock (Generic)

deriving stock instance (Typeable u, Data (LigoTypeF u)) => Data (LigoIndexedInfo u)
deriving stock instance (Show (LigoTypeF u)) => Show (LigoIndexedInfo u)
deriving anyclass instance (NFData (LigoTypeF u)) => NFData (LigoIndexedInfo u)
deriving stock instance Eq (LigoIndexedInfo 'Concise)

pattern LigoEmptyLocationInfo :: LigoIndexedInfo u
pattern LigoEmptyLocationInfo = LigoIndexedInfo Nothing Nothing Nothing

pattern LigoMereLocInfo :: Range -> LigoTypeF u -> LigoIndexedInfo u
pattern LigoMereLocInfo loc typ = LigoIndexedInfo
  { liiLocation = Just loc
  , liiEnvironment = Nothing
  , liiSourceType = Just typ
  }

pattern LigoMereEnvInfo :: LigoStack u -> LigoIndexedInfo u
pattern LigoMereEnvInfo env = LigoIndexedInfo
  { liiLocation = Nothing
  , liiEnvironment = Just env
  , liiSourceType = Nothing
  }

-- | The debug output produced by LIGO.
data LigoMapper u = LigoMapper
  { lmLocations :: [LigoIndexedInfo u]
  , lmTypes :: LigoTypesVec
  , lmMichelsonCode :: Micheline.Expression
  }

data EntrypointName = EntrypointName
  { enModule :: Text
  , enName :: Text
  } deriving stock (Show, Eq, Ord, Generic, Data)
    deriving anyclass (NFData)

newtype EntrypointsList = EntrypointsList { unEntrypoints :: [EntrypointName] }
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
    -- `"location"`
  , _ltdsRange     :: LigoRange
    -- | Definition body location
    -- `"body_location"`
  , _ltdsBodyRange :: LigoRange
  , _ltdsContent   :: LigoTypeExpression
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
  | LTFUnresolved
  deriving stock (Generic, Show)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

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

instance FromJSON LigoTypeLiteralValue where
  parseJSON val = asum
    [ twoElemParser val
    , LTLVUnit <$ guardOneElemList "Literal_unit" val
    ]
    where
      twoElemParser = genericParseJSON defaultOptions
        { sumEncoding = TwoElemArray
        , constructorTagModifier = ("Literal" ++) . toSnakeCase . drop 4
        }

instance FromJSON LigoString where
  parseJSON = genericParseJSON defaultOptions
    { sumEncoding = TwoElemArray
    , constructorTagModifier = drop 2
    }

instance FromJSON LigoTypeTable where
  parseJSON = withObject "LigoTypeTable" \o -> do
    _lttFields <- parseFields =<< o .: "fields"
    _lttLayout <- o .: "layout"
    pure LigoTypeTable{..}
    where
      parseFields :: [Value] -> Parser (HashMap Text LigoTypeExpression)
      parseFields = pure . HM.fromList <=< mapM parseElem

      parseElem :: Value -> Parser (Text, LigoTypeExpression)
      parseElem = withArray "TypeTableField" \arr -> do
        case V.toList arr of
          [ctor, content] -> do
            (_ :: Value, ctorValue) <- parseJSON ctor
            ctorName <- parseFromStringOrNumber ctorValue
            typeExpr <- parseJSON content
            pure (ctorName, typeExpr)
          _ -> fail "Expected two element array"

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

instance FromJSON LigoLayout where
  parseJSON = withArray "LigoLayout" \arr -> do
    case V.toList arr of
      [String ctor, val]
        | ctor == "Inner" -> LLInner <$> parseJSON val
        | ctor == "Field" ->
            case val of
              Object obj -> do
                (_ :: Value, nameValue) <- obj .: "name"
                name <- parseFromStringOrNumber nameValue
                pure $ LLField name
              _ -> fail [int||Expected object in constructor "Field"|]
        | otherwise -> fail [int||Unexpected constructor #{ctor}|]
      _ -> fail "Expected a two element array"

instance FromJSON (LigoVariable u) where
  parseJSON = fmap LigoVariable . parseJSON

instance Buildable LigoTypeRef where
  build (LigoTypeRef i) =  [int||type##{i}|]

-- We're writing this instance because sometimes
-- it's hard to construct @LigoType@ by hand (e.g. in tests).
-- So, we'll treat that types are equal if only their prettified representations
-- are equal.
instance Eq LigoType where
  lhs == rhs = pretty @_ @Text (buildType Caml lhs) == pretty @_ @Text (buildType Caml rhs)

instance (SingI u) => Buildable (LigoExposedStackEntry u) where
  build (LigoExposedStackEntry decl ty) =
    let declB = maybe (build unknownVariable) build decl
    in [int||elem #{declB} of #{buildLigoTypeF @u ty}|]

instance FromJSON LigoTypeRef where
  parseJSON val = parseJSON val >>= \(TextualNumber n) -> pure $ LigoTypeRef n

instance FromJSON (LigoExposedStackEntry 'Unique) where
  parseJSON = withObject "LIGO exposed stack entry" \o -> do
    leseType <- o .:! "source_type"
    leseDeclaration <- o .:! "name"
    return LigoExposedStackEntry{..}

instance (SingI u) => Buildable (LigoStackEntry u) where
  build = \case
    LigoStackEntry ese   -> build ese
    LigoHiddenStackEntry -> "<hidden elem>"

instance FromJSON (LigoStackEntry 'Unique) where
  parseJSON v = case v of
    Aeson.Null       -> pure LigoHiddenStackEntry
    Aeson.Object o
      | Aeson.null o -> pure LigoHiddenStackEntry
      | otherwise    -> LigoStackEntry <$> parseJSON v
    other            -> Aeson.unexpected other

instance Default (LigoIndexedInfo u) where
  def = LigoEmptyLocationInfo

instance (SingI u) => Buildable (LigoIndexedInfo u) where
  build = \case
    LigoEmptyLocationInfo -> "none"
    LigoIndexedInfo mLoc mEnv typ -> unlinesF $ catMaybes
      [ mLoc <&> \loc ->  [int||location: #{loc}|]
      , mEnv <&> \env -> nameF "environment stack" $ blockListF env
      , case typ of { Just ty -> Just [int||source type: #{buildLigoTypeF @u ty}|] ; _ -> Nothing }
      ]

instance FromJSON (LigoIndexedInfo 'Unique) where
  parseJSON = withObject "location info" \o -> do
    liiLocation <- o .:! "location"
    liiEnvironment <- o .:! "environment"
    liiSourceType <-  o .:! "source_type"
    return LigoIndexedInfo{..}

instance AsEmpty (LigoIndexedInfo u) where
  _Empty = prism
    do \() -> LigoEmptyLocationInfo
    do \case LigoEmptyLocationInfo -> Right (); other -> Left other

instance {-# OVERLAPPING #-} (SingI u) => Buildable [LigoIndexedInfo u] where
  build = blockListF

instance FromJSON (LigoMapper 'Unique) where
  parseJSON = withObject "ligo output" \o -> do
    mich <- o .: "michelson"
    lmMichelsonCode <- mich .: "expression"

    -- Here we are inlining types in @source_type@ fields.
    --
    -- Also, we need to replace textual numbers with fair
    -- json ones because the json-schema from source mapper
    -- differs from @ligo info get-scopes@ one in this place.
    lmTypes <- parseJSON . replaceTextualNumbers =<< o .: "types"
    lmLocations <- mich .: "locations"
    return LigoMapper{..}

-- This @Buildable@ instance handles only generated
-- main functions for module entrypoints.
-- For example:
-- 1. someFoo -> someFoo
-- 2. SomeModule.$main -> SomeModule
-- 3. $main -> Current module
instance Buildable EntrypointName where
  build EntrypointName{..}
    | not (T.null enModule) && enName /= generatedMainName = [int||#{enModule}.#{enName}|]
    | not (T.null enModule) || enName /= generatedMainName =
        [int||#{enModule}#{if enName == generatedMainName then "" else enName}|]
    | otherwise = [int||Current module|]

instance IsString EntrypointName where
  fromString = mkEntrypointName . toText

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

parseFromStringOrNumber :: Value -> Parser Text
parseFromStringOrNumber val = asum
  [ do
      TextualNumber (n :: Int) <- parseJSON val
      pure $ pretty n
  , parseJSON val
  ]

guardOneElemList :: Text -> Value -> Parser ()
guardOneElemList expected = withArray (toString expected) \arr -> do
  case V.length arr of
    1 -> do
      String ctor <- pure $ V.unsafeIndex arr 0
      guard (ctor == expected)
    len -> fail $ "Expected array of size 1, got " <> show len

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

fromLigoRangeOrDef :: LigoRange -> Range
fromLigoRangeOrDef = fromMaybe (point 0 0) . mbFromLigoRange

data NameKind = NameType | NameField FieldKind | NameModule
data FieldKind = FieldSum | FieldProduct

-- | Reconstruct `LIGO` tree out of `LigoTypeFull`.
fromLigoTypeFull :: LigoTypeFull -> LIGO Info
fromLigoTypeFull = \case
  LTFCore lte     -> fromLigoTypeExpression lte
  LTFResolved lte -> fromLigoTypeExpression lte
  LTFUnresolved   -> mkLigoError defaultState "unresolved"

fromLigoTypeExpression :: LigoTypeExpression -> LIGO Info
fromLigoTypeExpression
  LigoTypeExpression {..} =
    let st = putElem (fromLigoRangeOrDef _lteLocation) defaultState in
    fromLigoType st _lteTypeContent

fromLigoType
  :: Product Info
  -> LigoTypeContent
  -> LIGO Info
fromLigoType st = \case
  LTCConstant LigoTypeConstant {..} ->
    -- See: https://gitlab.com/ligolang/ligo/-/issues/1478
    fromLigoConstant (head _ltcInjection & over _head toLower) _ltcParameters

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
      [] -> mkErr "malformed sum type, please report this as a bug"
      v : vs ->
        let ligoLayout = _lttLayout sum in
        make' (st, TSum (ligoLayoutToLayout ligoLayout) (v :| vs))

  LTCSingleton literalValue -> fromLigoTypeLiteralValue literalValue
  LTCAbstraction _ -> mkErr "unsupported type Abstraction"    -- TODO not used

  LTCForAll LigoTypeForAll{..} ->
    let tyVar = fromLigoTypeExpression _ltfaType_ in
    make' (st, TVariable tyVar)

  LTCModuleAccessor LigoTypeModuleAccessor{..} ->
    let path = map (fromLigoPrimitive NameModule . _lvName) _ltmaModulePath in
    let element = fromLigoVariable _ltmaElement in
    make' (st, ModuleAccess path element)

  LTCApp LigoTypeApp{..} ->
    let n = fromLigoVariable _ltaTypeOperator in
    let p = fromLigoTypeExpression <$> _ltaArguments in
    make' (st, TApply n p)

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
      LTLVChest bts -> make' (st, CBytes bts)
      LTLVChestKey bts -> make' (st, CBytes bts)

    fromLigoString :: LigoString -> LIGO Info
    fromLigoString = \case
      LSStandard str -> make' (st, CString str)
      LSVerbatim str -> make' (st, Verbatim str)

    fromLigoVariable :: LigoVar -> LIGO Info
    fromLigoVariable = fromLigoPrimitive NameType . _lvName

    fromLigoPrimitive :: NameKind -> Text -> LIGO Info
    fromLigoPrimitive = \case
      NameType -> make' . (st,) . TypeName
      NameField FieldSum -> make' . (st,) . Ctor
      NameField FieldProduct -> make' . (st,) . FieldName
      NameModule -> make' . (st,) . ModuleName

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
        FieldSum     -> make' (st, Variant n type')
        FieldProduct -> make' (st, TField  n type')

    mkErr = mkLigoError st

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
    ges = List.filter (not . (`leq` i)) (extract <$> Data.Foldable.toList f)
    r = getElem (List.minimum ges) `merged` getElem (List.maximum ges)
    i' = putElem r i

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

stripSuffixHashVariable :: LigoVariable 'Unique -> LigoVariable 'Concise
stripSuffixHashVariable var = LigoVariable $ pretty var

mkTypeExpression :: LigoTypeContent -> LigoTypeExpression
mkTypeExpression content = LigoTypeExpression
  { _lteTypeContent = content
  , _lteLocation = LRVirtual "dummy"
  , _lteOrigVar = Nothing
  }

mkConstantType :: Text -> [LigoTypeExpression] -> LigoTypeExpression
mkConstantType typeName parameters = mkTypeExpression $ LTCConstant $
  LigoTypeConstant
    { _ltcParameters = parameters
    , _ltcLanguage = "Michelson"
    , _ltcInjection = singleton typeName
    }

mkArrowType :: LigoTypeExpression -> LigoTypeExpression -> LigoTypeExpression
mkArrowType domain codomain = mkTypeExpression $ LTCArrow $
  LigoTypeArrow
    { _ltaType2 = codomain
    , _ltaType1 = domain
    }

(~>) :: LigoTypeExpression -> LigoTypeExpression -> LigoTypeExpression
(~>) = mkArrowType

infixr 2 ~>

mkTypeTable :: LigoLayout -> [(Text, LigoTypeExpression)] -> LigoTypeTable
mkTypeTable layout keyValues = LigoTypeTable (HM.fromList keyValues) layout

mkRecordType :: LigoLayout -> [(Text, LigoTypeExpression)] -> LigoTypeExpression
mkRecordType layout keyValues = mkTypeTable layout keyValues
  & LTCRecord
  & mkTypeExpression

mkSumType :: LigoLayout -> [(Text, LigoTypeExpression)] -> LigoTypeExpression
mkSumType layout keyValues = mkTypeTable layout keyValues
  & LTCSum
  & mkTypeExpression

mkSimpleConstantType :: Text -> LigoTypeExpression
mkSimpleConstantType typ = mkConstantType typ []

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

buildLigoTypeF :: forall u. (SingI u) => LigoTypeF u -> Doc
buildLigoTypeF typ = case sing @u of
  SUnique -> build typ
  SConcise -> buildType Caml typ

mkEntrypointName :: Text -> EntrypointName
mkEntrypointName txt =
  let enName = T.takeWhileEnd (/= '.') txt in
  let enModule = T.dropEnd (T.length enName + 1) txt in
  EntrypointName{..}

makeLensesWith postfixLFields ''LigoExposedStackEntry
makeLensesWith postfixLFields ''LigoIndexedInfo
makePrisms ''LigoStackEntry

makeConciseLigoStackEntry :: LigoTypesVec -> LigoStackEntry 'Unique -> LigoStackEntry 'Concise
makeConciseLigoStackEntry vec = \case
  LigoStackEntry lese@LigoExposedStackEntry{..} -> LigoStackEntry lese
    { leseDeclaration = stripSuffixHashVariable <$> leseDeclaration
    , leseType = vec `readLigoType` leseType
    }
  LigoHiddenStackEntry -> LigoHiddenStackEntry

makeConciseLigoIndexedInfo :: LigoTypesVec -> LigoIndexedInfo 'Unique -> LigoIndexedInfo 'Concise
makeConciseLigoIndexedInfo vec indexedInfo =
  indexedInfo
    { liiEnvironment = liiEnvironment indexedInfo & _Just . each %~ makeConciseLigoStackEntry vec
    , liiSourceType = readLigoType vec <$> liiSourceType indexedInfo
    }

parseEntrypointsList :: Text -> Maybe EntrypointsList
parseEntrypointsList (lines -> parts) = do
  entrypoints <- safeTail >=> safeInit $ parts
  pure $ EntrypointsList (mkEntrypointName <$> entrypoints)
  where
    safeTail :: [a] -> Maybe [a]
    safeTail = fmap tail . nonEmpty

    safeInit :: [a] -> Maybe [a]
    safeInit = fmap init . nonEmpty

readLigoType :: LigoTypesVec -> Maybe LigoTypeRef -> LigoType
readLigoType (LigoTypesVec v) = \case
  Nothing -> LigoType Nothing
  Just (LigoTypeRef n) -> LigoType $ v V.!? n
