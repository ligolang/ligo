{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DeriveDataTypeable, StandaloneKindSignatures, UndecidableInstances #-}

-- | Types coming from @ligo@ executable.
module Language.LIGO.Debugger.CLI.Types
  ( module Language.LIGO.Debugger.CLI.Types
  ) where

import Control.Lens (AsEmpty (..), Each (each), forOf, makePrisms, prism)
import Control.Lens.Prism (_Just)
import Data.Aeson (FromJSON (..), Value (..), withObject, (.:!), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Lens (key, nth, values)
import Data.Aeson.Parser (scientific)
import Data.Aeson.Types qualified as Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.Char (isDigit)
import Data.Data (Data)
import Data.Default (Default (..))
import Data.List qualified as L
import Data.Scientific qualified as Sci
import Data.Singletons.TH (SingI (..), genSingletons)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Fmt (Buildable (..), Builder, blockListF, nameF, pretty)
import Fmt.Internal.Core (FromBuilder (..))
import Generics.SYB (everywhere, mkT)
import System.Console.ANSI
  (Color (Red), ColorIntensity (Dull), ConsoleIntensity (BoldIntensity), ConsoleLayer (Foreground),
  SGR (Reset, SetColor, SetConsoleIntensity))
import Text.Interpolation.Nyan (int, rmode')
import Util

import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Micheline.Expression qualified as Micheline
import Morley.Util.Lens
import Morley.Util.TypeLits (ErrorMessage (Text), TypeError)

import AST (Lang (..), Type, TypeName, lppDialect)
import Cli.Json (LigoTypeExpression, LigoTypeFull (LTFResolved), fromLigoTypeFull)
import Duplo (layer)

import Language.LIGO.Debugger.Error

-- | Sometimes numbers are carries as strings in order to fit into
-- common limits for sure.
newtype TextualNumber a = TextualNumber a
  deriving stock Functor
  deriving newtype NFData

instance Integral a => FromJSON (TextualNumber a) where
  parseJSON = \case
    Aeson.String t -> do
      i <- readMaybe @Integer (toString t)
        & maybe (fail "expected a number") pure
      fromIntegralNoOverflow i
        & either (fail . displayException) (pure . TextualNumber)
    Aeson.Number n -> do
      unless (Sci.isInteger n) $
        fail "Expected an integer number"
      let i :: Integer = round n
      fromIntegralNoOverflow i
        & either (fail . displayException) (pure . TextualNumber)
    other -> Aeson.unexpected other

-- | Position in a file.
data LigoPosition = LigoPosition
  { lpLine :: Int
    -- ^ 1-indexed line number
  , lpCol  :: Int
    -- ^ 0-indexed column number
  } deriving stock (Show, Eq, Ord, Generic, Data)
    deriving anyclass (NFData, Hashable)

instance Buildable LigoPosition where
  build (LigoPosition line col) = [int||#{line}:#{col}|]

instance FromJSON LigoPosition where
  parseJSON v = do
    startByte <- maybe (fail "Error parsing LIGO position") pure (v ^? nth 1 . key "start" . key "byte")
    flip (withObject "startByte") startByte $ \obj -> do
      lpLine <- obj .: "pos_lnum"
      column1 <- obj .: "pos_cnum"
      column0 <- obj .: "pos_bol"
      let lpCol = column1 - column0
      pure LigoPosition {..}

-- | Some LIGO location range.
data LigoRange = LigoRange
  { lrFile  :: FilePath
  , lrStart :: LigoPosition
  , lrEnd   :: LigoPosition
  } deriving stock (Show, Eq, Ord, Generic, Data)
    deriving anyclass (NFData, Hashable)

makeLensesWith postfixLFields ''LigoRange

instance Buildable LigoRange where
  build (LigoRange file start end) = [int||#{file}:#{start}-#{end}|]

instance FromJSON LigoRange where
  parseJSON = withObject "LIGO location range" \o -> do
    (file, startPos) <- parseLoc =<< o .: "start"
    (file', endPos) <- parseLoc =<< o .: "stop"
    when (file /= file') $
      fail "Can't handle a range spanning through multiple files"
    return $ LigoRange file startPos endPos
    where
      parseLoc = withObject "location" \o -> do
        file <- o .: "file"
        TextualNumber line <- o .: "line"
        TextualNumber col  <- o .: "col"
        when (line < 1) $ fail "Line number is zero"
        return (file, LigoPosition line col)

-- | Type marker, which stores information about
-- hashes presence in variable names.
data NameType
  = Unique
    -- ^ Variable name __may have (but not require)__ hashes.
    -- Example: @var#123@.
    --
    -- Up to our understanding, the rules are the following:
    --
    -- * Local variables __have__ hashes.
    -- * Top-level declarations within the module being run __don't have__
    -- hashes.
    -- * However, top-level declarations in the imported files __have__
    -- hashes.
    -- * Similarly, top-level declarations coming from @module@s __have__
    -- hashes.
    --
    -- Also, the main rule:
    -- * Two different variables will have different unique names.
    -- * The opposite is not true in some edge cases.
    --
    --   For instance, @advanced-curry.mligo@ from our tests has a nuance:
    --   in @apply@ function, variable @f@ will have numerous different
    --   unique names.
    --
    -- So whether can you rely on the names when distinguishing variables or not
    -- - depends on your use case and on your definition of "same" term
    -- on variables.
    --
    -- Not only the entire variable name is globally unique across the contract;
    -- the numeric identifier after the hash is globally unique too.
  | Concise
    -- ^ Variable name definitely doesn't have hashes.
    --
    -- For user-defined variables (and not LIGO-internal ones), concise
    -- representation will exactly match the name of the variable in the code.

genSingletons [''NameType]

newtype Name (u :: NameType) = Name Text
  deriving stock (Data)
  deriving newtype (Show, NFData, FromJSON)

deriving newtype instance FromBuilder (Name 'Concise)
deriving newtype instance IsString (Name 'Concise)
deriving newtype instance Eq (Name 'Concise)

instance (TypeError ('Text "You can't compare unique names directly. Please use \"compareUniqueNames\" for it")
         ) => Eq (Name 'Unique) where
  (==) = error "impossible"

compareUniqueNames :: Name 'Unique -> Name 'Unique -> Bool
compareUniqueNames (Name lhs) (Name rhs) = lhs == rhs

instance (SingI u) => Buildable (Name u) where
  build (Name varName) = case sing @u of
    SConcise -> build varName
    SUnique -> buildUnique varName
    where
      -- Here we want to pretty-print monomorphed variables.
      -- They have format like "poly_#SomeModule#NestedModule#foo_name_42"
      -- and we want to pretty-print them like "SomeModule.NestedModule.foo_name$42".
      buildUnique :: Text -> Builder
      buildUnique (T.stripPrefix "poly_" -> Just name)
        | not (T.null index) && T.all isDigit index && T.any (== '_') functionWithIndex =
          [int||#{moduleName}#{functionName}$#{index}|]
        where
          -- This should be non-empty
          splitted = T.split (== '#') $ T.dropWhile (== '#') name
          moduleParts = L.init splitted
          functionWithIndex = L.last splitted
          index = T.takeWhileEnd (/= '_') functionWithIndex
          moduleName
            | null moduleParts = ""
            | otherwise = T.intercalate "." moduleParts <> "."
          -- This @breakOnEnd@ shouldn't crash because of
          -- "T.any (== '_') functionWithIndex" check in guard above.
          (functionName, _) = first (T.dropEnd 1) $ T.breakOnEnd "_" functionWithIndex

      buildUnique name =
        -- Some variables may look like "varName#123". We want to strip that identifier.
        if T.all isDigit suffix
        then build $ T.dropEnd 1 $ T.dropWhileEnd (/= '#') name
        else build name
        where
          suffix = T.takeWhileEnd (/= '#') name

-- | Describes a variable.
newtype LigoVariable (u :: NameType) = LigoVariable
  { lvName :: Name u
  } deriving stock (Show, Generic, Data)
    deriving newtype (Buildable)
    deriving anyclass (NFData)

deriving stock instance Eq (LigoVariable 'Concise)

-- | Since we have @Maybe LigoVariable@ in @LigoExposedStackEntry@
-- we need to have some default variable for unknown variables.
unknownVariable :: Text
unknownVariable = "?"

instance FromJSON (LigoVariable u) where
  parseJSON = fmap LigoVariable . parseJSON

-- | This constant is used in cases when we can't find a name for a stack frame.
-- E.g. we're going into lambda function or in cycle.
--
-- We're using @<internal>@ name because these stack frames are related
-- to some internal @Michelson@ lambdas that @ligo@ can produce.
internalStackFrameName :: Text
internalStackFrameName = "<internal>"

stripSuffixHashVariable :: LigoVariable 'Unique -> LigoVariable 'Concise
stripSuffixHashVariable var = LigoVariable $ pretty var

-- | Reference to type description in the types map.
--
-- Not used at the moment.
newtype LigoTypeRef = LigoTypeRef { unLigoTypeRef :: Word }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Buildable LigoTypeRef where
  build (LigoTypeRef i) = [int||type##{i}|]

newtype LigoType = LigoType { unLigoType :: Maybe LigoTypeExpression }
  deriving stock (Data)
  deriving newtype (Show, Generic, NFData)

pattern LigoTypeResolved :: LigoTypeExpression -> LigoType
pattern LigoTypeResolved typ = LigoType (Just typ)

-- | Prettify @LigoType@ in provided dialect.
buildType :: Lang -> LigoType -> Builder
buildType lang LigoType{..} = case unLigoType of
  Nothing -> ""
  Just typExpr ->
    let
      tree = fromLigoTypeFull $ LTFResolved typExpr
      ppr = fromMaybe "" $ asum
        [ lppDialect lang <$> layer @AST.Type tree
        , lppDialect lang <$> layer @AST.TypeName tree
        ]
    in show ppr

-- We're writing this instance because sometimes
-- it's hard to construct @LigoType@ by hand (e.g. in tests).
-- So, we'll treat that types are equal if only their prettified representations
-- are equal.
instance Eq LigoType where
  lhs == rhs = buildType Caml lhs == buildType Caml rhs

-- | An element of the stack with some information interesting for us.
data LigoExposedStackEntry u = LigoExposedStackEntry
  { leseDeclaration :: Maybe (LigoVariable u)
  , leseType        :: LigoType
  } deriving stock (Show, Generic, Data)
    deriving anyclass (NFData)

deriving stock instance Eq (LigoExposedStackEntry 'Concise)

makeLensesWith postfixLFields ''LigoExposedStackEntry

instance (SingI u) => Buildable (LigoExposedStackEntry u) where
  build (LigoExposedStackEntry decl ty) =
    let declB = maybe (build unknownVariable) build decl
    in [int||elem #{declB} of #{buildType Caml ty}|]

instance FromJSON (LigoExposedStackEntry u) where
  parseJSON = withObject "LIGO exposed stack entry" \o -> do
    leseType <- LigoType <$> o .:! "source_type"
    leseDeclaration <- o .:! "name"
    return LigoExposedStackEntry{..}

-- | An element of the stack.
data LigoStackEntry u
  = LigoStackEntry (LigoExposedStackEntry u)
    -- ^ Stack entry with known details.
  | LigoHiddenStackEntry
    -- ^ Stack entry that is unknown.
    -- This can denote some auxiliary entry added by LIGO, like
    -- reusable functions or part of sum type when unfolding via @IF_LEFT@s.
  deriving stock (Show, Generic, Data)
  deriving anyclass (NFData)

deriving stock instance Eq (LigoStackEntry 'Concise)

makePrisms ''LigoStackEntry

instance (SingI u) => Buildable (LigoStackEntry u) where
  build = \case
    LigoStackEntry ese   -> build ese
    LigoHiddenStackEntry -> "<hidden elem>"

pattern LigoStackEntryNoVar :: LigoType -> LigoStackEntry u
pattern LigoStackEntryNoVar ty = LigoStackEntry LigoExposedStackEntry
  { leseDeclaration = Nothing
  , leseType = ty
  }

pattern LigoStackEntryVar :: Text -> LigoType -> LigoStackEntry 'Concise
pattern LigoStackEntryVar name ty = LigoStackEntry LigoExposedStackEntry
  { leseDeclaration = Just LigoVariable{ lvName = Name name }
  , leseType = ty
  }

instance FromJSON (LigoStackEntry u) where
  parseJSON v = case v of
    Aeson.Null       -> pure LigoHiddenStackEntry
    Aeson.Object o
      | Aeson.null o -> pure LigoHiddenStackEntry
      | otherwise    -> LigoStackEntry <$> parseJSON v
    other            -> Aeson.unexpected other

-- | Variables associated with each element on stack.
--
-- Entries are listed in top-to-bottom order.
type LigoStack u = [LigoStackEntry u]

stripSuffixHashLigoStackEntry :: LigoStackEntry 'Unique -> LigoStackEntry 'Concise
stripSuffixHashLigoStackEntry = \case
  LigoStackEntry lese@LigoExposedStackEntry{..} -> LigoStackEntry lese
    { leseDeclaration = stripSuffixHashVariable <$> leseDeclaration
    }
  LigoHiddenStackEntry -> LigoHiddenStackEntry

-- | All the information provided for specific point of a Michelson program.
data LigoIndexedInfo u = LigoIndexedInfo
  { liiLocation    :: Maybe LigoRange
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

  } deriving stock (Show, Generic, Data)
    deriving anyclass (NFData)

deriving stock instance Eq (LigoIndexedInfo 'Concise)

makeLensesWith postfixLFields ''LigoIndexedInfo

instance Default (LigoIndexedInfo u) where
  def = LigoIndexedInfo Nothing Nothing

instance (SingI u) => Buildable (LigoIndexedInfo u) where
  build = \case
    LigoEmptyLocationInfo -> "none"
    LigoIndexedInfo mLoc mEnv -> mconcat $ catMaybes
      [ mLoc <&> \loc -> [int||location: #{loc}|]
      , mEnv <&> \env -> nameF "environment stack" $ blockListF env
      ]

instance FromJSON (LigoIndexedInfo u) where
  parseJSON = withObject "location info" \o -> do
    liiLocation <- o .:! "location"
    liiEnvironment <- o .:! "environment"
    return LigoIndexedInfo{..}

pattern LigoEmptyLocationInfo :: LigoIndexedInfo u
pattern LigoEmptyLocationInfo = LigoIndexedInfo Nothing Nothing

pattern LigoMereLocInfo :: LigoRange -> LigoIndexedInfo u
pattern LigoMereLocInfo loc = LigoIndexedInfo
  { liiLocation = Just loc
  , liiEnvironment = Nothing
  }

pattern LigoMereEnvInfo :: LigoStack u -> LigoIndexedInfo u
pattern LigoMereEnvInfo env = LigoIndexedInfo
  { liiLocation = Nothing
  , liiEnvironment = Just env
  }

stripSuffixHashFromLigoIndexedInfo :: LigoIndexedInfo 'Unique -> LigoIndexedInfo 'Concise
stripSuffixHashFromLigoIndexedInfo indexedInfo =
  indexedInfo & liiEnvironmentL . _Just . each %~ stripSuffixHashLigoStackEntry

instance AsEmpty (LigoIndexedInfo u) where
  _Empty = prism
    do \() -> LigoIndexedInfo Nothing Nothing
    do \case LigoIndexedInfo Nothing Nothing -> Right (); other -> Left other

instance {-# OVERLAPPING #-} (SingI u) => Buildable [LigoIndexedInfo u] where
  build = blockListF

-- | The debug output produced by LIGO.
data LigoMapper u = LigoMapper
  { lmLocations :: [LigoIndexedInfo u]
  , lmMichelsonCode :: Micheline.Expression
  }

instance FromJSON (LigoMapper u) where
  parseJSON = withObject "ligo output" \o -> do
    mich <- o .: "michelson"
    lmMichelsonCode <- mich .: "expression"

    -- Here we are inlining types in @source_type@ fields.
    --
    -- Also, we need to replace textual numbers with fair
    -- json ones because the json-schema from source mapper
    -- differs from @ligo info get-scopes@ one in this place.
    Array types <- replaceTextualNumbers <$> o .: "types"
    locations <- mich .: "locations"
    locationsInlined <-
      forOf (values . key "environment" . values . key "source_type") (Array locations) \old -> do
        TextualNumber index <- parseJSON old
        maybe (fail $ "Undexpected out of bounds with index " <> show index) pure (types V.!? index)

    lmLocations <- parseJSON locationsInlined
    return LigoMapper{..}
    where
      replaceTextualNumbers :: Value -> Value
      replaceTextualNumbers = everywhere $ mkT \case
        str@(String val) -> parseOnly scientific (T.encodeUtf8 val)
          & either (const str) Number
        other -> other

-- | Something got wrong on @ligo@ executable's side.
newtype LigoCallException = LigoCallException { leMessage :: Text }
  deriving newtype (Eq, Show, FromBuilder)

instance Default LigoCallException where
  def = LigoCallException ""

-- | If we have malformed LIGO contract then we'll see
-- in error @ligo@ binary output a red-colored text
-- which represents a place where error occurred.
replaceANSI :: Text -> Text
replaceANSI =
    T.replace [int||#ansi{[Reset]}|] "<--"
  . T.replace
      [int||#ansi{[SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Red]}|] "-->"

instance Buildable LigoCallException where
  build LigoCallException{..} = build $ replaceANSI leMessage

instance Exception LigoCallException where
  displayException = pretty

instance DebuggerException LigoCallException where
  type ExceptionTag LigoCallException = "LigoCall"
  debuggerExceptionType _ = LigoLayerException
  shouldInterruptDebuggingSession = False

-- | Failed to decode LIGO output.
data LigoDecodeException = LigoDecodeException
  { ldeSource :: Text
    -- ^ What we were trying to decode.
  , ldeMessage :: Text
    -- ^ The error message.
  } deriving stock (Show)

instance Buildable LigoDecodeException where
  build LigoDecodeException{..} =
    [int||Failed to parse LIGO output (#{ldeSource}): #{ldeMessage}|]

instance Exception LigoDecodeException where
  displayException = pretty

instance DebuggerException LigoDecodeException where
  type ExceptionTag LigoDecodeException = "LigoDecode"
  debuggerExceptionType _ = MidLigoLayerException
  shouldInterruptDebuggingSession = False

newtype EntrypointsList = EntrypointsList { unEntrypoints :: [String] }
  deriving newtype (Buildable)

parseEntrypointsList :: Text -> Maybe EntrypointsList
parseEntrypointsList (lines -> parts) = do
  entrypoints <- safeTail >=> safeInit $ parts
  pure $ EntrypointsList (toString <$> entrypoints)
  where
    safeTail :: [a] -> Maybe [a]
    safeTail = fmap tail . nonEmpty

    safeInit :: [a] -> Maybe [a]
    safeInit = fmap init . nonEmpty

-- TODO: move this instance to morley-debugger
instance FromBuilder DAP.Message where
  fromBuilder txt = DAP.defaultMessage
    { DAP.formatMessage = fromBuilder txt
    }

-- | Invalid debugger configuration provided.
newtype ConfigurationException = ConfigurationException Text
  deriving newtype (Show, Buildable, FromBuilder)

instance Exception ConfigurationException where
  displayException (ConfigurationException msg) = pretty msg

instance DebuggerException ConfigurationException where
  type ExceptionTag ConfigurationException = "Configuration"
  debuggerExceptionType _ = UserException
  shouldInterruptDebuggingSession = False

-- | Some unexpected error in communication with the plugin.
newtype PluginCommunicationException = PluginCommunicationException Text
  deriving newtype (Show, Buildable, FromBuilder)

instance Exception PluginCommunicationException where
  displayException = pretty

instance DebuggerException PluginCommunicationException where
  type ExceptionTag PluginCommunicationException = "PluginComminication"
  debuggerExceptionType _ = MidPluginLayerException
  shouldInterruptDebuggingSession = True
