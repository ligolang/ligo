-- | Types coming from @ligo@ executable.
module Language.LIGO.Debugger.CLI.Types
  ( module Language.LIGO.Debugger.CLI.Types
  ) where

import Control.Lens (AsEmpty (..), forOf, prism)
import Data.Aeson
  (FromJSON (..), Value (..), withArray, withObject, withText, (.!=), (.:!), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Lens (key, nth, values)
import Data.Aeson.Types qualified as Aeson
import Data.Char (isDigit)
import Data.Default (Default (..))
import Data.List qualified as L
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Data.Vector qualified as V
import Fmt (Buildable (..), blockListF, mapF, nameF, pretty, tupleF)
import Fmt.Internal.Core (FromBuilder (..))
import Morley.Micheline.Expression qualified as Micheline
import Text.Interpolation.Nyan (int, rmode')

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
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

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
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

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

-- | Describes a variable.
newtype LigoVariable = LigoVariable
  { lvName :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

instance FromJSON LigoVariable where
  -- Some variables may look like "varName#123". We want to strip that identifier.
  parseJSON = withText "variable" \t -> do
    let suffix = T.takeWhileEnd (/= '#') t
    if T.all isDigit suffix
    then pure $ LigoVariable $ T.dropEnd 1 $ T.dropWhileEnd (/= '#') t
    else pure $ LigoVariable t

instance Buildable LigoVariable where
  -- Here we want to pretty-print monomorphed variables.
  -- They have format like "poly_#SomeModule#NestedModule#foo_name_42"
  -- and we want to pretty-print them like "SomeModule.NestedModule.foo_name$42".
  build (LigoVariable (T.stripPrefix "poly_" -> Just name))
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

  build (LigoVariable name) = build name

-- | Reference to type description in the types map.
--
-- Not used at the moment.
newtype LigoTypeRef = LigoTypeRef { unLigoTypeRef :: Word }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Buildable LigoTypeRef where
  build (LigoTypeRef i) = [int||type##{i}|]

-- TODO: in ligo lsp all these types declared as mixed sum and product type (which is obviously unsafe)
-- We can delete these types when this issue is resolved in lsp.

-- | Description of a type.
data LigoType
  = LTConstant LigoTypeConstant
  | LTVariable LigoTypeVariable
  -- | `"t_record"`
  -- A map from field name to its type.
  | LTRecord (HashMap Text LigoType)
  | LTApp LigoTypeApp
  | LTArrow LigoTypeArrow
  | LTUnresolved
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

instance FromJSON LigoType where
  -- If value is @Array@ then it is just a '"type_content"' field from a "type" object.
  parseJSON val@Array{} = parseJSON $ Object $ Aeson.singleton "type_content" val
  parseJSON val = flip (withObject "type") val \o -> do
    value <- o .: "type_content"
    flip (withArray "type_content") value \lst -> do
      typ <- maybe (fail "Expected list with length 2") pure (lst V.!? 1)
      asum
        [ LTConstant <$> parseJSON typ
        , LTVariable <$> parseJSON typ
        , LTApp <$> parseJSON typ
        , LTArrow <$> parseJSON typ
        , flip (withObject "T_record") typ \o' -> do -- "t_record"
            parsed <- sequence $ parseJSON <$> o'
            pure $ LTRecord $ Aeson.toHashMapText parsed
        , pure LTUnresolved
        ]

instance Buildable LigoType where
  build = \case
    LTConstant constant -> build constant
    LTVariable variable -> build variable
    LTRecord record -> mapF record
    LTApp app -> build app
    LTArrow arrow -> build arrow
    LTUnresolved -> ""

-- | `"t_constant"`
data LigoTypeConstant = LigoTypeConstant
  { -- | Arguments to which the type is applied.
    -- This list is not empty when we have complex constant values.
    --
    -- For example
    -- @
    -- let lst = [1;2;3] in
    -- @
    --
    -- In that case @injection@ would be `"list"` and in "parameters" we will see
    -- `"t_constant"` with `"int"`.
    ltcParameters :: [LigoType]
    -- | Type name.
  , ltcInjection :: NonEmpty Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

instance FromJSON LigoTypeConstant where
  parseJSON = withObject "T_constant" \o -> do
    injection <- o .: "injection"
    ltcParameters <- o .: "parameters"
    ltcInjection <- maybe (fail "Expected non-empty injection") pure (nonEmpty injection)
    pure LigoTypeConstant{..}

instance Buildable LigoTypeConstant where
  build LigoTypeConstant{..} =
    if null ltcParameters
      then build $ head ltcInjection
      else tupleF ltcParameters <> build (head ltcInjection)

-- | `"t_variable"`
newtype LigoTypeVariable = LigoTypeVariable
  { -- | Type name.
    ltvName :: Text
  } deriving stock (Show, Eq, Generic)
    deriving newtype (Buildable)
    deriving anyclass (NFData)

instance FromJSON LigoTypeVariable where
  parseJSON = withObject "T_variable" \o -> do
    ltvName <- o .: "name"
    pure LigoTypeVariable{..}

-- | `"t_app"`
data LigoTypeApp = LigoTypeApp
  { -- | Type operator name.
    ltaTypeOperator :: Text
    -- | Arguments to which the type is applied.
  , ltaArguments :: [LigoType]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

instance FromJSON LigoTypeApp where
  parseJSON = withObject "T_app" \o -> do
    ltaTypeOperator <- o .: "type_operator" >>= \o' -> o' .: "name"
    ltaArguments <- o .: "arguments"
    pure LigoTypeApp{..}

instance Buildable LigoTypeApp where
  build LigoTypeApp{..} = tupleF ltaArguments <> build ltaTypeOperator

-- | `"t_arrow"`, note that the order of its arguments is reversed.
data LigoTypeArrow = LigoTypeArrow -- "type2" -> "type1"
  { -- | Domain type.
    ltaType2 :: LigoType
    -- | Image type.
  , ltaType1 :: LigoType
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

instance FromJSON LigoTypeArrow where
  parseJSON = withObject "T_arrow" \o -> do
    ltaType1 <- o .: "type1"
    ltaType2 <- o .: "type2"
    pure LigoTypeArrow{..}

instance Buildable LigoTypeArrow where
  build LigoTypeArrow{..} = [int||#{ltaType2} -> #{ltaType1}|]

-- | An element of the stack with some information interesting for us.
data LigoExposedStackEntry = LigoExposedStackEntry
  { leseDeclaration :: Maybe LigoVariable
  , leseType        :: LigoType
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

instance Buildable LigoExposedStackEntry where
  build (LigoExposedStackEntry decl ty) =
    let declB = maybe "?" build decl
    in [int||elem #{declB} of #{ty}|]

instance FromJSON LigoExposedStackEntry where
  parseJSON = withObject "LIGO exposed stack entry" \o -> do
    leseType <- o .: "source_type"
    leseDeclaration <- o .:! "name"
    return LigoExposedStackEntry{..}

-- | An element of the stack.
data LigoStackEntry
  = LigoStackEntry LigoExposedStackEntry
    -- ^ Stack entry with known details.
  | LigoHiddenStackEntry
    -- ^ Stack entry that is unknown.
    -- This can denote some auxiliary entry added by LIGO, like
    -- reusable functions or part of sum type when unfolding via @IF_LEFT@s.
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance Buildable LigoStackEntry where
  build = \case
    LigoStackEntry ese   -> build ese
    LigoHiddenStackEntry -> "<hidden elem>"

pattern LigoStackEntryNoVar :: LigoType -> LigoStackEntry
pattern LigoStackEntryNoVar ty = LigoStackEntry LigoExposedStackEntry
  { leseDeclaration = Nothing
  , leseType = ty
  }

pattern LigoStackEntryVar :: Text -> LigoType -> LigoStackEntry
pattern LigoStackEntryVar name ty = LigoStackEntry LigoExposedStackEntry
  { leseDeclaration = Just LigoVariable{ lvName = name }
  , leseType = ty
  }

instance FromJSON LigoStackEntry where
  parseJSON v = case v of
    Aeson.Null       -> pure LigoHiddenStackEntry
    Aeson.Object o
      | Aeson.null o -> pure LigoHiddenStackEntry
      | otherwise    -> LigoStackEntry <$> parseJSON v
    other            -> Aeson.unexpected other

type LigoStack = [LigoStackEntry]

-- | All the information provided for specific point of a Michelson program.
data LigoIndexedInfo = LigoIndexedInfo
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

  , liiEnvironment :: Maybe LigoStack
    {- ^ Info about LIGO stack at the current point.

      For each value in /Michelson stack/ it contains a corresponding LIGO
      variable (if such is associated). But this is limited to the part of stack
      related in the current stack frame.

      This metadata is usually set between LIGO statements.

    -}

  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

instance Buildable LigoIndexedInfo where
  build = \case
    LigoEmptyLocationInfo -> "none"
    LigoIndexedInfo mLoc mEnv -> mconcat $ catMaybes
      [ mLoc <&> \loc -> [int||location: #{loc}|]
      , mEnv <&> \env -> nameF "environment stack" $ blockListF env
      ]

instance FromJSON LigoIndexedInfo where
  parseJSON = withObject "location info" \o -> do
    liiLocation <- o .:! "location"
    liiEnvironment <- o .:! "environment"
    return LigoIndexedInfo{..}

pattern LigoEmptyLocationInfo :: LigoIndexedInfo
pattern LigoEmptyLocationInfo = LigoIndexedInfo Nothing Nothing

pattern LigoMereLocInfo :: LigoRange -> LigoIndexedInfo
pattern LigoMereLocInfo loc = LigoIndexedInfo
  { liiLocation = Just loc
  , liiEnvironment = Nothing
  }

pattern LigoMereEnvInfo :: LigoStack -> LigoIndexedInfo
pattern LigoMereEnvInfo env = LigoIndexedInfo
  { liiLocation = Nothing
  , liiEnvironment = Just env
  }

instance AsEmpty LigoIndexedInfo where
  _Empty = prism
    do \() -> LigoIndexedInfo Nothing Nothing
    do \case LigoIndexedInfo Nothing Nothing -> Right (); other -> Left other

instance {-# OVERLAPPING #-} Buildable [LigoIndexedInfo] where
  build = blockListF

-- | The debug output produced by LIGO.
data LigoMapper = LigoMapper
  { lmLocations :: V.Vector LigoIndexedInfo
  , lmMichelsonCode :: Micheline.Expression
  }

instance FromJSON LigoMapper where
  parseJSON = withObject "ligo output" \o -> do
    mich <- o .: "michelson"
    lmMichelsonCode <- mich .: "expression"

    -- Here we are inlining types in @source_type@ fields.
    Array types <- o .: "types"
    locations <- mich .: "locations"
    locationsInlined <-
      forOf (values . key "environment" . values . key "source_type") (Array locations) \old -> do
        TextualNumber index <- parseJSON old
        maybe (fail $ "Undexpected out of bounds with index " <> show index) pure (types V.!? index)

    lmLocations <- parseJSON locationsInlined
    return LigoMapper{..}

data LigoException = LigoException
  { leMessage :: Text
  , leDescription :: Text
  , leLocation :: Maybe LigoPosition
  }
  deriving stock (Eq, Show)

instance FromJSON LigoException where
  parseJSON = withObject "LIGO output" $ \o -> do
    content <- o .: "content"
    flip (withObject "Content") content $ \c -> do
      leMessage <- c .: "message"
      leDescription <- c .:? "description" .!= ""
      location <- c .:? "location"
      leLocation <- maybe (pure Nothing) parseJSON location
      pure LigoException{..}

instance Default LigoException where
  def = LigoException "" "" Nothing

instance Exception LigoException where
  displayException = pretty

instance Buildable LigoException where
  build (LigoException msg desc loc) =
    [int||
        #{msg}
        #{desc}
        #{loc}
    |]

instance FromBuilder LigoException where
  fromBuilder b = def {leMessage = fromBuilder b}

newtype EntrypointsList = EntrypointsList { unEntrypoints :: [String] }

instance FromJSON EntrypointsList where
  parseJSON = withObject "list-declarations" \o -> do
    lst <- o .: "declarations"
    pure $ EntrypointsList lst
