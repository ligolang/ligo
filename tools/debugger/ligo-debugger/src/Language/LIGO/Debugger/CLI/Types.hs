-- | Types coming from @ligo@ executable.
module Language.LIGO.Debugger.CLI.Types
  ( module Language.LIGO.Debugger.CLI.Types
  ) where

import Control.Lens (AsEmpty (..), prism)
import Data.Aeson (FromJSON (..), withObject, (.:!), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Scientific qualified as Sci
import Data.Vector qualified as V
import Fmt (Buildable (..), blockListF, nameF)
import Morley.Micheline.Expression qualified as Micheline
import Text.Interpolation.Nyan

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
  { lpLine :: Word
    -- ^ 1-indexed line number
  , lpCol  :: Word
    -- ^ 0-indexed column number
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

instance Buildable LigoPosition where
  build (LigoPosition line col) = [int||#{line}:#{col}|]

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

instance Buildable LigoVariable where
  build (LigoVariable name) = build name

-- | Reference to type description in the types map.
--
-- Not used at the moment.
newtype LigoTypeRef = LigoTypeRef { unLigoTypeRef :: Word }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Buildable LigoTypeRef where
  build (LigoTypeRef i) = [int||type##{i}|]

-- | Description of a type.
data LigoType = LigoType
  { -- TODO: pick from LSP
  }

-- | An element of the stack with some information interesting for us.
data LigoExposedStackEntry = LigoExposedStackEntry
  { leseDeclaration :: Maybe LigoVariable
  , leseType        :: LigoTypeRef
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

instance Buildable LigoExposedStackEntry where
  build (LigoExposedStackEntry decl ty) =
    let declB = maybe "?" build decl
    in [int||elem #{declB} of #{ty}|]

instance FromJSON LigoExposedStackEntry where
  parseJSON = withObject "LIGO exposed stack entry" \o -> do
    TextualNumber leseType <- LigoTypeRef <<$>> o .: "source_type"
    leseDeclaration <- LigoVariable <<$>> o .:! "name"
    return LigoExposedStackEntry{..}

-- | An element of the stack.
data LigoStackEntry
  = LigoStackEntry LigoExposedStackEntry
    -- ^ Stack entry with known details.
  | LigoHiddenStackEntry
    -- ^ Stack entry that is unknown.
    -- This can denote some auxiliary entry added by LIGO, like reusable functions.
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance Buildable LigoStackEntry where
  build = \case
    LigoStackEntry ese   -> build ese
    LigoHiddenStackEntry -> "<hidden elem>"

pattern LigoStackEntryNoVar :: LigoTypeRef -> LigoStackEntry
pattern LigoStackEntryNoVar ty = LigoStackEntry LigoExposedStackEntry
  { leseDeclaration = Nothing
  , leseType = ty
  }

pattern LigoStackEntryVar :: Text -> LigoTypeRef -> LigoStackEntry
pattern LigoStackEntryVar name ty = LigoStackEntry LigoExposedStackEntry
  { leseDeclaration = Just LigoVariable{ lvName = name }
  , leseType = ty
  }

instance FromJSON LigoStackEntry where
  parseJSON v = case v of
    Aeson.Null     -> pure LigoHiddenStackEntry
    Aeson.Object{} -> LigoStackEntry <$> parseJSON v
    other          -> Aeson.unexpected other

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
  { lmTypes :: V.Vector LigoType
  , lmLocations :: V.Vector LigoIndexedInfo
  , lmMichelsonCode :: Micheline.Expression
  }

instance FromJSON LigoMapper where
  parseJSON = withObject "ligo output" \o -> do
    mich <- o .: "michelson"
    lmMichelsonCode <- mich .: "expression"
    lmLocations <- mich .: "locations"
    let lmTypes = mempty
    return LigoMapper{..}
