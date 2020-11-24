{-# LANGUAGE DeriveGeneric, RecordWildCards #-}

-- | The definition of type as is represented in ligo JSON output
module Cli.Json
  ( LigoError (..)
  , LigoErrorContent (..)
  , LigoScope (..)
  , LigoDefinitions (..)
  , LigoDefinitionsInner (..)
  , LigoDefinitionScope (..)
  , LigoTypeFull (..)
  , LigoTypeContent (..)
  , LigoTypeContentInner (..)
  , LigoRecordField (..)
  , LigoRange (..)
  , LigoRangeInner (..)
  , LigoByte (..)
  , mbFromLigoRange
  , fromLigoRangeOrDef
  , fromLigoErrorToMsg
  , prepareField
  )
where

import Control.Applicative (Alternative ((<|>)), liftA2)
import Data.Aeson.Types hiding (Error)
import Data.Char (isUpper, toLower)
import Data.Foldable (asum, toList)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Duplo.Pretty
import GHC.Generics

import AST.Skeleton (Error (..))
import Parser (Msg)
import Range

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- | Node representing ligo error with additional meta
data LigoError = LigoError
  { _leStatus :: Text
  , _leStage :: Text
  , _leContent :: LigoErrorContent
  }
  deriving stock (Generic, Show)

-- | An actual ligo error
data LigoErrorContent = LigoErrorContent
  { _lecMessage :: Text
  , _lecLocation :: LigoRange
  }
  deriving stock (Generic, Show)

-- | Whole ligo `get-scope` output
data LigoDefinitions = LigoDefinitions
  { _ldDefinitions :: LigoDefinitionsInner
  , _ldScopes :: [LigoScope]
  }
  deriving stock (Generic, Show)

-- | First part under `"variables"` constraint
data LigoDefinitionsInner = LigoDefinitionsInner
  { _ldiVariables :: HM.HashMap Text LigoDefinitionScope
  }
  deriving stock (Generic, Show)

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
    _lsRange :: LigoRange
  , _lsExpressionEnvironment :: [Text]
  , _lsTypeEnvironment :: Value
  }
  deriving stock (Generic, Show)

-- | Definition declaration that goes from `"definitions"` constraint
-- ```
-- { "definitions" { a#n : LigoDefinitionScope } }
-- ```
data LigoDefinitionScope = LigoDefinitionScope
  { _ldsName :: Text
  , _ldsRange :: LigoRange
  , _ldsBodyRange :: LigoRange
  , _ldsT :: Maybe LigoTypeFull
  , _ldsReferences :: Value
  }
  deriving stock (Generic, Show)

-- | Whole ligo type.
-- ```
-- { "t" : LigoTypeFull }
-- ```
data LigoTypeFull = LigoTypeFull
  { -- | Location of the definition.
    _ltLocation :: LigoRange
  , -- | *Some* meta constructors (e.g. `Some`).
    _ltTypeMeta :: Value
  , -- | We parse it by a chunks of 2, each odd element of array is a name for
    -- even element which is `LigoTypeContentInner`.
    -- ```
    -- { "type_content": [ <name>, LigoTypeContentInner ] }
    -- ```
    _ltTypeContent :: [LigoTypeContent]
  }
  deriving stock (Generic, Show)

-- | A pair in "type_content" array `[name, content]`.
-- ```
-- { "type_content": LigoTypeContent }
-- ```
data LigoTypeContent = LigoTypeContent
  { _ltcName :: Text
  , _ltcContentInner :: LigoTypeContentInner
  }
  deriving stock (Generic, Show)

-- | Inner object representing type content that depends on `name` in `LigoTypeContent`.
-- ```
-- { "type_content": [ <type>, LigoTypeContentInner ] }
-- ```
data LigoTypeContentInner
  = -- | Type call represented by the list of arguments and its constructor.
    LTCConst
      { _ltciArguments :: [Text]
      , _ltciTypeConst :: Value
      }
  | -- | Record type.
    LTCRecord (HM.HashMap Text LigoRecordField) -- TODO: continue
  | -- | Arrow type, note that the order of its arguments is reversed.
    LTCArrow -- "type2" -> "type1"
      { _ltciType2 :: LigoTypeFull
      , _ltciType1 :: LigoTypeFull
      }
  deriving stock (Generic, Show)

-- | Record field type value.
-- ```
-- { "type_content": ["T_record", { "key": LigoRecordField } ] }
-- ```
data LigoRecordField = LigoRecordField
  { -- | Declaration position (don't ask me I too don't know what actual
    -- position is this since from all the example it's somewhat always 0).
    _lrfDeclPos :: Int
  , -- | How the value is represented in michelson, currently ignored
    -- during parsing.
    _lrfMichelsonAnnotation :: Value
  , -- | The type itself.
    _lrfAssociatedType :: LigoTypeFull
  }
  deriving stock (Generic, Show)

-- | Location of definition.
-- ```
-- { "location": LigoRange }
-- ```
data LigoRange
  = Virtual Text
  | LigoRange
      { _lrStart :: LigoRangeInner
      , _lrStop :: LigoRangeInner
      }
  deriving stock (Generic, Show)

-- | Insides of ligo location.
-- ```
-- { ["start" | "stop"]: LigoRangeInner }
-- ```
data LigoRangeInner = LigoRangeInner
  { _lriByte :: LigoByte
  , _lriPointNum :: Int
  , _lriPointBol :: Int
  }
  deriving stock (Generic, Show)

-- | Byte representation of ligo location.
-- ```
-- { "byte": LigoByte }
-- ```
data LigoByte = LigoByte
  { _lbPosFname :: FilePath
  , _lbPosLnum :: Int
  , _lbPosBol :: Int
  , _lbPosCnum :: Int
  }
  deriving stock (Generic, Show)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance FromJSON LigoError where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance ToJSON LigoError where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON LigoErrorContent where
  parseJSON = withObject "error_content" $ \o -> do
    _lecMessage <- o .: "message"
    _lecLocation <- parseLigoRange "location_error_inner_range" =<< o .: "location"
    return $ LigoErrorContent {..}

-- TODO: malformed
instance ToJSON LigoErrorContent where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoDefinitions where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance ToJSON LigoDefinitions where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON LigoDefinitionsInner where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON LigoDefinitionsInner where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoScope where
  parseJSON = withObject "scope" $ \o -> do
    _lsRange <- parseLigoRange "scope_range" =<< o .: "range"
    _lsTypeEnvironment <- o .: "type_environment"
    _lsExpressionEnvironment <- o .: "expression_environment"
    return $ LigoScope {..}

-- TODO: malformed
instance ToJSON LigoScope where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON LigoDefinitionScope where
  parseJSON = withObject "scope" $ \o -> do
    _ldsName <- o .: "name"
    _ldsRange <- parseLigoRange "scope_range" =<< o .: "range"
    _ldsBodyRange <- parseLigoRange "scope_body_range" =<< o .: "body_range"
    _ldsT <- o .:? "t"
    _ldsReferences <- o .: "references"
    return $ LigoDefinitionScope {..}

-- TODO: malformed
instance ToJSON LigoDefinitionScope where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

-- TODO: We trust ligo compiler output for printing even number
-- of array elements.
instance FromJSON LigoTypeFull where
  parseJSON = withObject "type_full" $ \o -> do
    _ltLocation <- parseLigoRange "location_range" =<< o .: "location"
    type_content <- o .: "type_content"
    _ltTypeContent <-
      withArray "type_content" (mapM proceed . group 2 . toList) type_content
    _ltTypeMeta <- o .: "type_meta"
    return $ LigoTypeFull {..}
    where
      proceed [name, value] = do
        _ltcName <- parseJSON @Text name
        _ltcContentInner <- parseJSON @LigoTypeContentInner value
        return $ LigoTypeContent {..}
      proceed _ = error "number of type content elements is not even and cannot be grouped"

instance ToJSON LigoTypeFull where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON LigoTypeContent where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON LigoTypeContent where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoTypeContentInner where
  parseJSON = withObject "type_content" $ \o ->
    asum
      [ LTCConst <$> o .: "arguments" <*> o .: "type_constant"
      , LTCRecord <$> sequence (parseJSON @LigoRecordField <$> o)
      , LTCArrow <$> o .: "type2" <*> o .: "type1"
      ]

instance ToJSON LigoTypeContentInner where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoRecordField where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON LigoRecordField where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoRange where
  parseJSON = liftA2 (<|>) parseAsString parseAsObject
    where
      parseAsString (String o) = return $ Virtual o
      parseAsString _ = fail "failed to parse as string"
      parseAsObject = withObject "range" $ \o -> do
        _lrStart <- o .: "start"
        _lrStop <- o .: "stop"
        return $ LigoRange {..}


instance ToJSON LigoRange where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON LigoRangeInner where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON LigoRangeInner where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoByte where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance ToJSON LigoByte where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

-- | Construct a parser for ligo ranges that are represented in pairs
-- ```
-- [ "name", <LigoRange> ]
-- ```
parseLigoRange :: String -> Value -> Parser LigoRange
parseLigoRange = flip withArray (safeExtract . group 2 . toList)
  where
    safeExtract :: [[Value]] -> Parser LigoRange
    safeExtract ([_, value]:_) = parseJSON @LigoRange value
    safeExtract _ = error "number of range elements in array is not even and cannot be grouped"

----------------------------------------------------------------------------
-- Pretty
----------------------------------------------------------------------------

instance Pretty LigoError where
  pp (LigoError _ stage (LigoErrorContent msg loc)) = mconcat
    [ text "Error in ", text $ show stage
    , text "\n\nat: ", fromLigoRange loc
    , text "\n\n" <> pp msg
    ]
    where
      fromLigoRange r@(LigoRange _ _) =
        "[" <> pp (fromMaybe (error "impossible") (mbFromLigoRange r)) <> "]"
      fromLigoRange (Virtual _) = text "virtual"

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Convert ligo error to its corresponding internal representation.
fromLigoErrorToMsg :: LigoError -> Msg
fromLigoErrorToMsg LigoError
  { _leContent = LigoErrorContent
      { _lecMessage = err
      , _lecLocation = fromLigoRangeOrDef -> at
      }
  } = (at, Error (err :: Text) [])

-- | Helper function that converts qualified field to its JSON counterpart.
--
-- >>> prepareField 2 "__llFooBar"
-- "foo_bar"
prepareField :: Int -> String -> String
prepareField dropAmount = Prelude.drop (dropAmount + 2) . concatMap process
  where
    process c
      | isUpper c = "_" <> [toLower c]
      | otherwise = [c]

-- | Splits an array onto chunks of n elements, throws error otherwise.
--
-- >>> group 2 [1, 2, 3, 4]
-- [[1,2],[3,4]]
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

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
-- contracts/test.ligo:2:3-5:1
mbFromLigoRange :: LigoRange -> Maybe Range
mbFromLigoRange (Virtual _) = Nothing
mbFromLigoRange
  (LigoRange
    (LigoRangeInner (LigoByte { _lbPosLnum = startLine, _lbPosFname = startFilePath }) startCNum startBol)
    (LigoRangeInner (LigoByte { _lbPosLnum = endLine, _lbPosFname = endFilePath }) endCNum endBol)
  )
  | startFilePath /= endFilePath = error "start file of a range does not equal to it's end file"
  | otherwise = Just $ Range
      { rStart = (startLine, abs (startCNum - startBol), 0)
      , rFinish = (endLine, abs (endCNum - endBol), 0)
      , rFile = startFilePath
      }

fromLigoRangeOrDef :: LigoRange -> Range
fromLigoRangeOrDef = fromMaybe (point (-1) (-1)) . mbFromLigoRange

-- Uncomment when needed
-- -- | Converts ligo scope to our internal one.
-- -- TODO: convert `LigoTypeFull` to `LIGO ()`
-- toScopedDecl :: LigoDefinitionScope -> ScopedDecl
-- toScopedDecl
--   LigoDefinitionScope
--     { _ldsName = _sdName
--     , _ldsRange = (fromMaybe (error "no origin range") . mbFromLigoRange -> _sdOrigin)
--     , _ldsBodyRange = (mbFromLigoRange -> _sdBody)
--     } =
--     ScopedDecl
--       { _sdName
--       , _sdOrigin
--       , _sdBody
--       , _sdType = Nothing
--       , _sdRefs = []
--       , _sdDoc = []
--       , _sdParams = Nothing -- TODO LIGO-90
--       }
