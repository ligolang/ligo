{-# LANGUAGE DeriveGeneric, RecordWildCards #-}

-- | The definition of type as is represented in ligo JSON output
module Cli.Json
  ( LigoScope (..)
  , LigoDefinitions (..)
  , LigoDefinitionsInner (..)
  , LigoDefinitionScope (..)
  , LigoTypeFull (..)
  , LigoTypeContent (..)
  , LigoTypeContentInner (..)
  , LigoRecordField (..)
  , LigoRange (..)
  , convertLigoRange
  , toScopedDecl
  , prepareField
  )
where

import Data.Aeson
import Data.Char (isUpper, toLower)
import Data.Foldable (asum, toList)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics

import AST.Scope.Common
import Range

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

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
  { _lsRange :: LigoRange
  , _lsExpressionEnvironment :: [Text]
  , _lsTypeEnvironment :: Value -- TODO: currently ligo always outputs an empty list
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

-- | Location of type definition.
-- ```
-- { "location": LigoRange }
-- ```
data LigoRange
  = Virtual Text
  | LigoRange
      { _lrFile :: FilePath
      , _lrFromRow :: Int
      , _lrFromCol :: Int
      , _lrToRow :: Int
      , _lrToCol :: Int
      }
  deriving stock (Generic, Show)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance FromJSON LigoDefinitions where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance ToJSON LigoDefinitions where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON LigoDefinitionsInner where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON LigoDefinitionsInner where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoScope where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance ToJSON LigoScope where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON LigoDefinitionScope where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON LigoDefinitionScope where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

-- TODO: We trust ligo compiler output for printing even number
-- of array elements.
instance FromJSON LigoTypeFull where
  parseJSON = withObject "type_full" $ \o -> do
    _ltLocation <- o .: "location"
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
  parseJSON = withObject "location" $ \o ->
    asum
      [ Virtual <$> o .: "virtual"
      , do
          _lrFile <- o .: "file"
          _lrFromRow <- o .: "from_row"
          _lrFromCol <- o .: "from_col"
          _lrToRow <- o .: "to_row"
          _lrToCol <- o .: "to_col"
          return $ LigoRange {..}
      ]

instance ToJSON LigoRange where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

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
-- >>> convertLigoRange (LigoRange "test.ligo" 1 2 3 4)
-- [32m[test.ligo:1:2-3:4][0m
convertLigoRange :: LigoRange -> Maybe Range
convertLigoRange (Virtual _) = Nothing
convertLigoRange LigoRange {..} =
  Just
    Range
      { rStart = (_lrFromRow, _lrFromCol, 0)
      , rFinish = (_lrToRow, _lrToCol, 0)
      , rFile = _lrFile
      }

-- | Converts ligo scope to our internal one.
-- TODO: convert `LigoTypeFull` to `LIGO ()`
toScopedDecl :: LigoDefinitionScope -> ScopedDecl
toScopedDecl
  LigoDefinitionScope
    { _ldsName = _sdName
    , _ldsRange = (fromMaybe (error "no origin range") . convertLigoRange -> _sdOrigin)
    , _ldsBodyRange = (convertLigoRange -> _sdBody)
    } =
    ScopedDecl
      { _sdName
      , _sdOrigin
      , _sdBody
      , _sdType = Nothing
      , _sdRefs = []
      , _sdDoc = []
      }
