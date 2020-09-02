{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | The definition of type as is represented in ligo JSON output
module Cli.Json
  ( LigoTypeFull (..)
  , LigoTypeContent (..)
  , LigoTypeContentInner (..)
  , LigoRecordField (..)
  , LigoLocation (..)
  )
where

import Data.Aeson
import Data.Char (isUpper, toLower)
import Data.Foldable (asum, toList)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- | Whole ligo type.
-- ```
-- { "t" : LigoTypeFull }
-- ```
data LigoTypeFull = LigoTypeFull
  { -- | Location of the definition.
    ltLocation :: LigoLocation
  , -- | *Some* meta constructors (e.g. `Some`).
    ltTypeMeta :: Value
  , -- | We parse it by a chunks of 2, each odd element of array is a name for
    -- even element which is `LigoTypeContentInner`.
    -- ```
    -- { "type_content": [ <name>, LigoTypeContentInner ] }
    -- ```
    ltTypeContent :: [LigoTypeContent]
  }
  deriving stock (Generic, Show)

-- | A pair in "type_content" array `[name, content]`.
-- ```
-- { "type_content": LigoTypeContent }
-- ```
data LigoTypeContent = LigoTypeContent
  { ltcName :: Text
  , ltcContentInner :: LigoTypeContentInner
  }
  deriving stock (Generic, Show)

-- | Inner object representing type content that depends on `name` in `LigoTypeContent`.
-- ```
-- { "type_content": [ <type>, LigoTypeContentInner ] }
-- ```
data LigoTypeContentInner
  = -- | Type call represented by the list of arguments and its constructor.
    LTCConst
      { ltciArguments :: [Text]
      , ltciTypeConst :: Value
      }
  | -- | Record type.
    LTCRecord (HM.HashMap Text LigoRecordField) -- TODO: continue
  | -- | Arrow type, note that the order of its arguments is reversed.
    LTCArrow -- "type2" -> "type1"
      { ltcType2 :: LigoTypeFull
      , ltcType1 :: LigoTypeFull
      }
  deriving stock (Generic, Show)

-- | Record field type value.
-- ```
-- { "type_content": ["T_record", { "key": LigoRecordField } ] }
-- ```
data LigoRecordField = LigoRecordField
  { -- | Declaration position (don't ask me I too don't know what actual
    -- position is this since from all the example it's somewhat always 0).
    lrfDeclPos :: Int
  , -- | How the value is represented in michelson, currently ignored
    -- during parsing.
    lrfMichelsonAnnotation :: Value
  , -- | The type itself.
    lrfAssociatedType :: LigoTypeFull
  }
  deriving stock (Generic, Show)

-- | Location of type definition.
-- ```
-- { "location": LigoLocation }
-- ```
data LigoLocation
  = Virtual Text
  | LigoLocation
      { llFile :: FilePath
      , llFromRow :: Int
      , llFromCol :: Int
      , llToRow :: Int
      , llToCol :: Int
      }
  deriving stock (Generic, Show)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

-- TODO: We trust ligo compiler output for printing even number
-- of array elements.
instance FromJSON LigoTypeFull where
  parseJSON = withObject "type_full" $ \o -> do
    ltLocation <- o .: "location"
    type_content <- o .: "type_content"
    ltTypeContent <-
      withArray "type_content" (mapM proceed . group 2 . toList) type_content
    ltTypeMeta <- o .: "type_meta"
    return $ LigoTypeFull {..}
    where
      proceed [name, value] = do
        ltcName <- parseJSON @Text name
        ltcContentInner <- parseJSON @LigoTypeContentInner value
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

instance FromJSON LigoLocation where
  parseJSON = withObject "location" $ \o ->
    asum
      [ Virtual <$> o .: "virtual"
      , do
          llFile <- o .: "file"
          llFromRow <- o .: "from_row"
          llFromCol <- o .: "from_col"
          llToRow <- o .: "to_row"
          llToCol <- o .: "to_col"
          return $ LigoLocation {..}
      ]

instance ToJSON LigoLocation where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Helper function that converts qualified field to its JSON counterpart.
--
-- >>> prepareField 2 "llFooBar"
-- "foo_bar"
prepareField :: Int -> String -> String
prepareField dropAmount = Prelude.drop (dropAmount + 1) . concatMap process
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
