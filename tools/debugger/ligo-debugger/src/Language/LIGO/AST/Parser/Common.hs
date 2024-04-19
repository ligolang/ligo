module Language.LIGO.AST.Parser.Common
  ( Reg (..)
  , Wrap (..)
  , WrappedLexeme
  , WrappedTupleLexeme
  , Tuple1 (..)
  , replaceTextualNumbers
  , unpackReg
  , unpackWrap
  , makeWrappedLexeme
  , escapeText
  ) where

import Control.MessagePack (withMsgArray, withMsgMap, (.:))
import Control.Monad.Validate (MonadValidate (..))
import Data.MessagePack (MessagePack, Object (..), decodeError)
import Data.MessagePack.Types (MessagePack (..))
import Data.Vector qualified as V
import Debug qualified
import Text.Interpolation.Nyan hiding (rmode')
import Util

import Duplo (ElemIndex, fastMake)

import Language.LIGO.AST.Skeleton (Info, LIGO)
import Language.LIGO.AST.Skeleton qualified as AST
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Range

-- | The type @Reg a@ enables the concept of some value of type @a@ to
-- be related to a region in a source file.
data Reg a = Reg
  { rRegion :: LigoFileRange
    -- ^ A region of the value.
  , rValue :: a
    -- ^ A value itself.
  }
  deriving stock (Show, Generic, Functor)
  deriving anyclass (NFData)

-- | Wrapping tokens with some metadata.
data Wrap a = Wrap
  { wPayload :: a
    -- ^ A value itself.
  , wRegion :: LigoFileRange
    -- ^ A region of the value.
  }
  deriving stock (Show, Generic, Functor)
  deriving anyclass (NFData)

-- | Token with its metadata.
type WrappedLexeme = Wrap Text

-- | An auxiliary alias for literal tokens.
type WrappedTupleLexeme = Wrap (Tuple1 Text)

instance {-# OVERLAPPABLE #-} (MessagePack a) => MessagePack (Reg a) where
  fromObjectWith _ = withMsgMap "Reg" \o -> do
    rRegion <- (replaceTextualNumbers o) .: "region"
    rValue <- o .: "value"
    pure Reg{..}

instance {-# OVERLAPPING #-} MessagePack (Reg ()) where
  fromObjectWith _ = withMsgMap "Reg" \o -> do
    rRegion <- (replaceTextualNumbers o) .: "region"
    let rValue = ()
    pure Reg{..}

instance (MessagePack a) => MessagePack (Wrap a) where
  fromObjectWith _ = withMsgMap "Wrap" \o -> do
    wPayload <- o .: "payload"
    wRegion <- (replaceTextualNumbers o) .: "region"
    pure Wrap{..}

-- | Some fields are ignored in MessagePack objects for
-- CST nodes. For example, in the LIGO compiler we have:
--
-- @
--  D_attr of (attr * declaration)
-- @
--
-- Field @attr@ doesn't appear in MessagePack, but node @declaration@
-- appears in list of size 1.
--
-- This type is used to mark such fields.
newtype Tuple1 a = Tuple1 { unTuple1 :: a }
  deriving stock (Show, Generic, Functor)
  deriving anyclass (NFData)

instance MessagePack a => MessagePack (Tuple1 a) where
  fromObjectWith cfg = withMsgArray "Tuple1" \arr -> do
    case V.toList arr of
      [el] -> Tuple1 <$> fromObjectWith cfg el
      _ -> refute $ decodeError [int||Unexpected #{V.length arr}|]

-- | Replaces all textual numbers with @ObjectWord@.
replaceTextualNumbers :: Map Object Object -> Map Object Object
replaceTextualNumbers = fmap replaceInObj
  where
    replaceInObj :: Object -> Object
    replaceInObj = \case
      str@(ObjectStr (toString -> val)) -> maybe str ObjectWord (readMaybe val)
      ObjectArray arr -> ObjectArray (replaceInObj <$> arr)
      ObjectMap mp -> ObjectMap (second replaceInObj <$> mp)
      other -> other

-- | A convenient view pattern for @Reg@s.
unpackReg :: Reg a -> (Range, a)
unpackReg Reg{..} = (fromLigoRangeOrDef $ LRFile rRegion, rValue)

-- | A convenient view pattern for @Wrap@s.
unpackWrap :: Wrap a -> (Range, a)
unpackWrap Wrap{..} = (fromLigoRangeOrDef $ LRFile wRegion, wPayload)

-- | Wraps @WrappedLexeme@ into some AST node.
makeWrappedLexeme
  :: KnownNat (ElemIndex f AST.RawLigoList)
  => (Text -> f (LIGO Info)) -- ^ node constructor
  -> WrappedLexeme
  -> LIGO Info
makeWrappedLexeme ctor (unpackWrap -> (r, txt)) = fastMake r (ctor txt)

-- | Escapes special characters in text.
-- For example, the text:
--
-- @
--  aba
--  caba
-- @
-- would be escaped into @"aba\ncaba"@.
escapeText :: Text -> Text
escapeText = Debug.show . toString
