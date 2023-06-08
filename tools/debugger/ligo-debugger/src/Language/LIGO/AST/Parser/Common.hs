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

data Reg a = Reg
  { rRegion :: LigoFileRange
  , rValue :: a
  }
  deriving stock (Show, Generic, Functor)
  deriving anyclass (NFData)

data Wrap a = Wrap
  { wPayload :: a
  , wRegion :: LigoFileRange
  }
  deriving stock (Show, Generic, Functor)
  deriving anyclass (NFData)

type WrappedLexeme = Wrap Text
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

newtype Tuple1 a = Tuple1 { unTuple1 :: a }
  deriving stock (Show, Generic, Functor)
  deriving anyclass (NFData)

instance MessagePack a => MessagePack (Tuple1 a) where
  fromObjectWith cfg = withMsgArray "Tuple1" \arr -> do
    case V.toList arr of
      [el] -> Tuple1 <$> fromObjectWith cfg el
      _ -> refute $ decodeError [int||Unexpected #{V.length arr}|]

replaceTextualNumbers :: Map Object Object -> Map Object Object
replaceTextualNumbers = fmap replaceInObj
  where
    replaceInObj :: Object -> Object
    replaceInObj = \case
      str@(ObjectStr (toString -> val)) -> maybe str ObjectWord (readMaybe val)
      ObjectArray arr -> ObjectArray (replaceInObj <$> arr)
      ObjectMap mp -> ObjectMap (second replaceInObj <$> mp)
      other -> other

unpackReg :: Reg a -> (Range, a)
unpackReg Reg{..} = (fromLigoRangeOrDef $ LRFile rRegion, rValue)

unpackWrap :: Wrap a -> (Range, a)
unpackWrap Wrap{..} = (fromLigoRangeOrDef $ LRFile wRegion, wPayload)

makeWrappedLexeme
  :: KnownNat (ElemIndex f AST.RawLigoList)
  => (Text -> f (LIGO Info))
  -> WrappedLexeme
  -> LIGO Info
makeWrappedLexeme ctor (unpackWrap -> (r, txt)) = fastMake r (ctor txt)

escapeText :: Text -> Text
escapeText = Debug.show . toString
