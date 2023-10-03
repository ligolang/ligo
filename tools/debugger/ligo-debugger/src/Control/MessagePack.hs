{-# OPTIONS_GHC -Wno-orphans #-}
module Control.MessagePack
  ( withMsgMap
  , withMsgArray
  , withMsgText
  , withMsgVariant
  , (.:)
  , (.:!)
  , (.:?)
  , (.!=)
  , asumMsg
  , guardMsg
  , wrapMonadFail

  -- * Debug
  , _jsonToMsgPack
  , _parseAsMsgPack
  ) where

import Control.Monad.Validate (MonadValidate (refute), Validate, runValidate)
import Data.Aeson (Value (..), decodeFileStrict)
import Data.Aeson.Key qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.MessagePack
  (DecodeError, MessagePack, Object (..), decodeError, defaultConfig, fromObject, fromObjectWith,
  toObject)
import Data.Scientific (toBoundedInteger)
import Data.Vector qualified as V
import Text.Interpolation.Nyan

type MsgParser m = MonadValidate DecodeError m

-----------------
-- Combinators --
-----------------

withMsgMap
  :: (MsgParser m)
  => String
  -> (Map Object Object -> m a)
  -> Object
  -> m a
withMsgMap name f = \case
  ObjectMap (M.fromList . toList -> mp) -> f mp
  _ -> refute $ "Error in object " <> decodeError name

withMsgArray
  :: (MsgParser m)
  => String
  -> (Vector Object -> m a)
  -> Object
  -> m a
withMsgArray name f = \case
  ObjectArray arr -> f arr
  _ -> refute $ "Error in array " <> decodeError name

withMsgText :: (MsgParser m) => String -> (Text -> m a) -> Object -> m a
withMsgText name f = \case
  ObjectStr str -> f str
  _ -> refute $ "Error in text " <> decodeError name

withMsgVariant
  :: (MsgParser m)
  => String
  -> ((Text, Object) -> m a)
  -> Object
  -> m a
withMsgVariant name f = withMsgArray name \arr -> do
  case V.length arr of
    1 -> do
      -- MonadValidate doesn't have a constraint on MonadError.
      -- So, we need to explicitly pattern match here.
      case V.unsafeIndex arr 0 of
        ObjectStr ctor -> f (ctor, ObjectNil)
        _ -> refute $ "Expected constructor in first place " <> decodeError name
    2 -> do
      case V.unsafeIndex arr 0 of
        ObjectStr ctor -> do
          let val = V.unsafeIndex arr 1
          f (ctor, val)
        _ -> refute $ "Expected constructor in first place " <> decodeError name
    n -> refute $ decodeError [int||Too many: #{n}|]

(.:) :: (MsgParser m, MessagePack a) => Map Object Object -> Text -> m a
mp .: name = mp .:! name
  >>=
    maybe
      do refute $ decodeError [int||Can't find a value with key #{name}|]
      pure

(.:!) :: (MsgParser m, MessagePack a) => Map Object Object -> Text -> m (Maybe a)
mp .:! name = do
  case mp M.!? ObjectStr name of
    Nothing -> pure Nothing
    Just obj -> Just <$> fromObjectWith defaultConfig obj

(.:?) :: (MsgParser m, MessagePack a) => Map Object Object -> Text -> m (Maybe a)
mp .:? name = do
  case mp M.!? ObjectStr name of
    Nothing -> pure Nothing
    Just ObjectNil -> pure Nothing
    Just obj -> Just <$> fromObjectWith defaultConfig obj

(.!=) :: (MsgParser m) => m (Maybe a) -> a -> m a
ma .!= a = ma >>= \case
  Just val -> pure val
  Nothing -> pure a

asumMsg :: forall m a. (MsgParser m) => [Validate DecodeError a] -> m a
asumMsg parsers = parsers
  & foldl go (Left "Can't apply any parser")
  & either refute pure
  where
    go :: Either DecodeError a -> Validate DecodeError a -> Either DecodeError a
    go (Right res) _     = Right res
    go (Left  _  ) other = runValidate other

guardMsg :: Bool -> Validate DecodeError ()
guardMsg cond
  | cond = pass
  | otherwise = refute "Condition is not satisfied"

wrapMonadFail :: (MsgParser m) => DecodeError -> Maybe a -> m a
wrapMonadFail err = maybe (refute err) pure

-------------
-- Orphans --
-------------

instance (MessagePack a) => MessagePack (NonEmpty a) where
  toObject cfg lst = toObject cfg (toList lst)
  fromObjectWith cfg obj = do
    lst <- fromObjectWith cfg obj
    maybe (refute "List is empty") pure (nonEmpty lst)

instance (MessagePack a) => MessagePack (Maybe a) where
  toObject cfg mVal = maybe ObjectNil (toObject cfg) mVal
  fromObjectWith cfg = \case
    ObjectNil -> pure Nothing
    other -> Just <$> fromObjectWith cfg other

-----------
-- Debug --
-----------

_jsonToMsgPack :: Value -> Object
_jsonToMsgPack = \case
  Object obj -> obj
    & A.toList
    <&> do \(key, val) -> (ObjectStr $ A.toText key, _jsonToMsgPack val)
    & ObjectMap . V.fromList
  Array vec -> ObjectArray $ _jsonToMsgPack <$> vec
  String str -> ObjectStr str
  Number n -> ObjectInt (fromJust $ toBoundedInteger n)
  Bool b -> ObjectBool b
  Null -> ObjectNil

_parseAsMsgPack :: (MessagePack a) => FilePath -> IO a
_parseAsMsgPack path = do
  val <- fromJust <$> decodeFileStrict path
  fromObject (_jsonToMsgPack val)
