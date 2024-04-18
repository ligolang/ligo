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

-- | A convenient synonym that could be used in contraints.
type MsgParser m = MonadValidate DecodeError m

-----------------
-- Combinators --
-----------------

-- | @withMsgMap name f value@ applies @f@ to the @Map@ when @value@
-- is an @ObjectMap@ and fails otherwise.
withMsgMap
  :: (MsgParser m)
  => String
  -> (Map Object Object -> m a)
  -> Object
  -> m a
withMsgMap name f = \case
  ObjectMap (M.fromList . toList -> mp) -> f mp
  _ -> refute $ "Error in object " <> decodeError name

-- | @withMsgArray name f value@ applies @f@ to the @Array@ when @value@
-- is an @ObjectArray@ and fails otherwise.
withMsgArray
  :: (MsgParser m)
  => String
  -> (Vector Object -> m a)
  -> Object
  -> m a
withMsgArray name f = \case
  ObjectArray arr -> f arr
  _ -> refute $ "Error in array " <> decodeError name

-- | @withMsgText name f value@ applies @f@ to the @String@ when @value@
-- is an @ObjectStr@ and fails otherwise.
withMsgText :: (MsgParser m) => String -> (Text -> m a) -> Object -> m a
withMsgText name f = \case
  ObjectStr str -> f str
  _ -> refute $ "Error in text " <> decodeError name

-- | @withMsgVariant name f value@ applies @f@ to the variant when @value@
-- is an array of length 1 or 2. It's expected that the first element of the array
-- has a @String@ type. If it lacks of the second element then it would be treated
-- as @ObjectNil@.
--
-- Otherwise, it fails.
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

-- | @mp .: key@ picks and decodes a value with string @key@ from map @mp@.
-- Fails if @key@ is missing.
(.:) :: (MsgParser m, MessagePack a) => Map Object Object -> Text -> m a
mp .: name = mp .:! name
  >>=
    maybe
      do refute $ decodeError [int||Can't find a value with key #{name}|]
      pure

-- | Like @(.:)@ but returns @Nothing@ if key is missing.
(.:!) :: (MsgParser m, MessagePack a) => Map Object Object -> Text -> m (Maybe a)
mp .:! name = do
  case mp M.!? ObjectStr name of
    Nothing -> pure Nothing
    Just obj -> Just <$> fromObjectWith defaultConfig obj

-- | Like @(.:)@ but returns @Nothing@ if key is missing or value is @ObjectNil@.
(.:?) :: (MsgParser m, MessagePack a) => Map Object Object -> Text -> m (Maybe a)
mp .:? name = do
  case mp M.!? ObjectStr name of
    Nothing -> pure Nothing
    Just ObjectNil -> pure Nothing
    Just obj -> Just <$> fromObjectWith defaultConfig obj

-- | @ma .!= a@ returns @ma@ if its underlying is @Just@.
-- Otherwise, returns @a@.
(.!=) :: (MsgParser m) => m (Maybe a) -> a -> m a
ma .!= a = ma >>= \case
  Just val -> pure val
  Nothing -> pure a

-- | Runs all parsers from left to right till one of them satisfies.
-- Fails if none of them are satisfied.
asumMsg :: forall m a. (MsgParser m) => [Validate DecodeError a] -> m a
asumMsg parsers = parsers
  & foldl go (Left "Can't apply any parser")
  & either refute pure
  where
    go :: Either DecodeError a -> Validate DecodeError a -> Either DecodeError a
    go (Right res) _     = Right res
    go (Left  _  ) other = runValidate other

-- | Checks a boolean condition. Fails if it @False@.
guardMsg :: Bool -> Validate DecodeError ()
guardMsg cond
  | cond = pass
  | otherwise = refute "Condition is not satisfied"

-- | @wrapMonadFail err ma@ returns a value if underlying value of @ma@ is @Just@.
-- Fails with @err@ otherwise.
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

-- | A convenient function that transfroms JSON value to MessagePack object.
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

-- | Parses a JSON file, converts its contents to MessagePack, and runs
-- MessagePack parsers on the resulting object.
_parseAsMsgPack :: (MessagePack a) => FilePath -> IO a
_parseAsMsgPack path = do
  val <- fromJust <$> decodeFileStrict path
  fromObject (_jsonToMsgPack val)
