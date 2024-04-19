{-# LANGUAGE InstanceSigs #-}
module Language.LIGO.AST.Parser
  ( scanContracts
  , ASTs (..)
  ) where

import Control.MessagePack (withMsgArray, withMsgMap, (.:))
import Control.Monad.Validate (MonadValidate)
import Data.MessagePack (Config, DecodeError, MessagePack, Object (ObjectNil))
import Data.MessagePack.Types (MessagePack (..))
import System.FilePath ((</>))
import UnliftIO.Directory (doesDirectoryExist, listDirectory)

import Language.LIGO.AST.Parser.CameLigoCST qualified as CameLIGO
import Language.LIGO.AST.Parser.JsLigoCST qualified as JsLIGO
import Language.LIGO.AST.Skeleton
import Language.LIGO.Extension

-- | Scan the whole directory for LIGO contracts.
-- This ignores every other file which is not a contract.
scanContracts :: MonadIO m => (FilePath -> Bool) -> FilePath -> m [FilePath]
scanContracts predicate = liftIO . scanContractsImpl []
  where
    scanContractsImpl :: [FilePath] -> FilePath -> IO [FilePath]
    scanContractsImpl seen top
      | predicate top = do
        ds <- listDirectory top
        flip foldMap ds \d -> do
          let p = top </> d
          if predicate p
            then do
              exists <- doesDirectoryExist p
              if
                | exists -> scanContractsImpl seen p
                | isLigoFile p -> pure $ p : seen
                | otherwise -> pure seen
            else pure seen
      | otherwise = pure seen

-- | A type-class that provides a unificator for the CST.
class ToAST (lang :: Lang) where
  -- | Type of the CST.
  type CST lang

  -- | Unificator of the CST.
  toAST :: CST lang -> LIGO Info

instance ToAST 'Caml where
  type CST 'Caml = CameLIGO.CST
  toAST = CameLIGO.toAST

instance ToAST 'Js where
  type CST 'Js = JsLIGO.CST
  toAST = JsLIGO.toAST

-- | An existential type that incapsulates @Lang@ type
-- and evidences that its CST could be parsed from MessagePack
-- and unified.
data SomeLang where
  SomeLang :: (ToAST lang, MessagePack (CST lang)) => Proxy lang -> SomeLang

-- | Lift a value of @Lang@ type to the type of @Lang@ kind.
reifyLang :: Lang -> SomeLang
reifyLang = \case
  Caml -> SomeLang (Proxy @'Caml)
  Js -> SomeLang (Proxy @'Js)

-- | A convenient wrapper that stores a list of parsed and unified CSTs.
newtype ASTs = ASTs { unASTs :: [SomeLIGO Info] }

instance MessagePack ASTs where
  toObject _ = const ObjectNil

  fromObjectWith :: forall m. (MonadValidate DecodeError m) => Config -> Object -> m ASTs
  fromObjectWith _ = withMsgArray "ASTs" \(toList -> arr) ->
    ASTs <$> traverse objectToAST arr
    where
      objectToAST :: Object -> m (SomeLIGO Info)
      objectToAST = withMsgMap "AST" \o -> do
        lang <- o .: "lang"
        SomeLang (Proxy :: Proxy lang) <- pure $ reifyLang lang
        cst <- o .: "cst"
        pure $ SomeLIGO lang (toAST @lang cst)
