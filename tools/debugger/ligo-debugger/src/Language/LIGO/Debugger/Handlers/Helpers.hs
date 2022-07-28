{-# OPTIONS_GHC -Wno-orphans #-}

-- | Helpers for implementing DAP handlers.
module Language.LIGO.Debugger.Handlers.Helpers
  ( module Language.LIGO.Debugger.Handlers.Helpers
  ) where

import Control.Concurrent.STM (writeTChan)
import Control.Monad.Except (MonadError, liftEither, throwError)
import Data.Char qualified as C
import Data.Singletons (SingI)
import Fmt (Buildable (..), Builder, pretty)
import Fmt.Internal.Core (FromBuilder (..))
import Morley.Debugger.Core.Common (typeCheckingForDebugger)
import Morley.Debugger.Core.Navigate (SourceLocation)
import Morley.Debugger.DAP.LanguageServer qualified as MD
import Morley.Debugger.DAP.Types
  (DAPOutputMessage (..), DAPSpecificResponse (..), RIO, RioContext (..))
import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Michelson.Parser qualified as P
import Morley.Michelson.TypeCheck (typeVerifyTopLevelType)
import Morley.Michelson.Typed (Contract' (..), SomeContract (..))
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import Text.Interpolation.Nyan

import Language.LIGO.Debugger.CLI.Call

-- TODO: move this instance to morley-debugger
instance FromBuilder DAP.Message where
  fromBuilder txt = DAP.defaultMessage
    { DAP.formatMessage = fromBuilder txt
    }

-- | LIGO-debugger-specific state that we initialize before debugger session
-- creation.
data LigoLanguageServerState = LigoLanguageServerState
  { lsProgram :: FilePath
  , lsContract :: SomeContract
  , lsEntrypoint :: String  -- ^ @main@ method to use
  , lsAllLocs :: Set SourceLocation
  }

instance Buildable LigoLanguageServerState where
  build LigoLanguageServerState{..} = [int||
    Debugging program: #{lsProgram}
    |]

throwDAPError :: (MonadError DAP.Message m) => Builder -> m a
throwDAPError = throwError . fromBuilder

writeResponse :: DAPSpecificResponse ext -> RIO ext ()
writeResponse msg = do
  ch <- asks _rcOutputChannel
  atomically $ writeTChan ch (DAPResponse msg)

withMichelsonEntrypoint
  :: (MonadError e m)
  => T.Contract param st
  -> Maybe String
  -> (Text -> e)
  -> (forall arg. SingI arg => T.Notes arg -> T.EntrypointCallT param arg -> m a)
  -> m a
withMichelsonEntrypoint contract@T.Contract{} mEntrypoint liftErr cont = do
  let noParseEntrypointErr = [int|m|Could not parse entrypoint: #{id}|]
  michelsonEntrypoint <- case mEntrypoint of
    Nothing -> pure U.DefEpName
    -- extension may return default entrypoints as "default"
    Just "default" -> pure U.DefEpName
    Just ep -> U.buildEpName (toText $ firstLetterToLowerCase ep)
      & first (liftErr . noParseEntrypointErr)
      & liftEither

  let noEntrypointErr = [int||Entrypoint `#{michelsonEntrypoint}` not found|]
  T.MkEntrypointCallRes notes call <-
    T.mkEntrypointCall michelsonEntrypoint (cParamNotes contract)
    & maybe (throwError $ liftErr noEntrypointErr) pure

  cont notes call
  where
    -- LIGO has constructors starting from capital letters,
    -- however in Michelson they appear as field annotations starting from
    -- lower-case letter.
    -- This is a detail that we would like to hide from the end user.
    firstLetterToLowerCase = \case
      [] -> []
      c : rest -> C.toLower c : rest

-- | Try our best to parse and typecheck a value of a certain category.
parseValue
  :: (SingI t, MonadIO m, MonadError Text m)
  => FilePath
  -> Text
  -> Text
  -> Text
  -> m (T.Value t)
parseValue ctxContractPath category val valueType = do
  let src = P.MSName category
  uvalue <- case valueType of
    "LIGO" ->
      runExceptT (compileLigoExpression src ctxContractPath val) >>= \case
        Right x -> pure x
        Left err -> throwError [int||
          Error parsing #{category}:

          #{err}
          |]
    "Michelson" ->
      P.parseExpandValue src val
        & either (throwError . pretty . MD.prettyFirstError) pure
    _ -> throwError [int||
        Expected "LIGO" or "Michelson" in field "valueType" \
        but got #{valueType}
      |]

  typeVerifyTopLevelType mempty uvalue
    & typeCheckingForDebugger
    & either (\msg -> throwError [int||Typechecking as #{category} failed: #{msg}|]) pure
