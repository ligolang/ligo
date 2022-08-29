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
  (DAPOutputMessage (..), DAPSpecificResponse (..), HasSpecificMessages (LanguageServerStateExt),
  RIO, RioContext (..))
import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Michelson.Parser qualified as P
import Morley.Michelson.TypeCheck (typeVerifyTopLevelType)
import Morley.Michelson.Typed (Contract' (..), SomeContract (..))
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import Text.Interpolation.Nyan

import Cli (HasLigoClient)
import Language.LIGO.Debugger.CLI.Call

-- TODO: move this instance to morley-debugger
instance FromBuilder DAP.Message where
  fromBuilder txt = DAP.defaultMessage
    { DAP.formatMessage = fromBuilder txt
    }

-- | LIGO-debugger-specific state that we initialize before debugger session
-- creation.
data LigoLanguageServerState = LigoLanguageServerState
  { lsProgram :: Maybe FilePath
  , lsContract :: Maybe SomeContract
  , lsEntrypoint :: Maybe String  -- ^ @main@ method to use
  , lsAllLocs :: Maybe (Set SourceLocation)
  , lsBinaryPath :: Maybe FilePath
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
  :: (SingI t, HasLigoClient m)
  => FilePath
  -> Text
  -> Text
  -> Text
  -> m (Either Text (T.Value t))
parseValue ctxContractPath category val valueType = do
  let src = P.MSName category
  eUvalue <- case valueType of
    "LIGO" ->
      compileLigoExpression src ctxContractPath val >>= \case
        Right x -> pure $ Right x
        Left err -> pure $ Left [int||
          Error parsing #{category}:

          #{err}
          |]
    "Michelson" -> pure $
      P.parseExpandValue src val
        & first (pretty . MD.prettyFirstError)
    _ -> pure $ Left [int||
        Expected "LIGO" or "Michelson" in field "valueType" \
        but got #{valueType}
      |]
  case eUvalue of
    Right uvalue -> pure $
      typeVerifyTopLevelType mempty uvalue
        & typeCheckingForDebugger
        & first (\msg -> [int||Typechecking as #{category} failed: #{msg}|])
    Left err -> pure $ Left err

getServerState :: HasCallStack => RIO ext (LanguageServerStateExt ext)
getServerState = asks _rcLSState >>= readTVarIO >>= \case
  Nothing -> error "Language server state is not initialized"
  Just s -> pure s

getProgram
  :: (HasCallStack, LanguageServerStateExt ext ~ LigoLanguageServerState)
  => RIO ext FilePath
getProgram = fromMaybe (error "Program is not initialized") . lsProgram <$> getServerState

getContract
  :: (HasCallStack, LanguageServerStateExt ext ~ LigoLanguageServerState)
  => RIO ext SomeContract
getContract = fromMaybe (error "Contract is not initialized") . lsContract <$> getServerState

getAllLocs
  :: (HasCallStack, LanguageServerStateExt ext ~ LigoLanguageServerState)
  => RIO ext (Set SourceLocation)
getAllLocs = fromMaybe (error "All locs are not initialized") . lsAllLocs <$> getServerState
