{-# LANGUAGE DeriveDataTypeable, StandaloneKindSignatures, UndecidableInstances #-}

-- | All the types needed for cli to work.
module Language.LIGO.Debugger.CLI.Helpers
  ( LigoClientEnv (..)
  , HasLigoClient(..)
  , NameType (..)
  , SNameType (..)
  , UniqueSym0
  , ConciseSym0
  , Name (..)
  , LigoJSON (..)
  , ligoBinaryPath
  , compareUniqueNames
  , toSnakeCase
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson
  (FromJSON (parseJSON), GFromJSON, Options (fieldLabelModifier), Zero, defaultOptions,
  genericParseJSON)
import Data.Char (isDigit, isUpper, toLower)
import Data.Data (Data)
import Data.Default (Default (..))
import Data.List qualified as L
import Data.Singletons.TH
import Data.Text qualified as T
import Fmt (Buildable (build), Builder)
import Fmt.Internal.Core (FromBuilder)
import GHC.Generics (Generic (Rep))
import GHC.TypeLits (Nat)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)
import Text.Interpolation.Nyan
import UnliftIO.Exception as UnliftIO (catch, throwIO)

import Morley.Util.TypeLits (ErrorMessage (Text), TypeError)

-- | Environment passed throughout the ligo interaction
newtype LigoClientEnv = LigoClientEnv
  { -- | LIGO binary path
    _lceClientPath :: FilePath
  }

-- | Attempts to get the environment variable 'LIGO_BINARY_PATH'. If such
-- variable is not present, defaults to "ligo", assuming it is in PATH.
ligoBinaryPath :: IO FilePath
ligoBinaryPath = getEnv "LIGO_BINARY_PATH" `UnliftIO.catch` \e ->
  if isDoesNotExistError e
  then pure "ligo"
  else throwIO e

instance Default LigoClientEnv where
  def = LigoClientEnv{_lceClientPath = "ligo"}

class MonadUnliftIO m => HasLigoClient m where
  getLigoClientEnv :: m LigoClientEnv

-- Mostly for debugging purposes
instance HasLigoClient IO where
  getLigoClientEnv = do
    _lceClientPath <- ligoBinaryPath
    pure def
      { _lceClientPath
      }

-- | Type marker, which stores information about
-- hashes presence in variable names.
data NameType
  = Unique
    -- ^ Variable name __may have (but not require)__ hashes.
    -- Example: @var#123@.
    --
    -- Up to our understanding, the rules are the following:
    --
    -- * Local variables __have__ hashes.
    -- * Top-level declarations within the module being run __don't have__
    -- hashes.
    -- * However, top-level declarations in the imported files __have__
    -- hashes.
    -- * Similarly, top-level declarations coming from @module@s __have__
    -- hashes.
    --
    -- Also, the main rule:
    -- * Two different variables will have different unique names.
    -- * The opposite is not true in some edge cases.
    --
    --   For instance, @advanced-curry.mligo@ from our tests has a nuance:
    --   in @apply@ function, variable @f@ will have numerous different
    --   unique names.
    --
    -- So whether can you rely on the names when distinguishing variables or not
    -- - depends on your use case and on your definition of "same" term
    -- on variables.
    --
    -- Not only the entire variable name is globally unique across the contract;
    -- the numeric identifier after the hash is globally unique too.
  | Concise
    -- ^ Variable name definitely doesn't have hashes.
    --
    -- For user-defined variables (and not LIGO-internal ones), concise
    -- representation will exactly match the name of the variable in the code.

genSingletons [''NameType]

newtype Name (u :: NameType) = Name Text
  deriving stock (Data)
  deriving newtype (Show, NFData, FromJSON)

deriving newtype instance FromBuilder (Name 'Concise)
deriving newtype instance IsString (Name 'Concise)
deriving newtype instance Eq (Name 'Concise)

instance (TypeError ('Text "You can't compare unique names directly. Please use \"compareUniqueNames\" for it")
         ) => Eq (Name 'Unique) where
  (==) = error "impossible"

compareUniqueNames :: Name 'Unique -> Name 'Unique -> Bool
compareUniqueNames (Name lhs) (Name rhs) = lhs == rhs

instance (SingI u) => Buildable (Name u) where
  build (Name varName) = case sing @u of
    SConcise -> build varName
    SUnique -> buildUnique varName
    where
      -- Here we want to pretty-print monomorphed variables.
      -- They have format like "poly_#SomeModule#NestedModule#foo_name_42"
      -- and we want to pretty-print them like "SomeModule.NestedModule.foo_name$42".
      buildUnique :: Text -> Builder
      buildUnique (T.stripPrefix "poly_" -> Just name)
        | not (T.null index) && T.all isDigit index && T.any (== '_') functionWithIndex =
          [int||#{moduleName}#{functionName}$#{index}|]
        where
          -- This should be non-empty
          splitted = T.split (== '#') $ T.dropWhile (== '#') name
          moduleParts = L.init splitted
          functionWithIndex = L.last splitted
          index = T.takeWhileEnd (/= '_') functionWithIndex
          moduleName
            | null moduleParts = ""
            | otherwise = T.intercalate "." moduleParts <> "."
          -- This @breakOnEnd@ shouldn't crash because of
          -- "T.any (== '_') functionWithIndex" check in guard above.
          (functionName, _) = first (T.dropEnd 1) $ T.breakOnEnd "_" functionWithIndex

      buildUnique name =
        -- Some variables may look like "varName#123". We want to strip that identifier.
        if T.all isDigit suffix
        then build $ T.dropEnd 1 $ T.dropWhileEnd (/= '#') name
        else build name
        where
          suffix = T.takeWhileEnd (/= '#') name

newtype LigoJSON (n :: Nat) a = LigoJSON a

-- | Converts string to snake_case
toSnakeCase :: String -> String
toSnakeCase = foldr helper []
  where
    helper c acc
      | isUpper c = '_' : toLower c : acc
      | otherwise = c : acc

instance forall n a. (Generic a, GFromJSON Zero (Rep a), KnownNat n) => FromJSON (LigoJSON n a) where
  parseJSON = fmap LigoJSON . genericParseJSON defaultOptions
    { fieldLabelModifier =
      genericDrop (natVal (Proxy @n) + 2)
      . toSnakeCase
    }
