{-# LANGUAGE RecordWildCards #-}

module AST.Capabilities.SignatureHelp
  ( LSP.ParameterInformation (..)
  , LSP.SignatureInformation (..)
  , findSignatures
  , getSignatureHelp
  , makeSignatureLabel
  , runSigHelpM
  ) where

import qualified Language.Haskell.LSP.Types as LSP
  (List (..), ParameterInformation (..), SignatureHelp (..), SignatureInformation (..))

import Control.Monad (void)
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString (ByteString)
import Data.Foldable (asum)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text (intercalate)
import Duplo.Lattice (leq)
import Duplo.Pretty (fsep, pp, ppToText)
import Duplo.Tree (match, spineTo)

import AST.Capabilities.Find (CanSearch)
import AST.Scope (ScopedDecl (..))
import AST.Scope.Common (Category (Variable), Parameter (..), lookupEnv, ofCategory)
import AST.Skeleton (Expr (Apply), LIGO)
import Product (Product, getElem)
import Range (Range, cutOut)

-- | Embed 'Maybe' actions into 'MaybeT m'
liftMaybe :: Applicative m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . pure

-- | Find a 'ScopedDecl' of a function that is applied at the given position.
findNestingFunction
  :: CanSearch xs => Range -> MaybeT (SigHelpM xs) ScopedDecl
findNestingFunction position = do
  tree <- asks sheTree
  liftMaybe $ do
    let covers = spineTo (leq position . getElem) tree
    (callInfo, fName) <- asum (map extractFunctionCall covers)
    let termEnv = filter (ofCategory Variable) (getElem callInfo)
    lookupEnv fName termEnv

-- | If the given tree is a function application, extract it's information
-- characteristics and the function's name.
extractFunctionCall :: LIGO xs -> Maybe (Product xs, Text)
extractFunctionCall tree = do
  (i, Apply name _) <- match tree
  pure (i, ppToText (void name))

-- | Find all function signatures (one in this implementation) that could be
-- applied at the given position. A function signature includes its label which
-- is what will represent the function, its documentation comments and its
-- parameters (if present, they must be a part of the label). Parameters might
-- be highlighted by the editor.
findSignatures :: CanSearch xs => Range -> SigHelpM xs [LSP.SignatureInformation]
findSignatures position = maybeToList <$> runMaybeT do
  ScopedDecl{..} <- findNestingFunction position
  params <- liftMaybe _sdParams
  contents <- asks sheContents
  let paramOrigins = map parOrigin params
      label = makeSignatureLabel _sdName paramLabels
      paramLabels = map (`cutOut` contents) paramOrigins
  pure LSP.SignatureInformation
    { _label = label
    , _documentation = Just (ppToText (fsep (map pp _sdDoc)))
    , _parameters = Just (map toLspParameter paramLabels)
    }

-- | Make a function signature label by a function name and its parameters.
makeSignatureLabel :: Text -> [Text] -> Text
makeSignatureLabel name params
  = name <> " (" <> Text.intercalate ", "  params <> ")"

-- | Make a 'ParameterInformation' by a parameter's name. For now, we don't
-- support parameter docs.
toLspParameter :: Text -> LSP.ParameterInformation
toLspParameter name = LSP.ParameterInformation name Nothing

-- | Find all function signatures at the given position and wrap them up in a
-- 'SignatureHelp'.
--
-- See 'findSignatures'.
getSignatureHelp :: CanSearch xs => Range -> SigHelpM xs LSP.SignatureHelp
getSignatureHelp position = do
  signatures <- findSignatures position
  pure LSP.SignatureHelp
    { _signatures = LSP.List signatures
    , _activeSignature = Just 0
    , _activeParameter = Nothing
    }

-- | A context for signature help implementation which contains a tree and
-- contents of the contract we are operating upon.
data SigHelpEnv xs = SigHelpEnv
  { sheTree :: LIGO xs
  , sheContents :: ByteString
  }

type SigHelpM xs = Reader (SigHelpEnv xs)

-- | Perform 'SigHelpM' action with a tree and contents of the contract the
-- action operates upon.
runSigHelpM :: LIGO xs -> ByteString -> SigHelpM xs a -> a
runSigHelpM tree contents = flip runReader (SigHelpEnv tree contents)
