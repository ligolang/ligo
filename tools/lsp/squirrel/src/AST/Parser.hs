module AST.Parser
  ( Source(..)
  , parse
  , parseWithScopes
  , parseWithScopes'
  ) where

import Control.Exception.Safe (try)
import Control.Lens (_1, element, view, (%~), (&), (.~), (<&>), (^.))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List

import qualified AST.Parser.Camligo as CAML
import qualified AST.Parser.Pascaligo as Pascal
import qualified AST.Parser.Reasonligo as Reason
import AST.Scope
import AST.Skeleton

import Duplo (Lattice (leq))

import Cli
  (LigoBinaryCallError (DecodedExpectedClientFailure, LigoErrorNodeParseError), fromLigoErrorToMsg)
import Extension
import ParseTree (Source (..), toParseTree)
import Parser
import Range (point)

parse :: Source -> IO (LIGO Info, [Msg])
parse src = do
  recogniser <- onExt ElimExt
    { eePascal = Pascal.recognise
    , eeCaml   = CAML.recognise
    , eeReason = Reason.recognise
    } (srcPath src)
  toParseTree src
    >>= (runParserM . recogniser)
    <&> (_1 %~ view nestedLIGO)

-- | Parse with arbitrary parser.
parseWithScopes :: forall impl m. HasScopeForest impl m => Source -> m (LIGO Info', [Msg])
parseWithScopes src = do
  recogniser <- liftIO do
    onExt ElimExt
      { eePascal = Pascal.recognise
      , eeCaml   = CAML.recognise
      , eeReason = Reason.recognise
      } (srcPath src)

  (ast, msg) <- liftIO do
    toParseTree src >>= runParserM . recogniser

  ast' <- addLocalScopes @impl src ast
  return (ast' ^. nestedLIGO, msg)

-- | Parse with both compiler and fallback parsers.
parseWithScopes' :: forall m.
  ( HasScopeForest FromCompiler m
  , HasScopeForest Fallback m
  ) => Source -> m (LIGO Info', [Msg])
parseWithScopes' src = do
  recogniser <- liftIO do
    onExt ElimExt
      { eePascal = Pascal.recognise
      , eeCaml   = CAML.recognise
      , eeReason = Reason.recognise
      } (srcPath src)

  (ast, msg) <- liftIO do
    toParseTree src >>= runParserM . recogniser

  ligoAst <- try $ addLocalScopes @FromCompiler src ast

  case ligoAst of
    Right ast' ->
      return (ast' ^. nestedLIGO, msg)
    Left (DecodedExpectedClientFailure err) -> do
      fbAst <- addLocalScopes @Fallback src ast
      -- We are either rewriting fallback errors with ligo message found at the
      -- same local scope or appending it to the end.
      -- TODO: global scope errors are not collecting
      return (fbAst ^. nestedLIGO, msg `rewriteAt` fromLigoErrorToMsg err)
      -- return (fbAst, msg <> [fromLigoErrorToMsg err])
    Left (LigoErrorNodeParseError err) -> do
      -- Print all other ligo errors at [1;1]
      fbAst <- addLocalScopes @Fallback src ast
      return (fbAst ^. nestedLIGO, msg <> [(point (-1) (-1), Error err [])])
    Left err -> throwM err

  where
    -- | Rewrite error message at the most local scope or append it to the end.
    rewriteAt :: [Msg] -> Msg -> [Msg]
    rewriteAt at what@(from, _) = maybe (at <> [what]) (\(i, _) -> at & element i .~ what) el
      where
        el = List.find ((from `leq`) . fst . snd) (zip [0..] at)
