
module AST.Parser
  ( Source(..)
  , parse
  , parseWithScopes
  , parseWithScopes'
  ) where

import Control.Lens ( (&), (.~), element )
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try)
import Control.Monad.Catch (MonadThrow(throwM))
import qualified Data.ByteString.Lazy.Char8 as S8L
import qualified Data.List as List

import qualified AST.Parser.Pascaligo as Pascal
import qualified AST.Parser.Reasonligo as Reason
import qualified AST.Parser.Camligo as CAML
import           AST.Skeleton
import           AST.Scope

import Duplo (Lattice(leq))

import ParseTree
import Parser
import Extension
import Cli (LigoBinaryCallError(DecodedExpectedClientFailure), LigoBinaryCallError, fromLigoErrorToMsg)
import Cli.Types

parse :: Source -> IO (LIGO Info, [Msg])
parse src = do
  recogniser <- onExt ElimExt
    { eePascal = Pascal.recognise
    , eeCaml   = CAML.recognise
    , eeReason = Reason.recognise
    } (srcPath src)
  toParseTree src >>= runParserM . recogniser

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

  contractCode <- liftIO $ S8L.readFile (srcPath src)

  ast' <- addLocalScopes @impl (RawContractCode contractCode) ast
  return (ast', msg)

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

  contractCode <- liftIO $ srcToBytestring src

  ligoAst <- liftIO $ try @LigoBinaryCallError 
    $ addLocalScopes @FromCompiler (RawContractCode $ S8L.fromStrict contractCode) ast
  
  case ligoAst of 
    Right ast' -> 
      return (ast', msg)
    Left (DecodedExpectedClientFailure err) -> do 
      fbAst <- addLocalScopes @Fallback (RawContractCode $ S8L.fromStrict contractCode) ast
      -- We are either rewriting fallback errors with ligo message found at the 
      -- same local scope or appending it to the end.
      -- TODO: global scope errors are not collecting
      return (fbAst, msg `rewriteAt` fromLigoErrorToMsg err)
      -- return (fbAst, msg <> [fromLigoErrorToMsg err])
    Left err -> throwM err

  where 
    -- | Rewrite error message at the most local scope or append it to the end.
    rewriteAt :: [Msg] -> Msg -> [Msg]
    rewriteAt at what@(from, _) = maybe (at <> [what]) (\(i, _) -> at & element i .~ what) el
      where 
        el = List.find ((from `leq`) . fst . snd) (zip [0..] at)
