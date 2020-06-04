
{- |
  The thing that can untangle the mess that TreeSitter produces.

  In presence of serious errors, it /will/ be a mess, anyway.

  The AST you are building must be the @Tree@ in each point.

  I recommend, in your tree-sitter grammar, to add `field("foo", ...)`
  to each sub-rule, that has `$.` in front of it -  in a rule, that doesn't
  start with `_` in its name.

  As a general rule of thumb, make each significant part a separate rule,
  even if it is a keyword. Then, apply previous advice.

  Only make rule start with `_` if it is a pure choice.

  > ('block'
  >   ...
  >   a: <a>
  >   ...
  >   b: <b>
  >   ...)

  ->

  > block = do
  >   subtree "block" do
  >     ranged do
  >       pure Block
  >         <*> inside "a" a
  >         <*> inside "b" b
-}

module Parser
  ( -- * Parser type
    Parser
  , runParser
  , debugParser

    -- * Combinators
  , subtree
  , anything
  , token
  , stubbed
  , getInfo
  , inside

    -- * Replacement for `Alternative`, because reasons
  , many
  , some
  , (<|>)
  , optional
  , select

    -- * Debug
  , dump

    -- * Comments and ranges
  , ASTInfo(..)
  ) where

import Control.Lens hiding (inside)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

import Data.Foldable
import Data.Traversable
import Data.Functor
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import ParseTree
import Range
import Pretty
import HasComments
import Error
import Stubbed

import Debug.Trace

-- | Parser of tree-sitter-made tree.
--
--   TODO: separate state. Polysemy?
--
newtype Parser a = Parser
  { unParser
      :: WriterT [Error]                -- Early I though to report errors that way.
      (  StateT  (ParseForest, [Text])  -- Current forest to recognise + comments.
      (  ExceptT Error                  -- Backtracking. Change `Error` to `()`?
      (  Identity )))                   -- I forgot why. `#include`? Debug via `print`?
         a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState   (ParseForest, [Text])
    , MonadWriter [Error]
    , MonadError   Error
    )

-- | Generate error originating at current location.
makeError :: Text -> Parser Error
makeError msg = do
  rng <- currentRange
  makeError' msg rng

-- | Generate error originating at given location.
makeError' :: Text -> Range -> Parser Error
makeError' msg rng = do
  rng <- currentRange
  src <- gets (pfGrove . fst) <&> \case
    []                     -> ""
    (,) _ ParseTree { ptSource } : _ -> ptSource
  return Expected
    { eMsg   = msg
    , eWhole = src
    , eRange = rng
    }

-- | Pick next tree in a forest or die with msg.
takeNext :: Text -> Parser ParseTree
takeNext msg = do
  (st@Forest {pfGrove, pfRange}, comms) <- get
  case pfGrove of
    [] -> die msg
    (_, t) : f -> do
      if "comment" `Text.isSuffixOf` ptName t
      then do
        (st, comms) <- get
        put (st, ptSource t : comms)
        takeNext msg
      else do
        put
          ( st
            { pfRange = diffRange pfRange (ptRange t)
            , pfGrove = f
            }
          , comms
          )
        return t

--fields :: Text -> Parser a -> Parser [a]
--fields name parser = do
--  (fs, rest) <- gets $ splitForest name . fst
--  res <- for fs \f -> do
--    put f
--    parser
--
--  put rest
--  return res
--
--splitForest :: Text -> ParseForest -> [ParseForest]
--splitForest name = go . pfGrove
--  where
--    go []             acc fs = (fs, acc)
--    go ((tName, tree) : other) acc fs =
--      if tName == name
--      then go other [] (reverse (tree : acc) : fs)
--      else go other (tree : acc) fs

-- | Pick a tree with that /field name/ or die with name as msg.
--
--   Will erase all subtrees with different names on the path!
--
field :: Text -> Parser a -> Parser a
field name parser = do
  grove <- gets (pfGrove . fst)
  case grove of
    (name', t) : _
      | name == name' -> do
        sandbox True t

    _ -> do
      case lookup name grove of
        Just tree -> sandbox False tree
        Nothing   -> die name

  where
    sandbox firstOne tree@ParseTree {ptID, ptRange} = do
      (st@Forest {pfGrove = grove, pfRange = rng}, comments) <- get
      let (errs, new_comments, grove') = delete name grove
      put
        ( Forest
          { pfID    = ptID
          , pfGrove = [(name, tree)]
          , pfRange = ptRange
          }
        , comments ++ new_comments
        )

      res <- parser

      put
        ( st
          { pfGrove = grove'
          , pfRange = if firstOne then diffRange rng ptRange else rng
          }
        , []
        )

      for_ errs (tell . pure . unexpected)

      return res

-- | Variuos error reports.
fallback  :: Stubbed a => Text          -> Parser a
fallback' :: Stubbed a => Text -> Range -> Parser a
die       ::              Text          -> Parser a
die'      ::              Text -> Range -> Parser a
complain  ::              Text -> Range -> Parser ()
fallback  msg     = pure . stub =<< makeError  msg
fallback' msg rng = pure . stub =<< makeError' msg rng
die       msg     = throwError  =<< makeError  msg
die'      msg rng = throwError  =<< makeError' msg rng
complain  msg rng = tell . pure =<< makeError' msg rng

-- | When tree-sitter found something it was unable to process.
unexpected :: ParseTree -> Error
unexpected ParseTree { ptSource, ptRange } =
  Expected "not that" ptSource ptRange

-- | If a parser fails, return stub with error originating here.
stubbed :: Stubbed a => Text -> Parser a -> Parser a
stubbed msg parser = do
  parser <|> fallback msg

-- | The forest must start with tree of that name. Its subtrees become new
--   forest. Otherwise, it dies with name as msg.
subtree :: Text -> Parser a -> Parser a
subtree msg parser = do
  ParseTree {ptChildren, ptName} <- takeNext msg
  if ptName == msg
  then do
    (save, comms) <- get
    put (ptChildren, comms)
    rest <- gets (pfGrove . fst)
    collectErrors rest
    (_, comms') <- get
    parser <* put (save, comms')
  else do
    die msg

-- | Because `ExceptT` requires error to be `Monoid` for `Alternative`.
(<|>) :: Parser a -> Parser a -> Parser a
Parser l <|> Parser r = Parser (l `catchError` const r)

-- | Custom @foldl1 (<|>)@.
select :: [Parser a] -> Parser a
select = foldl1 (<|>)

-- | Custom @optionMaybe@.
optional :: Parser a -> Parser (Maybe a)
optional p = fmap Just p <|> return Nothing

-- | Custom `Alternative.many`.
--
--   TODO: remove, replace with `fields` combinator.
--
many :: Parser a -> Parser [a]
many p = many'
  where
    many' = some' <|> pure []
    some' = do
      x <- p
      xs <- many'
      return (x : xs)

-- | Custom `Alternative.some`.
--
some :: Parser a -> Parser [a]
some p = some'
  where
    many' = some' <|> pure []
    some' = do
      x <- p
      xs <- many'
      return (x : xs)

-- | Run parser on given file.
--
runParser :: Parser a -> FilePath -> IO (a, [Error])
runParser (Parser parser) fin = do
  pforest <- toParseTree fin
  let
    res =
             runIdentity
      $      runExceptT
      $ flip runStateT (pforest, [])
      $      runWriterT
      $ parser

  either (error . show) (return . fst) res

-- | Run parser on given file and pretty-print stuff.
--
debugParser :: Show a => Parser a -> FilePath -> IO ()
debugParser parser fin = do
  (res, errs) <- runParser parser fin
  putStrLn "Result:"
  print res
  unless (null errs) do
    putStrLn ""
    putStrLn "Errors:"
    for_ errs (print . nest 2 . pp)

-- | Consume next tree if it has the given name. Or die.
token :: Text -> Parser Text
token node = do
  tree@ParseTree {ptName, ptRange, ptSource} <- takeNext node
  if ptName == node
  then return ptSource
  else die' node ptRange

-- | Consume next tree, return its textual representation.
anything :: Parser Text
anything = do
  tree <- takeNext "anything"
  return $ ptSource tree

-- | Get range of the current tree (or forest) before the parser was run.
range :: Parser a -> Parser (a, Range)
range parser =
  get >>= \case
    (,) Forest {pfGrove = (,) _ ParseTree {ptRange} : _} _ -> do
      a <- parser
      return (a, ptRange)

    (,) Forest {pfRange} _ -> do
      a <- parser
      return (a, pfRange)

-- | Get current range.
currentRange :: Parser Range
currentRange = snd <$> range (return ())

-- | Remove all keys until given key is found; remove the latter as well.
--
--   Also returns all ERROR-nodes.
--
--   TODO: rename.
--
--   Notice: this works differently from `Prelude.remove`!
--
delete :: Text -> [(Text, ParseTree)] -> ([ParseTree], [Text], [(Text, ParseTree)])
delete _ [] = ([], [], [])
delete k ((k', v) : rest) =
  if k == k'
  then (addIfError v [], addIfComment v [], rest)
  else (addIfError v vs, addIfComment v cs, remains)
  where
    (vs, cs, remains) = delete k rest
    addIfError v =
      if ptName v == "ERROR"
      then (:) v
      else id

    addIfComment v =
      if "comment" `Text.isSuffixOf` ptName v
      then (ptSource v :)
      else id

-- | Report all ERRORs from the list.
collectErrors :: [(Text, ParseTree)] -> Parser ()
collectErrors vs =
  for_ vs \(_, v) -> do
    when (ptName v == "ERROR") do
      tell [unexpected v]

-- | Parser negation.
notFollowedBy :: Parser a -> Parser ()
notFollowedBy parser = do
  good <- do
      parser
      return False
    <|> do
      return True

  unless good do
    die "notFollowedBy"

-- | Universal accessor.
--
--   Usage:
--
--   > inside "$field:$treename"
--   > inside "$field"
--   > inside ":$treename" -- don't, use "subtree"
--
inside :: Stubbed a => Text -> Parser a -> Parser a
inside sig parser = do
  let (f, st') = Text.breakOn ":" sig
  let st       = Text.drop 1 st'
  if Text.null f
  then do
    -- The order is important.
    subtree st do
      stubbed f do
        parser
  else do
    field f do
      stubbed f do
        if Text.null st
        then do
          parser
        else do
          subtree st do
            parser

-- Auto-accumulated information to be fed into AST being build.
data ASTInfo = ASTInfo
  { aiRange    :: Range
  , aiComments :: [Text]
  }

instance HasComments ASTInfo where
  getComments = aiComments

instance HasRange ASTInfo where
  getRange = aiRange

-- | Equip given constructor with info.
getInfo :: Parser ASTInfo
getInfo = ASTInfo <$> currentRange <*> grabComments

-- | Take the accumulated comments, clean the accumulator.
grabComments :: Parser [Text]
grabComments = do
  (st, comms) <- get
  put (st, [])
  return comms

-- | /Actual/ debug pring.
dump :: Parser ()
dump = gets (pfGrove . fst) >>= traceShowM
