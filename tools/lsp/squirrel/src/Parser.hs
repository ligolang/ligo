
{-
  The thing that can untangle the mess that tree-sitter produced.

  If there be errors, it /will/ be a mess.

  The AST you are building must:
  1) Have first field with type `ASTInfo` in each non-error constructor at each
     type.
  2) Have `Error`-only constructor to represent failure and implement `Stubbed`.

  I recommend parametrising your `AST` with some `info` typevar to be
  `ASTInfo` in the moment of parsing.

  I also recomment, in your tree-sitter grammar, to add `field("foo", ...)`
  to each sub-rule, that has `$.` in front of it -  in a rule, that doesn't
  start with `_` in its name.

  As a general rule of thumb, make each significant part a separate rule,
  even if it is a keyword. Then, apply previous advice.

  Only make rule start with `_` if it is a pure choice.

  ('block'
    ...
    a: <a>
    ...
    b: <b>
    ...)

  ->

  block = do
    subtree "block" do
      ctor Block
        <*> inside "a" a
        <*> inside "b" b
-}

module Parser (module Parser, gets, pfGrove) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

import Data.Foldable
import Data.Functor
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import ParseTree
import Range
import Pretty

import Debug.Trace

-- | Parse error.
data Error
  = Expected
    { eMsg   :: Text   -- ^ Description of what was expected.
    , eWhole :: Text   -- ^ Offending text.
    , eRange :: Range  -- ^ Location of the error.
    }
  deriving (Show) via PP Error

instance Pretty Error where
  pp (Expected msg found r) = "░" <> pp msg <> pp r <> "▒" <> pp found <> "▓"

-- | Parser of tree-sitter-made tree.
newtype Parser a = Parser
  { unParser
      :: WriterT [Error]      -- Early I though to report errors that way.
      (  StateT  ParseForest  -- Current forest to recognise.
      (  ExceptT Error        -- Backtracking. Change `Error` to `()`?
      (  Identity )))         -- I forgot why. `#include`? Debug via `print`?
         a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState   ParseForest
    , MonadWriter [Error]
    , MonadError   Error
    )

-- | Generate error originating at current location.
makeError :: Text -> Parser Error
makeError msg = do
  rng <- getRange
  makeError' msg rng

-- | Generate error originating at given location.
makeError' :: Text -> Range -> Parser Error
makeError' msg rng = do
  rng <- getRange
  src <- gets pfGrove <&> \case
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
  st@Forest {pfGrove, pfRange} <- get
  case pfGrove of
    [] -> die msg
    (_, t) : f -> do
      put st
        { pfRange = diffRange pfRange (ptRange t)
        , pfGrove = f
        }
      return t

-- | Pick a tree with that /field name/ or die with name as msg.
--
--   Will erase all subtrees with different names on the path!
--
field :: Text -> Parser a -> Parser a
field name parser = do
  grove <- gets pfGrove
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
      st@Forest {pfGrove = grove, pfRange = rng} <- get
      let (errs, grove') = delete name grove
      put Forest
        { pfID    = ptID
        , pfGrove = [(name, tree)]
        , pfRange = ptRange
        }

      res <- parser

      put st
        { pfGrove = grove'
        , pfRange = if firstOne then diffRange rng ptRange else rng
        }

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

unexpected :: ParseTree -> Error
unexpected ParseTree { ptSource, ptRange } =
  Expected "unexpected" ptSource ptRange

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
    save <- get
    put ptChildren
    rest <- gets pfGrove
    collectErrors rest
    parser <* put save
  else do
    die msg

-- | Because `ExceptT` requires error to be `Monoid` for `Alternative`.
(<|>) :: Parser a -> Parser a -> Parser a
Parser l <|> Parser r = Parser (l `catchError` const r)

select :: [Parser a] -> Parser a
select = foldl1 (<|>)

optional :: Parser a -> Parser (Maybe a)
optional p = fmap Just p <|> return Nothing

-- | Custom `Alternative.many`.
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
--   TODO: invent /proper/ 'ERROR'-node collector.
--
runParser :: Parser a -> FilePath -> IO (a, [Error])
runParser (Parser parser) fin = do
  pforest <- toParseTree fin
  let
    res =
             runIdentity
      $      runExceptT
      $ flip runStateT pforest
      $      runWriterT
      $ parser

  either (error . show) (return . fst) res

-- | Run parser on given file and pretty-print stuff.
debugParser :: Show a => Parser a -> FilePath -> IO ()
debugParser parser fin = do
  (res, errs) <- runParser parser fin
  putStrLn "Result:"
  print res
  unless (null errs) do
    putStrLn ""
    putStrLn "Errors:"
    for_ errs (print . nest 2 . pp)

-- | Consume next tree if it has give name. Or die.
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

-- | Get range of current tree or forest before the parser was run.
range :: Parser a -> Parser (a, Range)
range parser =
  get >>= \case
    Forest {pfGrove = (,) _ ParseTree {ptRange} : _} -> do
      a <- parser
      return (a, ptRange)

    Forest {pfRange} -> do
      a <- parser
      return (a, pfRange)

-- | Get current range.
getRange :: Parser Range
getRange = snd <$> range (return ())

-- | Remove all keys until given key is found; remove the latter as well.
--
--   Notice: this works differently from `Prelude.remove`!
--
delete :: Text -> [(Text, ParseTree)] -> ([ParseTree], [(Text, ParseTree)])
delete _ [] = ([], [])
delete k ((k', v) : rest) =
  if k == k'
  then (addIfError v [], rest)
  else (addIfError v vs, remains)
  where
    (vs, remains) = delete k rest

addIfError v =
  if ptName v == "ERROR"
  then (:) v
  else id

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

-- | For types that have a default replacer with an `Error`.
class Stubbed a where
  stub :: Error -> a

instance Stubbed Text where
  stub = pack . show

-- | This is bad, but I had to.
--
--   TODO: find a way to remove this instance.
--
instance Stubbed [a] where
  stub _ = []

-- | `Nothing` would be bad default replacer.
instance Stubbed a => Stubbed (Maybe a) where
  stub = Just . stub

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

-- | Equip given constructor with info.
ctor :: (ASTInfo -> a) -> Parser a
ctor = (<$> (ASTInfo <$> getRange <*> pure []))

-- | /Actual/ debug pring.
dump :: Parser ()
dump = gets pfGrove >>= traceShowM
