
module Parser where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

import Data.Text.Encoding
-- import Data.Traversable (for)
import Data.Text (Text, pack, unpack)

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import ParseTree

-- import Debug.Trace

data Error
  = Expected Text Range
  | Unexpected Range
  deriving stock (Show)

newtype Parser a = Parser
  { unParser
      :: WriterT [Error]
      (  ReaderT ParserEnv
      (  StateT  ParseForest
      (  ExceptT Error
      (  Identity ))))
         a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState   ParseForest
    , MonadWriter [Error]
    , MonadReader  ParserEnv
    , MonadError   Error
    )

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

field :: Stubbed a => Text -> Parser a -> Parser a
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
      let grove' = delete name grove
      put Forest
        { pfID    = ptID
        , pfGrove = [(name, tree)]
        , pfRange = ptRange
        }

      stubbed name parser <* put st
        { pfGrove = grove'
        , pfRange = if firstOne then diffRange rng ptRange else rng
        }

fallback :: Stubbed a => Text -> Parser a
fallback msg = (stub . Expected msg) <$> getRange

die :: Text -> Parser a
die msg = throwError . Expected msg =<< getRange

stubbed :: Stubbed a => Text -> Parser a -> Parser a
stubbed msg parser = do
  parser <|> fallback msg

subtree :: Text -> Parser a -> Parser a
subtree msg parser = do
  ParseTree {ptChildren, ptName} <- takeNext msg
  if ptName == msg
  then do
    save <- get
    put ptChildren
    parser <* put save
  else do
    die msg

(<|>) :: Parser a -> Parser a -> Parser a
Parser l <|> Parser r = Parser (l `catchError` const r)

select :: [Parser a] -> Parser a
select = foldl1 (<|>)

optional :: Parser a -> Parser (Maybe a)
optional p = fmap Just p <|> return Nothing

many :: Text -> Parser a -> Parser [a]
many msg p = many'
  where
    many' = some' <|> pure []
    some' = (:) <$> (productive msg p) <*> many'

some :: Text -> Parser a -> Parser [a]
some msg p = some'
  where
    many' = some' <|> pure []
    some' = (:) <$> (productive msg p) <*> many'

getTreeID :: Parser (Maybe Int)
getTreeID = Parser do
  pfGrove <$> get >>= return . \case
    [] -> Nothing
    (_, tree) : _ -> Just (ptID tree)

productive :: Text -> Parser a -> Parser a
productive msg p = do
  was <- getTreeID
  res <- p
  now <- getTreeID
  unless (was /= now) do
    error ("unproductive: " ++ unpack msg)
  return res

data ParserEnv = ParserEnv
  { peRange  :: Range
  , peSource :: ByteString
  }

runParser :: Parser a -> FilePath -> IO (a, [Error])
runParser (Parser parser) fin = do
  pforest <- toParseTree fin
  text    <- ByteString.readFile fin
  let
    res
      =      runIdentity
      $      runExceptT
      $ flip runStateT pforest
      $ flip runReaderT (ParserEnv (pfRange pforest) text)
      $      runWriterT
      $ parser

  either (error . show) (return . fst) res

token :: Text -> Parser Text
token node = do
  tree@ParseTree {ptName, ptRange} <- takeNext node
  if ptName == node
  then do
    source <- asks peSource
    return (cutOut source tree)

  else do
    throwError $ Expected node ptRange

anything :: Parser Text
anything = do
  tree <- takeNext "anything"
  source <- asks peSource
  return (cutOut source tree)

consume :: Text -> Parser ()
consume node = do
  ParseTree {ptName, ptRange} <- takeNext node
  when (ptName /= node) do
    tell [Expected node ptRange]

cutOut :: ByteString -> ParseTree -> Text
cutOut bs (ParseTree _ _ s f _ _) =
  decodeUtf8 $ ByteString.take (f - s) (ByteString.drop s bs)

range :: Parser a -> Parser (a, Range)
range parser =
  get >>= \case
    Forest {pfGrove = (,) _ ParseTree {ptRange} : _} -> do
      a <- parser
      return (a, ptRange)

    Forest {pfRange} -> do
      a <- parser
      return (a, pfRange)

getRange :: Parser Range
getRange = snd <$> range (return ())

delete :: Eq k => k -> [(k, v)] -> [(k, v)]
delete _ [] = []
delete k ((k', v) : rest) =
  if k == k'
  then rest
  else (k', v) : delete k rest

class Stubbed a where
  stub :: Error -> a

instance Stubbed [a] where
  stub _ = []

instance Stubbed Text where
  stub e = pack ("<" <> show e <> ">")
