
module Parser (module Parser, gets, pfGrove) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

import Data.Foldable
import Data.Text.Encoding
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import ParseTree
import Range
import Pretty

import Debug.Trace

data Error
  = Expected
    { eMsg   :: Text
    , eWhole :: Text
    , eRange :: Range
    }
  deriving stock (Show)

instance Pretty Error where
  pp (Expected msg found r) = "░" <> pp msg <> pp r <> "▒" <> pp found <> "▓"

newtype Parser a = Parser
  { unParser
      :: WriterT [Error]
      (  ReaderT ParserEnv
      (  StateT  ParseForest
      (  ExceptT Error
      (  IO ))))
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
    , MonadIO
    )

makeError :: Text -> Parser Error
makeError msg = do
  rng <- getRange
  makeError' msg rng

makeError' :: Text -> Range -> Parser Error
makeError' msg rng = do
  rng <- getRange
  src <- cutOut rng
  return Expected
    { eMsg   = msg
    , eWhole = src
    , eRange = rng
    }

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
      let grove' = delete name grove
      put Forest
        { pfID    = ptID
        , pfGrove = [(name, tree)]
        , pfRange = ptRange
        }

      parser <* put st
        { pfGrove = grove'
        , pfRange = if firstOne then diffRange rng ptRange else rng
        }

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
    some' = do
      hasPossibleInput
      (x, consumed) <- productive p
      if consumed then do
        xs <- many'
        return (x : xs)
      else do
        return [x]

some :: Text -> Parser a -> Parser [a]
some msg p = some'
  where
    many' = some' <|> pure []
    some' = do
      hasPossibleInput
      (x, consumed) <- productive p
      if consumed then do
        xs <- many'
        return (x : xs)
      else do
        return [x]

getTreeID :: Parser (Maybe Int)
getTreeID = Parser do
  pfGrove <$> get >>= return . \case
    [] -> Nothing
    (_, tree) : _ -> Just (ptID tree)

productive :: Parser a -> Parser (a, Bool)
productive p = do
  was <- getTreeID
  res <- p
  now <- getTreeID
  return (res, was /= now)

hasPossibleInput :: Parser ()
hasPossibleInput = do
  yes <- gets (not . null . pfGrove)
  unless yes do
    die "something"

data ParserEnv = ParserEnv
  { peSource :: ByteString
  }

puts :: MonadIO m => Show a => a -> m ()
puts = liftIO . print

runParser :: Parser a -> FilePath -> IO (a, [Error])
runParser (Parser parser) fin = do
  pforest <- toParseTree fin
  text    <- ByteString.readFile fin
  res <-
             runExceptT
      $ flip runStateT pforest
      $ flip runReaderT (ParserEnv text)
      $      runWriterT
      $ parser

  either (error . show) (return . fst) res

debugParser :: Parser a -> FilePath -> IO a
debugParser parser fin = do
  (res, errs) <- runParser parser fin
  putStrLn "Errors:"
  for_ errs (print . nest 2 . pp)
  putStrLn ""
  putStrLn "Result:"
  return res

token :: Text -> Parser Text
token node = do
  tree@ParseTree {ptName, ptRange} <- takeNext node
  if ptName == node
  then do
    cutOut ptRange

  else do
    die' node ptRange

anything :: Parser Text
anything = do
  tree <- takeNext "anything"
  cutOut $ ptRange tree

consume :: Text -> Parser ()
consume node = do
  ParseTree {ptName, ptRange} <- takeNext node
  when (ptName /= node) do
    complain node ptRange

consumeOrDie :: Text -> Parser ()
consumeOrDie node = do
  ParseTree {ptName, ptRange} <- takeNext node
  when (ptName /= node) do
    die' node ptRange

cutOut :: Range -> Parser Text
cutOut (Range (_, _, s) (_, _, f)) = do
  bs <- asks peSource
  return $ decodeUtf8 $ ByteString.take (f - s) (ByteString.drop s bs)

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
  else delete k rest

notFollowedBy :: Parser a -> Parser ()
notFollowedBy parser = do
  good <- do
      parser
      return False
    <|> do
      return True

  unless good do
    die "notFollowedBy"

class Stubbed a where
  stub :: Error -> a

instance Stubbed Text where
  stub = pack . show

instance Stubbed [a] where
  stub _ = []

instance Stubbed a => Stubbed (Maybe a) where
  stub = Just . stub

inside :: Stubbed a => Text -> Parser a -> Parser a
inside sig parser = do
  let (f, st') = Text.breakOn ":" sig
  let st       = Text.drop 1 st'
  if Text.null f
  then do
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

data ASTInfo = ASTInfo
  { aiRange    :: Range
  , aiComments :: [Text]
  }

ctor :: (ASTInfo -> a) -> Parser a
ctor = (<$> (ASTInfo <$> getRange <*> pure []))

dump :: Parser ()
dump = gets pfGrove >>= traceShowM
