{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.LIGO.Debugger.Util.Parser
  ( ParserM
  , LineMarkerType (..)
  , LineMarker (..)
  , FieldDecodeExceptionReason (..)
  , FieldDecodeException (..)
  , CodeSource (..)
  , Info
  , ParsedInfo

  , runParserM
  , collectTreeErrors
  , parseLineMarkerText
  , flag
  , field
  , fieldOpt
  , fields
  , fields1
  , emptyParsedInfo
  , fillInfo
  , withComments
  , boilerplate
  , boilerplate'
  , fallthrough
  , noMatch
  ) where

import Prelude hiding (Product)

import Control.Exception (throwIO)
import Control.Monad.RWS hiding (Product)
import Data.Text qualified as Text
import Language.LSP.Types qualified as J
import Text.Interpolation.Nyan
import Unsafe qualified

import Duplo.Pretty hiding (int)
import Duplo.Tree

import Language.LIGO.Debugger.Util.AST.Skeleton (Error (..), SomeLIGO, getLIGO)
import Language.LIGO.Debugger.Util.Diagnostic (Message (..), MessageDetail (..), Severity (..))
import Language.LIGO.Debugger.Util.ParseTree
import Language.LIGO.Debugger.Util.Product
import Language.LIGO.Debugger.Util.Range

{-
  Comment grabber has 2 buffers: 1 and 2.

  1) We collect all comments before the node and append them into buffer 1.
  2) We collect all comments after the node and put them into buffer 2.
  3) `grabComments` takes all comments from buffer 1.
  4) On leaving, move move comments from 2 to 1.
-}

-- | Reader environment for the parser.
data ParserEnv = ParserEnv
  { peNodes :: [RawTree]
  -- ^ Nodes that must yet be parsed.
  , peNodeType :: Text
  -- ^ Type of the node being parsed. Stored for better error messages.
  , peNodeRange :: Range
  -- ^ Type of the node being parsed. Stored for better error messages.
  }

runParserM :: MonadIO m => ParserM a -> m (a, [Message])
runParserM p = liftIO $ (\(a, _, errs) -> (a, errs)) <$> runRWST p initEnv ([], [])
  where
    initEnv = ParserEnv
      { peNodes = []
      , peNodeType = ""
      , peNodeRange = point 0 0
      }

type ParserM = RWST ParserEnv [Message] ([Text], [Text]) IO

collectTreeErrors :: Contains Range info => SomeLIGO info -> [Message]
collectTreeErrors =
  map (\(info, Error msg _) -> Message msg SeverityError (getRange info)) . collect . getLIGO

-- | The flag of some line marker.
--
-- Note that we make the assumption that a flag may only be 1 or 2, since LIGO
-- should not have system header files or be wrapped in `extern "C"` blocks.
data LineMarkerType
  = RootFile      -- ^ No flag.
  | IncludedFile  -- ^ Flag 1.
  | ReturnToFile  -- ^ Flag 2.
  deriving stock (Eq, Show)

-- | A inclusion line marker left by running `ligo preprocess`.
--
-- Note that we assume that we may only have zero or one flag instead of zero or
-- more, since flags 1 and 2 are mutually exclusive. See 'LineMarkerType'.
--
-- See also: https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html
data LineMarker = LineMarker
  { lmFile :: FilePath  -- ^ The file that was included.
  , lmFlag :: LineMarkerType  -- ^ The "parsed" flag of the line marker.
  , lmLine :: J.UInt  -- ^ The line number that should be used after the inclusion.
  , lmLoc  :: Range  -- ^ The location in the preprocessed file where the line marker was added.
  } deriving stock (Eq, Show)

parseLineMarkerText :: Text -> Maybe (FilePath, LineMarkerType, J.UInt)
parseLineMarkerText marker = do
  "#" : lineStr : fileText : flags <- Just $ words marker
  line <- readMaybe $ toString lineStr
  let file = toString $ Text.init $ Text.tail fileText
  let markerType = case flags of
        -- TODO: There is an edge case when there are line markers and comments
        -- (see src/test/contracts/includer.{,m,re}ligo, so we assume for now
        -- that any extra fields after "1" or "2" are comments, and anything
        -- else is a root file (possibly with comments).
        "1" : _ -> IncludedFile
        "2" : _ -> ReturnToFile
        _       -> RootFile
  pure (file, markerType, line)

parseLineMarker :: (RawInfo, ParseTree RawTree) -> Maybe LineMarker
parseLineMarker ((range, _), ParseTree (ParseTreeNode ty _) _ marker) = do
  guard (ty == "line_marker")
  (file, markerType, line) <- parseLineMarkerText marker
  pure $ LineMarker file markerType line range

data FieldDecodeExceptionReason
  = UnrecognizedField
  | EmptyField
  deriving stock (Show)

data FieldDecodeException = FieldDecodeException
  { ufeFieldName :: Text
  , ufeNodeType :: Text
  , ufeNodeRange :: Range
  , ufeReason :: FieldDecodeExceptionReason
  } deriving stock (Show)

instance Exception FieldDecodeException where
  displayException FieldDecodeException{ufeFieldName, ufeNodeType, ufeNodeRange, ufeReason} =
    [int||Cannot decode field `#{ufeFieldName}` with `#{ufeNodeType}` (at #{ufeNodeRange}). Reason: #{reason}.|]
    where
      reason :: String
      reason = case ufeReason of
        UnrecognizedField -> "Field not found"
        EmptyField -> "Expected 1 or more fields, but found 0"

instance Scoped (Range, Text) ParserM RawTree ParseTree where
  before _ (ParseTree _ cs _) = do
    let (comms, rest) = allComments cs
    let (comms1, _)   = allComments $ reverse rest
    modify $ first  (++ comms)
    modify $ second (++ reverse comms1)

    tell $ allErrors cs

  after _ _ = do
    modify $ \(_, y) -> (y, [])

grabComments :: ParserM [Text]
grabComments = do
  ls <- gets fst
  modify \(_, y) -> ([], y)
  return ls

allComments :: [RawTree] -> ([Text], [RawTree])
allComments = first (map getBody . filter isComment) . break isMeaningful
  where
    isMeaningful :: RawTree -> Bool
    isMeaningful = not . Text.null . snd . extract

    isComment :: RawTree -> Bool
    isComment (gist -> (ParseTree (ParseTreeNode ty _) _ _)) = "comment" `Text.isSuffixOf` ty

allErrors :: [RawTree] -> [Message]
allErrors = mapMaybe extractUnnamedError
  where
    extractUnnamedError :: RawTree -> Maybe Message
    extractUnnamedError tree = case only tree of
      ((r, ""), ParseTree (ParseTreeNode "ERROR" _) _ _)
        -> Just (Message (Unexpected $ getBody tree) SeverityError r)
      ((r, ""), ParseTree (ParseTreeNode "MISSING" info) _ _)
        -> Just (Message (Missing $ fromMaybe (getBody tree) info) SeverityError r)
      _ -> Nothing

getBody :: RawTree -> Text
getBody (gist -> f) = ptSource f

flag :: Text -> ParserM Bool
flag name = fieldOpt name <&> isJust

field :: Text -> ParserM RawTree
field name =
  fieldOpt name
    >>= maybe
      (do
        ParserEnv {peNodeType, peNodeRange} <- ask
        lift $ throwIO $ FieldDecodeException name peNodeType peNodeRange UnrecognizedField)
      pure

fieldOpt :: Text -> ParserM (Maybe RawTree)
fieldOpt name = find ((== name) . snd . extract) <$> asks peNodes

fields :: Text -> ParserM [RawTree]
fields name = go <$> asks peNodes
  where
    go [] = []
    go (tree : rest)
      | (_, n) <- extract tree, n == name = tree : go rest
      | errorAtTheTop tree = tree : go rest
      | otherwise = go rest

    errorAtTheTop :: RawTree -> Bool
    errorAtTheTop (match -> Just (_, ParseTree (ParseTreeNode "ERROR" _) _ _)) = True
    errorAtTheTop _ = False

fields1 :: Text -> ParserM (NonEmpty RawTree)
fields1 name = fields name >>= \case
  [] -> do
    ParserEnv {peNodeType, peNodeRange} <- ask
    lift $ throwIO $ FieldDecodeException name peNodeType peNodeRange EmptyField
  x : xs -> pure (x :| xs)

newtype CodeSource = CodeSource { unCodeSource :: Text }
  deriving newtype (Eq, Ord, Show, Pretty)

type Info = [[Text], [LineMarker], Range, CodeSource]

type ParsedInfo = PreprocessedRange ': Info

emptyParsedInfo :: Product ParsedInfo
emptyParsedInfo =
  PreprocessedRange emptyPoint :> [] :> [] :> emptyPoint :> CodeSource "" :> Nil
  where
    emptyPoint = point 0 0

instance Contains [Text] xs => Modifies (Product xs) where
  ascribe = ascribeComms . getElem

fillInfo :: Functor f => f (Product xs) -> f (Product ([Text] : Range : xs))
fillInfo = fmap \it -> [] :> point 0 0 :> it

ascribeComms :: [Text] -> Doc -> Doc
ascribeComms comms
  | null comms = id
  | otherwise  = \d ->
      block $ map pp comms ++ [d]

withComments
  :: ParserM (Product xs, a)
  -> ParserM (Product ([Text] : xs), a)
withComments act = do
  comms <- grabComments
  first (comms :>) <$> act

getMarkers :: [RawTree] -> [LineMarker]
getMarkers = mapMaybe (parseLineMarker . Unsafe.fromJust . match)

boilerplateImpl
  :: ParserM (f RawTree)
  -> RawInfo
  -> ParseTree RawTree
  -> ParserM (Product Info, f RawTree)
boilerplateImpl handler (r, _) (ParseTree (ParseTreeNode ty _) cs src) =
  withComments do
    -- TODO: What is exactly the appropriate action in case something ever
    -- returns 'Nothing'? 'catMaybes'? If something goes wrong, then we will
    -- probably get unwanted behavior in 'AST.Parser'.
    let markers = getMarkers cs
    f' <- local (const $ ParserEnv cs ty r) handler
    return (markers :> r :> CodeSource src :> Nil, f')

boilerplate
  :: (Text -> ParserM (f RawTree))
  -> RawInfo
  -> ParseTree RawTree
  -> ParserM (Product Info, f RawTree)
boilerplate f info pt = boilerplateImpl (f $ ptnName $ ptName pt) info pt

boilerplate'
  :: ((Text, Text) -> ParserM (f RawTree))
  -> RawInfo
  -> ParseTree RawTree
  -> ParserM (Product Info, f RawTree)
boilerplate' f info pt@(ParseTree (ParseTreeNode ty _) _ src) = boilerplateImpl (f (ty, src)) info pt

fallthrough :: ParserM a
fallthrough = lift $ throwIO HandlerFailed

noMatch :: RawInfo -> ParseTree it -> ParserM (Product Info, Error it)
noMatch (r, _) (ParseTree _ children source) = withComments $ pure
  ( [] :> r :> CodeSource source :> Nil
  , Error (Unrecognized source) children
  )
