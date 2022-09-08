{- | The input tree from TreeSitter. Doesn't have any pointers to any data
     from actual tree the TS produced and therefore has no usage limitations.

     All datatypes here are strict.
-}

module ParseTree
  ( -- * Tree/Forest
    ParseTree (..)
  , ParseTreeNode (..)
  , RawInfo
  , RawTree
  , SomeRawTree(..)
  , Source(..)

    -- * Invoke the TreeSitter and get the tree it outputs
  , toParseTree

    -- * Read file contents from its source
  , pathToSrc
  )
  where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Functor.Classes (Show1 (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Traversable (for)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import Katip (LogItem (..), PayloadSelection (AllKeys), ToObject)
import TreeSitter.Language
import TreeSitter.Node
import TreeSitter.Parser
import TreeSitter.Tree hiding (Tree)
import UnliftIO.Exception (displayException)

import Duplo.Tree

import Extension
import Log (Log)
import Log qualified
import Range

foreign import ccall unsafe tree_sitter_PascaLigo  :: Ptr Language
foreign import ccall unsafe tree_sitter_ReasonLigo :: Ptr Language
foreign import ccall unsafe tree_sitter_CameLigo   :: Ptr Language
foreign import ccall unsafe tree_sitter_JsLigo     :: Ptr Language

data Source = Source
  { srcPath :: FilePath
  , srcIsDirty :: Bool
  , srcText :: Text
  } deriving stock (Eq, Ord)

instance ToJSON Source where
  toJSON src = object ["srcPath" .= srcPath src]

instance ToObject Source

instance LogItem Source where
  payloadKeys = const $ const AllKeys

instance Show Source where
  show = show . srcPath

-- | Reads the provided file path and checks whether it contains valid UTF-8
-- string, logging an error message in case and returning a leniently decoded
-- string in this situation.
pathToSrc :: Log m => FilePath -> m Source
pathToSrc p = do
  raw <- liftIO $ BS.readFile p
  -- Is it valid UTF-8?
  fmap (Source p False) $ Text.decodeUtf8' raw & \case
    Left exception -> do
      $(Log.err) [Log.i|LIGO expects UTF-8 encoded data, but #{p} has invalid data. #{displayException exception}.|]
      -- Leniently decode the data so we can continue working even with invalid
      -- data. Note that this case means we'll decode the file two times; this
      -- is fine, as invalid data should be very uncommon.
      -- TODO: Once we migrate to text-2.0:
      -- s/Text.decodeUtf8With Text.lenientDecode/Text.decodeUtf8Lenient/
      pure $ Text.decodeUtf8With Text.lenientDecode raw
    Right decoded -> pure decoded

type RawTree = Tree '[ParseTree] RawInfo
type RawInfo = (Range, Text)

data ParseTreeNode = ParseTreeNode
  { ptnName :: Text        -- ^ Name of the node
  , ptnInfo :: Maybe Text     -- ^ Additional information, stored in the node
  } deriving stock (Show, Eq)

-- | The tree tree-sitter produces.
data ParseTree self = ParseTree
  { ptName     :: ParseTreeNode -- ^ Node representation.
  , ptChildren :: [self]             -- ^ Subtrees.
  , ptSource   :: ~Text              -- ^ Source of the node.
  }
  deriving stock (Functor, Foldable, Traversable)

instance Show1 ParseTree where
  liftShowsPrec lift liftList d (ParseTree name children _) = showParen (d > appPrec)
    $ showString "ParseTree "
    . showsPrec (appPrec + 1) name . showChar ' '
    . liftShowsPrec lift liftList d children
    where
      appPrec :: Int
      appPrec = 10

data SomeRawTree = SomeRawTree Lang RawTree
  deriving stock (Show)

toParseTree :: (MonadIO m, Log m) => Lang -> Source -> m SomeRawTree
toParseTree dialect (Source fp _ input) = Log.addNamespace "toParseTree" do
  $(Log.debug) [Log.i|Reading #{fp}|]
  let language = case dialect of
        Pascal -> tree_sitter_PascaLigo
        Caml   -> tree_sitter_CameLigo
        Reason -> tree_sitter_ReasonLigo
        Js     -> tree_sitter_JsLigo

  res <- liftIO $ SomeRawTree dialect <$> withParser language \parser -> do
    let src = Text.encodeUtf8 input
    withParseTree parser src \tree ->
      withRootNode tree (peek >=> go src)

  $(Log.debug) [Log.i|Done reading #{fp}|]
  pure res
  where
    go :: ByteString -> Node -> IO RawTree
    go src node = do
      let count = fromIntegral $ nodeChildCount node
      allocaArray count $ \children -> do
        alloca $ \tsNodePtr -> do
          poke tsNodePtr $ nodeTSNode node
          ts_node_copy_child_nodes tsNodePtr children
          nodes <- for [0.. count - 1] $ \i -> do
            node' <- peekElemOff children i
            (only -> ((r, _), tree :: ParseTree RawTree)) <- go src node'
            field <-
              if   nodeFieldName node' == nullPtr
              then return ""
              else peekCString $ nodeFieldName node'
            pure $ fastMake (r, Text.pack field) tree

          ty <- peekCString $ nodeType node

          let
            start2D  = nodeStartPoint node
            finish2D = nodeEndPoint   node

            (name, info)
              | toInteger (nodeIsMissing node) == 1 = ("MISSING", Just (Text.pack ty))
              -- There are cases when `start2D == finish2D`, but the node is not Missing. We need to
              -- investigate such cases. To check such problem run trace on missing contracts from
              -- "test/error-recovery/simple/jsligo" and check cases when `nodeIsMissing` returns `False`,
              -- but `start2D == finish2D`.
              | start2D == finish2D = ("ERROR", Nothing)
              | otherwise = (Text.pack ty, Nothing)
            range = Range
              { _rStart  =
                  ( fromIntegral $ pointRow    start2D + 1
                  , fromIntegral $ pointColumn start2D + 1
                  , fromIntegral $ nodeStartByte node
                  )
              , _rFinish =
                  ( fromIntegral $ pointRow    finish2D + 1
                  , fromIntegral $ pointColumn finish2D + 1
                  , fromIntegral $ nodeEndByte node
                  )
              , _rFile = fp
              }

          pure $ fastMake
            (range, "")
            ParseTree
              { ptName     = ParseTreeNode name info
              , ptChildren = nodes
              , ptSource   = cutOut range src
              }
