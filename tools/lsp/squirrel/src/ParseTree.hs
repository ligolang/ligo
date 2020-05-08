
{-# language StrictData #-}

module ParseTree where

import Data.IORef
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Traversable (for)
import Data.Text.Encoding
import Data.Text.Foreign (withCStringLen)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Language
import           TreeSitter.Node
import           Foreign.C.String (peekCString)
import           Foreign.Ptr                    ( Ptr
                                                , nullPtr
                                                )
import           Foreign.Marshal.Alloc          ( alloca )
import           Foreign.Marshal.Array          ( allocaArray )
import           Foreign.Storable               ( peek
                                                , peekElemOff
                                                , poke
                                                )
import           Control.Monad ((>=>))

import Text.PrettyPrint hiding ((<>))

import Paths_squirrel

import Range
import Pretty

-- import Debug.Trace

foreign import ccall unsafe tree_sitter_PascaLigo :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "../pascaligo/src/node-types.json"

-- | The tree tree-sitter produces.
data ParseTree = ParseTree
  { ptID       :: Int          -- ^ Unique number, for fast comparison.
  , ptName     :: Text         -- ^ Name of the node.
  , ptRange    :: Range        -- ^ Range of the node.
  , ptChildren :: ParseForest  -- ^ Subtrees.
  , ptSource   :: ~Text        -- ^ Range of the node.
  }
  deriving (Show) via PP ParseTree

-- ^ The forest we work with.
data ParseForest = Forest
  { pfID    :: Int                  -- ^ Unique number for comparison.
  , pfGrove :: [(Text, ParseTree)]  -- ^ Subtrees.
  , pfRange :: Range                -- ^ Full range of the forest.
  }
  deriving (Show) via PP ParseForest

instance Pretty ParseTree where
  pp (ParseTree _ n r forest _) =
    parens
      ( hang
        (quotes (text (Text.unpack n)) <+> pp r)
        2
        (pp forest)
      )

instance Pretty ParseForest where
  pp = vcat . map ppPair . pfGrove
    where
      ppPair (field, tree) =
        if field == Text.empty
        then nest 2 $ pp tree
        else hang (text (Text.unpack field) <> ": ") 2 (pp tree)

-- | Extract textual representation of given range.
cutOut :: Range -> ByteString -> Text
cutOut (Range (_, _, s) (_, _, f)) bs =
  decodeUtf8
    $ BS.take (f - s)
    $ BS.drop  s
      bs

-- | Feed file contents into PascaLIGO grammar recogniser.
toParseTree :: FilePath -> IO ParseForest
toParseTree fin = do
  parser <- ts_parser_new
  True   <- ts_parser_set_language parser tree_sitter_PascaLigo

  src <- BS.readFile fin

  idCounter <- newIORef 0

  BS.useAsCStringLen src \(str, len) -> do
    tree <- ts_parser_parse_string parser nullPtr str len
    finalTree <- withRootNode tree (peek >=> go src idCounter)
    return $ Forest 0 [("", finalTree)] (ptRange finalTree)

  where
    nextID :: IORef Int -> IO Int
    nextID ref = do
      modifyIORef' ref (+ 1)
      readIORef ref

    go :: ByteString -> IORef Int -> Node -> IO ParseTree
    go src idCounter node = do
      let count = fromIntegral $ nodeChildCount node
      allocaArray count \children -> do
        alloca \tsNodePtr -> do
          poke tsNodePtr $ nodeTSNode node
          ts_node_copy_child_nodes tsNodePtr children
          nodes <- for [0.. count - 1] \i -> do
            peekElemOff children i

          trees <- for nodes \node' -> do
            tree <- go src idCounter node'
            field <-
              if nodeFieldName node' == nullPtr
              then return ""
              else peekCString $ nodeFieldName node'
            return (Text.pack field, tree)

          ty <- peekCString $ nodeType node

          let
            start2D  = nodeStartPoint node
            finish2D = nodeEndPoint   node
            i = fromIntegral

          treeID <- nextID idCounter
          fID    <- nextID idCounter

          let
            range = Range
              { rStart  =
                  ( i $ pointRow    start2D + 1
                  , i $ pointColumn start2D + 1
                  , i $ nodeStartByte node
                  )

              , rFinish =
                  ( i $ pointRow    finish2D + 1
                  , i $ pointColumn finish2D + 1
                  , i $ nodeEndByte node
                  )
              }

          return $ ParseTree
            { ptID       = treeID
            , ptName     = Text.pack ty
            , ptRange    = range
            , ptChildren = Forest fID trees range
            , ptSource   = cutOut range src
            }