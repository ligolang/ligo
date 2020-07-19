
{-# language StrictData #-}

{- | The input tree from TreeSitter. Doesn't have any pointers to any data
     from actual tree the TS produced and therefore has no usage limitations.

     All datatypes here are strict.
-}

module ParseTree
  ( -- * Tree/Forest
    ParseTree(..)
  , Source(..)
  , RawTree
  , RawInfo

    -- * Invoke the TreeSitter and get the tree it outputs
  , toParseTree
  -- , example
  )
  where

import Data.ByteString (ByteString)
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import Data.Traversable (for)

import           TreeSitter.Parser
import           TreeSitter.Tree hiding (Tree)
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

import           System.FilePath                (takeFileName)

import           System.IO.Unsafe (unsafePerformIO)

import Duplo.Pretty
import Duplo.Tree

import Range
import Product
import Debouncer

foreign import ccall unsafe tree_sitter_PascaLigo :: Ptr Language

data Source
  = Path       { srcPath :: FilePath }
  | Text       { srcPath :: FilePath, srcText :: Text }
  | ByteString { srcPath :: FilePath, srcBS   :: ByteString }

srcToBytestring :: Source -> IO ByteString
srcToBytestring = \case
  Path       p   -> BS.readFile p
  Text       _ t -> return $ Text.encodeUtf8 t
  ByteString _ s -> return s

type RawTree = Tree '[ParseTree] RawInfo
type RawInfo = Product [Range, Text]

instance Modifies RawInfo where
  ascribe (r :> n :> _) d = color 3 (pp n) <+> pp r `indent` pp d

-- | The tree tree-sitter produces.
data ParseTree self = ParseTree
  { ptName     :: Text         -- ^ Name of the node.
  , ptChildren :: [self]       -- ^ Subtrees.
  , ptSource   :: ~Text        -- ^ Range of the node.
  }
  deriving stock (Functor, Foldable, Traversable)

instance Pretty1 ParseTree where
  pp1 (ParseTree n forest _) =
    parens
      ( hang
        (quotes (text (Text.unpack n)))
        2
        (pp forest)
      )

-- | Feed file contents into PascaLIGO grammar recogniser.
toParseTree :: Source -> IO RawTree
toParseTree = unsafePerformIO $ debounced inner
  where
    inner fin = do
      parser <- ts_parser_new
      True   <- ts_parser_set_language parser tree_sitter_PascaLigo

      src <- srcToBytestring fin

      idCounter <- newIORef 0

      BS.useAsCStringLen src \(str, len) -> do
        tree <- ts_parser_parse_string parser nullPtr str len
        withRootNode tree (peek >=> go src idCounter)

      where
        nextID :: IORef Int -> IO Int
        nextID ref = do
          modifyIORef' ref (+ 1)
          readIORef ref

        go :: ByteString -> IORef Int -> Node -> IO RawTree
        go src idCounter node = do
          let count = fromIntegral $ nodeChildCount node
          allocaArray count \children -> do
            alloca \tsNodePtr -> do
              poke tsNodePtr $ nodeTSNode node
              ts_node_copy_child_nodes tsNodePtr children
              nodes <- for [0.. count - 1] \i -> do
                peekElemOff children i

              trees <- for nodes \node' -> do
                (only -> (r :> _, tree :: ParseTree RawTree)) <- go src idCounter node'
                field <-
                  if nodeFieldName node' == nullPtr
                  then return ""
                  else peekCString $ nodeFieldName node'
                return $ make (r :> Text.pack field :> Nil, tree)

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
                  , rFile = takeFileName $ srcPath fin
                  }

              return $ make (range :> "" :> Nil, ParseTree
                { ptName     = Text.pack ty
                , ptChildren = trees
                , ptSource   = cutOut range src
                })
