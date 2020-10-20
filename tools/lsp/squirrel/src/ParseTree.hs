{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StrictData, TupleSections #-}

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
  )
  where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Map
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Traversable (for)

import           Control.Monad ((>=>))
import           Foreign.C.String (peekCString)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Marshal.Array (allocaArray)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.Storable (peek, peekElemOff, poke)
import           TreeSitter.Language
import           TreeSitter.Node
import           TreeSitter.Parser
import           TreeSitter.Tree hiding (Tree)

import           System.FilePath (takeFileName)

import           Duplo.Pretty as PP
import           Duplo.Tree

import           Debouncer
import           Extension
import           Product
import           Range
import           Log

foreign import ccall unsafe tree_sitter_PascaLigo  :: Ptr Language
foreign import ccall unsafe tree_sitter_ReasonLigo :: Ptr Language
foreign import ccall unsafe tree_sitter_CameLigo   :: Ptr Language

data Source
  = Path       { srcPath :: FilePath }
  | Text       { srcPath :: FilePath, srcText :: Text }
  | ByteString { srcPath :: FilePath, srcBS   :: ByteString }

instance IsString Source where
  fromString = Path

instance Show Source where
  show = show . srcPath

srcToBytestring :: Source -> IO ByteString
srcToBytestring = \case
  Path       p   -> BS.readFile p
  Text       _ t -> return $ Text.encodeUtf8 t
  ByteString _ s -> return s

type RawTree = Tree '[ParseTree] RawInfo
type RawInfo = Product [Range, Text]

-- instance {-# OVERLAPS #-} Modifies RawInfo where
--   ascribe (r :> n :> _) d = color 3 (pp n) `indent` pp d

data TreeKind
  = Error
  | Comment
  | Field Text
  deriving stock (Eq, Ord)

-- TODO: move and refactor
instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pp = pp . fmap snd . toList

instance Pretty TreeKind where
  pp = \case
    Error -> "error"
    Comment -> "comment"
    Field t -> "field (" PP.<.> pp t PP.<.> ")"

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
toParseTree = unsafeDebounce \fin -> do
  Log.debug "TS" [i|Reading #{fin}|]
  language <- onExt ElimExt
    { eePascal = tree_sitter_PascaLigo
    , eeCaml   = tree_sitter_CameLigo
    , eeReason = tree_sitter_ReasonLigo
    } (srcPath fin)

  withParser language \parser -> do
    src <- srcToBytestring fin
    res <- withParseTree parser src \tree -> do
      withRootNode tree (peek >=> go fin src)
    Log.debug "TS" [i|Done reading #{fin}|]
    return res

  where
    go :: Source -> ByteString -> Node -> IO RawTree
    go fin src node = do
      let count = fromIntegral $ nodeChildCount node
      allocaArray count $ \children -> do
        alloca $ \tsNodePtr -> do
          poke tsNodePtr $ nodeTSNode node
          ts_node_copy_child_nodes tsNodePtr children
          nodes <- for [0.. count - 1] $ \i -> do
            peekElemOff children i

          trees <- for nodes \node' -> do
            (only -> (r :> _, tree :: ParseTree RawTree)) <- go fin src node'
            field <-
              if   nodeFieldName node' == nullPtr
              then return ""
              else peekCString $ nodeFieldName node'
            return $ make (r :> Text.pack field :> Nil, tree)

          ty <- peekCString $ nodeType node

          let
            start2D  = nodeStartPoint node
            finish2D = nodeEndPoint   node
            i        = fromIntegral

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

          return $ make
            ( range :> "" :> Nil
            , ParseTree
              { ptName     = Text.pack ty
              , ptChildren = trees
              , ptSource   = cutOut range src
              }
            )
