
{-# language Strict #-}

module ParseTree where

import Data.IORef
import qualified Data.Text as Text
import Data.Traversable (for)
import Data.Text.Foreign (withCStringLen)
import Data.Text.IO as IO

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
import PrettyPrint

-- import Debug.Trace

foreign import ccall unsafe tree_sitter_PascaLigo :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "../pascaligo/src/node-types.json"

data ParseTree = ParseTree
  { ptID       :: Int
  , ptName     :: Text.Text
  , ptStart    :: Int
  , ptFinish   :: Int
  , ptRange    :: Range
  , ptChildren :: ParseForest
  }

data ParseForest = Forest
  { pfID    :: Int
  , pfGrove :: [(Text.Text, ParseTree)]
  , pfRange :: Range
  }

instance Show ParseTree where
  show = show . pp

instance Show ParseForest where
  show = show . pp

instance Pretty ParseTree where
  pp (ParseTree _ n _ _ (Range (sr, sc) (fr, fc)) forest) =
    parens
      ( hang
        (   quotes (text (Text.unpack n))
        <+> brackets
          ( int sr <> ":" <> int sc
          <> " - "
          <> int fr <> ":" <> int fc
          )
        )
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

toParseTree :: FilePath -> IO ParseForest
toParseTree fin = do
  parser <- ts_parser_new
  True   <- ts_parser_set_language parser tree_sitter_PascaLigo

  src <- IO.readFile fin

  idCounter <- newIORef 0

  withCStringLen src \(str, len) -> do
    tree <- ts_parser_parse_string parser nullPtr str len
    finalTree <- withRootNode tree (peek >=> go idCounter)
    return $ Forest 0 [("", finalTree)] (ptRange finalTree)

  where
    nextID :: IORef Int -> IO Int
    nextID ref = do
      modifyIORef' ref (+ 1)
      readIORef ref

    go :: IORef Int -> Node -> IO ParseTree
    go idCounter node = do
      let count = fromIntegral $ nodeChildCount node
      allocaArray count \children -> do
        alloca \tsNodePtr -> do
          poke tsNodePtr $ nodeTSNode node
          ts_node_copy_child_nodes tsNodePtr children
          nodes <- for [0.. count - 1] \i -> do
            peekElemOff children i

          trees <- for nodes \node' -> do
            tree <- go idCounter node'
            field <-
              if nodeFieldName node' == nullPtr
              then return ""
              else peekCString $ nodeFieldName node'
            return (Text.pack field, tree)

          ty <- peekCString $ nodeType node

          TSNode start _ _ _ _ <- peek tsNodePtr

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
                  )

              , rFinish =
                  ( i $ pointRow    finish2D + 1
                  , i $ pointColumn finish2D + 1
                  )
              }

          return $ ParseTree
            { ptID       = treeID
            , ptName     = Text.pack ty
            , ptStart    = fromIntegral start
            , ptFinish   = fromIntegral $ nodeEndByte node
            , ptRange    = range
            , ptChildren = Forest fID trees range
            }