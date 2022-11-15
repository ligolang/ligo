module Test.Capabilities.Highlights
  ( unit_highlights
  ) where

import Language.LSP.Test
import Language.LSP.Types
  (DocumentHighlight (..), DocumentHighlightKind (..), List (..), Position (..), Range (..))
import System.FilePath ((</>))
import Unsafe qualified

import Test.HUnit (Assertion)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldMatchList)
import Test.Common.LSP (openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "find"

unit_highlights :: Assertion
unit_highlights = runHandlersTest contractsDir do
  doc <- openLigoDoc "name-shadowing.mligo"
  let
    commit_type_decl = Range (Position 0 5) (Position 0 11)
    commit_type_usage =
      [ Range (Position 2 18) (Position 2 24)
      , Range (Position 2 28) (Position 2 34)
      ]
    commit_param_decl = Range (Position 2 9) (Position 2 15)
    commit_param_usage =
      [ Range (Position 3 15) (Position 3 21)
      ]
    commit1_local_decl = Range (Position 3 6) (Position 3 12)
    commit1_local_usage =
      [ Range (Position 4 15) (Position 4 21)
      ]
    commit2_local_decl = Range (Position 4 6) (Position 4 12)
    commit2_local_usage =
      [ Range (Position 5 2) (Position 5 8)
      ]

    highlightTest :: HasCallStack => Range -> [Range] -> Range -> Session ()
    highlightTest decl usage cursor = do
      List highlight <- getHighlights doc $ _start cursor
      liftIO $ highlight `shouldMatchList` map (`DocumentHighlight` Just HkRead)
        (decl : usage)

  -- Highlight "commit" type
  highlightTest commit_type_decl commit_type_usage commit_type_decl
  -- Highlight "commit" parameter
  highlightTest commit_param_decl commit_param_usage commit_param_decl
  -- Highlight "commit" first local variable (when cursor on declaration)
  highlightTest commit1_local_decl commit1_local_usage commit1_local_decl
  -- Highlight "commit" second local variable (when cursor on declaration)
  highlightTest commit2_local_decl commit2_local_usage commit2_local_decl
  -- Highlight "commit" first local variable (when cursor on usage)
  highlightTest commit1_local_decl commit1_local_usage (Unsafe.head commit1_local_usage)
