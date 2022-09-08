module Test.Capabilities.Highlights
  ( unit_highlights
  ) where

import Language.LSP.Test
import Language.LSP.Types
  ( Range (..), List (..), Position (..)
  , DocumentHighlight (..), DocumentHighlightKind (..)
  )
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldMatchList)
import Test.Common.LSP (openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "find"

unit_highlights :: Assertion
unit_highlights = do
  let filename = "name-shadowing.mligo"
      -- TODO: This variables are commented to avoid warnings because used only in commented tests below
      -- commit_type_decl     = Range (Position 0 5) (Position 0 11)
      -- commit_type_usage    = [ Range (Position 2 18) (Position 2 24)
      --                        , Range (Position 2 28) (Position 2 34)
      --                        ]
      -- commit_param_decl    = Range (Position 2 9) (Position 2 15)
      -- commit_param_usage   = [ Range (Position 3 15) (Position 3 21)
      --                        ]
      commit1_local_decl   = Range (Position 3 6) (Position 3 12)
      commit1_local_usage  = [ Range (Position 4 15) (Position 4 21)
                             ]
      commit2_local_decl   = Range (Position 4 6) (Position 4 12)
      commit2_local_usage  = [ Range (Position 5 2) (Position 5 8)
                             ]


  -- TODO: This should be fixed in LIGO-729
  -- Highlight "commit" type
  -- List highlight1 <- runHandlersTest contractsDir do
  --   doc <- openLigoDoc filename
  --   getHighlights doc (_start commit_type_decl)

  -- highlight1 `shouldMatchList` fmap (`DocumentHighlight` Just HkRead)
  --   (commit_type_decl : commit_type_usage)

  -- TODO: This should be fixed in LIGO-729
  -- Highlight "commit" parameter
  -- List highlight2 <- runHandlersTest contractsDir do
  --   doc <- openLigoDoc filename
  --   getHighlights doc (_start commit_param_decl)

  -- highlight2 `shouldMatchList` fmap (`DocumentHighlight` Just HkRead)
  --   (commit_param_decl : commit_param_usage)

  -- Highlight "commit" first local variable (when cursor on declaration)
  List highlight3 <- runHandlersTest contractsDir do
    doc <- openLigoDoc filename
    getHighlights doc (_start commit1_local_decl)

  highlight3 `shouldMatchList` fmap (`DocumentHighlight` Just HkRead)
    (commit1_local_decl : commit1_local_usage)

  -- Highlight "commit" second local variable (when cursor on declaration)
  List highlight4 <- runHandlersTest contractsDir do
    doc <- openLigoDoc filename
    getHighlights doc (_start commit2_local_decl)

  highlight4 `shouldMatchList` fmap (`DocumentHighlight` Just HkRead)
    (commit2_local_decl : commit2_local_usage)

  -- TODO: This should be fixed in LIGO-700
  -- Highlight "commit" first local variable (when cursor on usage)
  -- List highlight5 <- runHandlersTest contractsDir do
  --   doc <- openLigoDoc filename
  --   getHighlights doc (_start $ head commit1_local_usage)

  -- highlight5 `shouldMatchList` fmap (`DocumentHighlight` Just HkRead)
  --   (commit1_local_decl : commit2_local_usage)
