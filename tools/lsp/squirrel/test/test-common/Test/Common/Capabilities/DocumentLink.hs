module Test.Common.Capabilities.DocumentLink
  ( documentLinkBDriver
  , documentLinkCDriver
  , simplifyDocumentLink
  , simplifiedBLinks
  , simplifiedCLinks
  , contractsDir
  ) where

import Control.Lens ((^.))
import Language.LSP.Types (DocumentLink (..), uriToFilePath)
import Language.LSP.Types.Lens (character, end, line, start)
import System.FilePath ((</>))
import Util (removeDots)

import Test.HUnit (Assertion)

import AST.Capabilities.DocumentLink (getDocumentLinks)
import AST.Scope (HasScopeForest)
import AST.Skeleton (getLIGO)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.Util (readContractWithScopes)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "find" </> "includes"

type SimpleDocumentLink = ((Int, Int), (Int, Int), Maybe FilePath)

simplifyDocumentLink :: DocumentLink -> SimpleDocumentLink
simplifyDocumentLink (DocumentLink _range _uri _ _) =
  ( (_range ^. start . line, _range ^. start . character)
  , (_range ^. end . line, _range ^. end . character)
  , _uri >>= uriToFilePath
  )

simplifiedBLinks :: [SimpleDocumentLink]
simplifiedBLinks =
  [ ((0, 0), (1, 0), Just (removeDots $ contractsDir </> "B2/B2.ligo"))
  ]

simplifiedCLinks :: [SimpleDocumentLink]
simplifiedCLinks =
  [ ((0, 0), (1, 0), Just (removeDots $ contractsDir </> "C2.religo"))
  , ((1, 0), (2, 0), Just (removeDots $ contractsDir </> "C3.mligo"))
  ]

documentLinkBDriver :: forall impl. HasScopeForest impl IO => Assertion
documentLinkBDriver = do
  let inputFile = contractsDir </> "B1.ligo"
  tree <- readContractWithScopes @impl inputFile
  let symbols = getDocumentLinks inputFile (getLIGO tree)
  fmap simplifyDocumentLink symbols `shouldBe` simplifiedBLinks

documentLinkCDriver :: forall impl. HasScopeForest impl IO => Assertion
documentLinkCDriver = do
  let inputFile = contractsDir </> "C1.mligo"
  tree <- readContractWithScopes @impl inputFile
  let symbols = getDocumentLinks inputFile (getLIGO tree)
  fmap simplifyDocumentLink symbols `shouldBe` simplifiedCLinks
