module Test.Common.Capabilities.DocumentLink
  ( documentLinkBDriver
  , documentLinkCDriver
  , simplifyDocumentLink
  , simplifiedBLinks
  , simplifiedCLinks
  , getContractsDir
  ) where

import Language.LSP.Types (DocumentLink (..), UInt, uriToFilePath)
import Language.LSP.Types.Lens (character, end, line, start)
import System.Directory (canonicalizePath)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import AST.Capabilities.DocumentLink (getDocumentLinks)
import AST.Skeleton (getLIGO)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.Util (ScopeTester, readContractWithScopes)

getContractsDir :: IO FilePath
getContractsDir = canonicalizePath (Common.contractsDir </> "find" </> "includes")

type SimpleDocumentLink = ((UInt, UInt), (UInt, UInt), Maybe FilePath)

simplifyDocumentLink :: DocumentLink -> SimpleDocumentLink
simplifyDocumentLink (DocumentLink _range _uri _ _) =
  ( (_range ^. start . line, _range ^. start . character)
  , (_range ^. end . line, _range ^. end . character)
  , _uri >>= uriToFilePath
  )

simplifiedBLinks :: IO [SimpleDocumentLink]
simplifiedBLinks = do
  contractsDir <- getContractsDir
  pure [ ((0, 0), (1, 0), Just (contractsDir </> "B2/B2.ligo"))
       ]

simplifiedCLinks :: IO [SimpleDocumentLink]
simplifiedCLinks = do
  contractsDir <- getContractsDir
  pure [ ((0, 0), (1, 0), Just (contractsDir </> "C2.mligo"))
       , ((1, 0), (2, 0), Just (contractsDir </> "C3.mligo"))
       ]

documentLinkBDriver :: forall impl. ScopeTester impl => Assertion
documentLinkBDriver = do
  contractsDir <- getContractsDir
  let inputFile = contractsDir </> "B1.ligo"
  tree <- readContractWithScopes @impl inputFile
  symbols <- getDocumentLinks inputFile (getLIGO tree)
  target <- simplifiedBLinks
  fmap simplifyDocumentLink symbols `shouldBe` target

documentLinkCDriver :: forall impl. ScopeTester impl => Assertion
documentLinkCDriver = do
  contractsDir <- getContractsDir
  let inputFile = contractsDir </> "C1.mligo"
  tree <- readContractWithScopes @impl inputFile
  symbols <- getDocumentLinks inputFile (getLIGO tree)
  target <- simplifiedCLinks
  fmap simplifyDocumentLink symbols `shouldBe` target
