module Test.Project.Driver
  ( unit_simple
  , unit_two_project_files
  , unit_two_projects
  , unit_backward_include
  , unit_no_edge
  , unit_ignore_paths
  , unit_ignore_included
  , unit_can_delete_file
-- TODO: See https://gitlab.com/ligolang/ligo/-/issues/1594
--  , unit_invalidate_transitive_imports
  ) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.Class qualified as G
import Data.Aeson (Result (Success), Value (Null), fromJSON)
import Language.LSP.Test qualified as LSP
import Language.LSP.Types qualified as LSP
import Language.LSP.Types.Lens qualified as LSP
import System.Directory (canonicalizePath, removeFile)
import System.FilePath ((</>))
import System.IO (writeFile)
import UnliftIO qualified

import AST (Includes (..))

import Test.HUnit (Assertion)

import Language.LSP.Test (Session, getHover, getReferences)
import Language.LSP.Types (Position (..), TextDocumentIdentifier)
import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.LSP (openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "projects"

runHandlersTestCanonicalized :: FilePath -> Session a -> IO a
runHandlersTestCanonicalized rootDirectory session = do
  projectPath <- canonicalizePath $ contractsDir </> rootDirectory
  runHandlersTest projectPath session

getBuildGraph :: LSP.Session (AM.AdjacencyMap FilePath)
getBuildGraph = do
  LSP.ResponseMessage {LSP._result = Right p} <- LSP.request (LSP.SCustomMethod "buildGraph") Null
  let Success (Includes actualGraph) = fromJSON p
  return actualGraph

checkBuildGraph :: HasCallStack => FilePath -> FilePath -> Includes FilePath -> Session TextDocumentIdentifier
checkBuildGraph rootDirectory docName (Includes expectedBuildGraph) = do
  projectPath <- liftIO $ canonicalizePath $ contractsDir </> rootDirectory
  let buildGraph = Includes $ AM.gmap (projectPath </>) expectedBuildGraph
  checkBuildGraph' docName buildGraph

checkBuildGraph' :: HasCallStack => FilePath -> Includes FilePath -> Session TextDocumentIdentifier
checkBuildGraph' docName (Includes expectedGraph) = do
  doc <- openLigoDoc docName
  actualGraph <- getBuildGraph
  liftIO $ actualGraph `shouldBe` expectedGraph
  pure doc

unit_simple :: Assertion
unit_simple =
  runHandlersTestCanonicalized "simple" $
    void $ checkBuildGraph "simple" "a.mligo" $ G.edge "a.mligo" "b.mligo"

unit_two_project_files :: Assertion
unit_two_project_files = do
  let root = "two_project_files"
  -- From root we need to see both.
  runHandlersTestCanonicalized root $
    void $ checkBuildGraph root "a.mligo" $ G.edge "a.mligo" ("inner" </> "b.mligo")

  let root' = root </> "inner"
  -- From inner we need to see only the inner one.
  runHandlersTestCanonicalized root' $
    void $ checkBuildGraph root' "b.mligo" $ G.vertex "b.mligo"

-- We update indexing settings to use the last active file
unit_two_projects :: Assertion
unit_two_projects = do
  projectPathAbsolute <- canonicalizePath $ contractsDir </> "two_projects"
  let
      checkGraph :: Includes FilePath -> LSP.Session ()
      checkGraph (Includes expectedRelative) = do
        let expectedAbsolute = AM.gmap (projectPathAbsolute </>) expectedRelative
        actual <- getBuildGraph
        liftIO $ actual `shouldBe` expectedAbsolute
  runHandlersTest projectPathAbsolute $ do
    aDoc <- openLigoDoc $ "a" </> "a.jsligo"
    checkGraph $ G.vertex $ "a" </> "a.jsligo"
    bDoc <- openLigoDoc $ "b" </> "b.jsligo"
    checkGraph $ G.vertex $ "b" </> "b.jsligo"
    _ <- getHover aDoc (Position 0 7)
    checkGraph $ G.vertex $ "a" </> "a.jsligo"
    _ <- getHover bDoc (Position 0 7)
    checkGraph $ G.vertex $ "b" </> "b.jsligo"

unit_backward_include :: Assertion
unit_backward_include = do
  root <- canonicalizePath $ contractsDir </> "backward_include"
  runHandlersTest (root </> "inner") $
    void $ checkBuildGraph' "b.mligo"
      $ G.edge (root </> "inner" </> "b.mligo") (root </> "a.mligo")

unit_no_edge :: Assertion
unit_no_edge =
  runHandlersTestCanonicalized "no_edge" $
    void $ checkBuildGraph "no_edge" "a.mligo" $ G.vertices ["a.mligo", "b.mligo"]

unit_ignore_paths :: Assertion
unit_ignore_paths =
  runHandlersTestCanonicalized "ignore_directories" $
    void $ checkBuildGraph "ignore_directories" "include_me.mligo" $
      G.edge "include_me.mligo" ("dir" </> "include_me.mligo")

unit_ignore_included :: Assertion
unit_ignore_included =
  runHandlersTestCanonicalized "ignore_included" $
    void $ checkBuildGraph "ignore_included" "include_me.jsligo" $ G.vertex "include_me.jsligo"

unit_can_delete_file :: Assertion
unit_can_delete_file = do
  let included = "a.mligo"
  let includer = "b.mligo"
  let root = "delete_file"
  directory <- canonicalizePath $ contractsDir </> root
  UnliftIO.finally
    (runHandlersTest directory do
      -- Sanity check: check that the edge is correct.
      includerDoc <- checkBuildGraph root includer $ G.edge includer included

      let
        checkReferences numRefs = do
          LSP.List refs <- getReferences includerDoc (Position 1 8) True
          liftIO $ length refs `shouldBe` numRefs

      -- Another sanity check: Do we have references?
      checkReferences 2

      -- Change the included file: did we update the build graph?
      -- Note that lsp-test doesn't implement file watch protocol... so for now,
      -- we send such notification manually.
      liftIO $ removeFile $ directory </> included
      LSP.sendNotification LSP.SWorkspaceDidChangeWatchedFiles LSP.DidChangeWatchedFilesParams
        { _changes = LSP.List
          [ LSP.FileEvent
            { _uri = LSP.filePathToUri $ directory </> included
            , _xtype = LSP.FcDeleted
            }
          ]
        }
      void $ checkBuildGraph root includer $ G.vertex includer

      -- What about the references?
      checkReferences 0)
    (
      -- Restore the file after the test runs.
      writeFile (directory </> included) "let a = \"a\"\n")

_unit_invalidate_transitive_imports :: Assertion
_unit_invalidate_transitive_imports = do
  let
    fileA = "a.mligo"
    fileB = "b.mligo"
    fileC = "c.mligo"
    root = "transitive"
  runHandlersTestCanonicalized root do
    docB <- checkBuildGraph root fileB $ G.overlay (G.vertex fileA) (G.edge fileC fileB)

    let changeRange = LSP.Range (LSP.Position 0 0) (LSP.Position 1 0)
    LSP.changeDoc docB
      [ LSP.TextDocumentContentChangeEvent
        (Just changeRange)
        Nothing
        "#include \"a.mligo\"\n"
      ]

    -- Sanity check:
    contents <- LSP.documentContents docB
    liftIO $ contents `shouldBe` "#include \"a.mligo\"\n"

    -- lsp-test doesn't implement the file watch protocol
    LSP.sendNotification LSP.SWorkspaceDidChangeWatchedFiles LSP.DidChangeWatchedFilesParams
      { _changes = LSP.List
        [ LSP.FileEvent
          { _uri = docB ^. LSP.uri
          , _xtype = LSP.FcChanged
          }
        ]
      }

    docC <- checkBuildGraph root fileC $ G.path [fileC, fileB, fileA]
    LSP.List refs <- getReferences docC (Position 1 8) True
    liftIO $ length refs `shouldBe` 2
