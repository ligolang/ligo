module Test.Project.Driver
  ( unit_simple
  , unit_two_project_files
  , unit_backward_include
  , unit_no_edge
  , unit_ignore_paths
  , unit_ignore_included
  ) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.Class qualified as G
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result (Success), Value (Null), fromJSON)
import GHC.Stack (HasCallStack)
import Language.LSP.Test qualified as LSP
import Language.LSP.Types qualified as LSP
import System.Directory (canonicalizePath)
import System.FilePath ((</>))

import AST (Includes (..))

import Test.HUnit (Assertion)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.LSP (openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "projects"

checkBuildGraph :: HasCallStack => FilePath -> FilePath -> Includes FilePath -> Assertion
checkBuildGraph rootDirectory docName (Includes expectedBuildGraph) = do
  projectPath <- canonicalizePath $ contractsDir </> rootDirectory
  let buildGraph = Includes $ AM.gmap (projectPath </>) expectedBuildGraph
  checkBuildGraph' projectPath docName buildGraph

checkBuildGraph' :: HasCallStack => FilePath -> FilePath -> Includes FilePath -> Assertion
checkBuildGraph' rootDirectory docName (Includes expectedGraph) =
  runHandlersTest rootDirectory do
    _doc <- openLigoDoc docName
    LSP.ResponseMessage {LSP._result = Right p} <- LSP.request (LSP.SCustomMethod "buildGraph") Null
    let Success (Includes actualGraph) = fromJSON p
    liftIO $ actualGraph `shouldBe` expectedGraph

unit_simple :: Assertion
unit_simple = checkBuildGraph "simple" "a.mligo" $ G.edge "a.mligo" "b.mligo"

unit_two_project_files :: Assertion
unit_two_project_files = do
  let root = "two_project_files"
  -- From root we need to see both.
  checkBuildGraph root "a.mligo" $ G.edge "a.mligo" ("inner" </> "b.mligo")
  -- From inner we need to see only the inner one.
  checkBuildGraph (root </> "inner") "b.mligo" $ G.vertex "b.mligo"

unit_backward_include :: Assertion
unit_backward_include = do
  root <- canonicalizePath $ contractsDir </> "backward_include"
  checkBuildGraph' (root </> "inner") "b.mligo"
    $ G.edge (root </> "inner" </> "b.mligo") (root </> "a.mligo")

unit_no_edge :: Assertion
unit_no_edge = checkBuildGraph "no_edge" "a.mligo" $ G.vertices ["a.mligo", "b.mligo"]

unit_ignore_paths :: Assertion
unit_ignore_paths =
  checkBuildGraph "ignore_directories" "include_me.mligo" $
    G.edge "include_me.mligo" ("dir" </> "include_me.mligo")

unit_ignore_included :: Assertion
unit_ignore_included =
  checkBuildGraph "ignore_included" "include_me.ligo" $ G.vertex "include_me.ligo"
