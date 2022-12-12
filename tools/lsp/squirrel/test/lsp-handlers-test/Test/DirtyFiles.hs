module Test.DirtyFiles
  ( unit_changed_file_is_dirty
  ) where

import Data.Aeson (Result (..), fromJSON, toJSON)
import Data.String.Interpolate (i)
import Language.LSP.Test (Session, changeDoc, closeDoc, request, sendNotification)
import Language.LSP.Types as LSP
import System.FilePath ((</>))
import Test.HUnit (Assertion)

import Test.Common.Capabilities.Util qualified as Util (contractsDir)
import Test.Common.FixedExpectations (expectationFailure)
import Test.Common.LSP (openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Util.contractsDir </> "dirty"

unit_changed_file_is_dirty :: Assertion
unit_changed_file_is_dirty = runHandlersTest contractsDir do
  -- We just opened the file, so it should be clean.
  doc <- openLigoDoc "dirty.jsligo"
  expectStatus False doc

  -- After changing the file, it should be dirty.
  changeDoc doc
    [ LSP.TextDocumentContentChangeEvent
      (Just $ LSP.Range (LSP.Position 2 19) (LSP.Position 2 24))
      Nothing
      "true"
    ]
  expectStatus True doc

  -- "Save" the file, and expect it to be clean again.
  -- It doesn't actually save it, but sending a notification is good enough.
  let params = LSP.DidSaveTextDocumentParams doc Nothing
  sendNotification LSP.STextDocumentDidSave params
  expectStatus False doc

  closeDoc doc

expectStatus :: HasCallStack => Bool -> LSP.TextDocumentIdentifier -> Session ()
expectStatus expectedDirty doc = do
  let
    dirty :: Bool -> String
    dirty = bool "clean" "dirty"

  ResponseMessage{_result = Right result} <- request (LSP.SCustomMethod "isDirty") (toJSON doc)
  liftIO case fromJSON result of
    Error err -> expectationFailure err
    Success actualDirty
      | expectedDirty == actualDirty ->
        pass
      | otherwise ->
        expectationFailure [i|Expected file to be marked #{dirty expectedDirty}, but it's #{dirty actualDirty}.|]
