module AST.Capabilities.DocumentLink
  ( getDocumentLinks
  ) where

import Control.Monad (zipWithM)
import Control.Monad.IO.Class (MonadIO)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import System.FilePath (takeDirectory, (</>))
import UnliftIO.Directory (canonicalizePath)

import Duplo (collect, match)
import Language.LSP.Types qualified as J (DocumentLink (..), Uri, filePathToUri)

import AST.Includes
  ( ExtractionDepth (DirectInclusions), MarkerInfo (..), getMarkerInfos, getMarkers
  )
import AST.Skeleton (Binding (..), Constant (..), LIGO)
import Parser (LineMarker (..), LineMarkerType (..))
import Product (Contains)
import Range (Range (..), getRange, toLspRange)

getDocumentLinks
  :: forall xs m. (Contains Range xs, Contains [LineMarker] xs, MonadIO m)
  => FilePath
  -> LIGO xs
  -> m [J.DocumentLink]
getDocumentLinks source ligo = do
  (++) <$> getUnprocessedDocumentLinks source ligo
       <*> getPreprocessedDocumentLinks source ligo

getUnprocessedDocumentLinks
  :: forall xs m. (Contains Range xs, MonadIO m)
  => FilePath
  -> LIGO xs
  -> m [J.DocumentLink]
getUnprocessedDocumentLinks source ligo =
  fmap catMaybes $ mapM (processBinding . snd) $ collect ligo
  where
    processBinding :: Binding (LIGO xs) -> m (Maybe J.DocumentLink)
    processBinding = \case
      -- TODO (LIGO-204): Add a case match for BInclude as well.
      BInclude (match @Constant -> Just (r, CString n))
        -> do uri <- toJUri n
              let range = toLspRange $ fillLine $ getRange r
                  fillLine (Range (rsl, _, _) _ f) =
                    Range (rsl, 1, 0) (rsl+1, 1, 0) f
              pure (Just (J.DocumentLink range (Just uri) Nothing Nothing))
      _ -> pure Nothing

    toJUri :: T.Text -> m J.Uri
    toJUri = fmap J.filePathToUri . withPwd . T.unpack . stripQuotes

    withPwd :: FilePath -> m FilePath
    withPwd = canonicalizePath . (pwd </>)

    pwd :: FilePath
    pwd = takeDirectory source

    stripQuotes :: T.Text -> T.Text
    stripQuotes = T.tail . T.init

getPreprocessedDocumentLinks
  :: forall xs m. (Contains [LineMarker] xs, MonadIO m)
  => FilePath
  -> LIGO xs
  -> m [J.DocumentLink]
getPreprocessedDocumentLinks source ligo = do
  let markers = getMarkers ligo
  markerInfos <- IntMap.elems . fst <$> getMarkerInfos DirectInclusions source markers
  let includes = flip filter markerInfos $ \mi ->
        lmFlag (miMarker mi) == IncludedFile
        && miDepth mi == 1
  let returns = flip filter markerInfos $ \mi ->
        lmFlag (miMarker mi) == ReturnToFile
        && miDepth mi == 0

  zipWithM mkLink includes returns
    where
      mkLink :: MarkerInfo -> MarkerInfo -> m J.DocumentLink
      mkLink inc ret = do
        let range = toLspRange (Range (line, 1, 0) (line + 1, 1, 0) source)
            line = lmLine (miMarker ret) - 1
        uri <- fmap J.filePathToUri . canonicalizePath . lmFile . miMarker $ inc
        pure (J.DocumentLink range (Just uri) Nothing Nothing)
