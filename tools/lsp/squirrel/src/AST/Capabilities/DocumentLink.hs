module AST.Capabilities.DocumentLink
  ( getDocumentLinks
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import System.FilePath (takeDirectory, (</>))

import Duplo (collect, match)
import Language.LSP.Types qualified as J (DocumentLink (..), Uri, filePathToUri)

import AST.Includes (getMarkerInfos, getMarkers)
import AST.Scope.Common (MarkerInfo (..))
import AST.Skeleton (Binding (..), Constant (..), LIGO)
import Parser (LineMarker (..), LineMarkerType (..))
import Product (Contains)
import Range (Range (..), getRange, toLspRange)
import Util (removeDots)

getDocumentLinks
  :: forall xs. (Contains Range xs, Contains [LineMarker] xs)
  => FilePath
  -> LIGO xs
  -> [J.DocumentLink]
getDocumentLinks source ligo =
  getUnprocessedDocumentLinks source ligo
  ++ getPreprocessedDocumentLinks source ligo

getUnprocessedDocumentLinks
  :: forall xs. Contains Range xs
  => FilePath
  -> LIGO xs
  -> [J.DocumentLink]
getUnprocessedDocumentLinks source ligo =
  mapMaybe (processBinding . snd) $ collect ligo
  where
    processBinding :: Binding (LIGO xs) -> Maybe J.DocumentLink
    processBinding = \case
      -- TODO (LIGO-204): Add a case match for BInclude as well.
      BInclude (match @Constant -> Just (r, String n))
        -> let uri = toJUri n
               range = toLspRange $ fillLine $ getRange r
               fillLine (Range (rsl, _, _) _ f) =
                 Range (rsl, 1, 0) (rsl+1, 1, 0) f
            in Just (J.DocumentLink range (Just uri) Nothing Nothing)
      _ -> Nothing

    toJUri :: T.Text -> J.Uri
    toJUri = J.filePathToUri . withPwd . T.unpack . stripQuotes

    withPwd :: FilePath -> FilePath
    withPwd = removeDots . (pwd </>)

    pwd :: FilePath
    pwd = takeDirectory source

    stripQuotes :: T.Text -> T.Text
    stripQuotes = T.tail . T.init

getPreprocessedDocumentLinks
  :: forall xs. Contains [LineMarker] xs
  => FilePath
  -> LIGO xs
  -> [J.DocumentLink]
getPreprocessedDocumentLinks source ligo = documentLinks
  where
    includes = flip filter markerInfos $ \mi ->
      lmFlag (miMarker mi) == IncludedFile
      && miDepth mi == 1
    returns = flip filter markerInfos $ \mi ->
      lmFlag (miMarker mi) == ReturnToFile
      && miDepth mi == 0

    documentLinks :: [J.DocumentLink]
    documentLinks = zipWith mkLink includes returns
      where
        mkLink inc ret = J.DocumentLink range (Just uri) Nothing Nothing
          where
            range = toLspRange (Range (line, 1, 0) (line + 1, 1, 0) source)
            line = lmLine (miMarker ret) - 1

            uri = J.filePathToUri . removeDots . lmFile . miMarker $ inc

    markers = getMarkers ligo
    markerInfos = IntMap.elems $ fst $ getMarkerInfos True source markers
