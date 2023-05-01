{-# LANGUAGE OverloadedLists #-}

module Language.LIGO.AST.Includes
  ( insertPreprocessorRanges
  , extractIncludes
  ) where
import Prelude hiding (Product)

import Control.Lens (to, (+~), (-~))
import Control.Monad.RWS.Strict (RWS, RWST, execRWS, execRWST, tell)
import Data.DList (DList)
import Data.DList qualified as DList (toList)
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Duplo.Tree (Cofree ((:<)))
import System.FilePath (takeDirectory, (</>))
import Text.Regex.TDFA (Regex, getAllTextMatches, makeRegexM, match)
import UnliftIO.Directory (canonicalizePath)
import Witherable (imapMaybe)

import Language.LIGO.AST.Common (ContractInfo, ParsedContractInfo, pattern FindContract)
import Language.LIGO.AST.Skeleton (LIGO, SomeLIGO (..))
import Language.LIGO.Diagnostic (Message (..))
import Language.LIGO.ParseTree (Source (..))
import Language.LIGO.Parser
  (Info, LineMarker (..), LineMarkerType (..), ParsedInfo, parseLineMarkerText)
import Language.LIGO.Product (Contains, Product (..), getElem, modElem, putElem)
import Language.LIGO.Range
  (LigoPosition (LigoPosition), PreprocessedRange (..), Range (..), finishLine, getRange, lpLine,
  rFile, rFinish, rStart, rangeLines, startLine)

data ExtractionDepth
  = DirectInclusions
  -- ^ Extract only file names that were directly included by a file.
  | AllInclusions
  -- ^ Extract all dependencies of a file.

-- | An auxiliary data type that containts not only a 'LineMarker', but also
-- some metadata regarding that line marker.
data MarkerInfo = MarkerInfo
  { miMarker    :: LineMarker
    -- ^ The associated line marker to its metadata.
  , miLastRange :: Range
    -- ^ The range of the closest look-behind line marker that contains an
    -- included file whose depth is 1. This is used to "compress" ranges of
    -- included files into the range that is provided by that line marker.
  , miDepth     :: Int
    -- ^ The non-negative depth of inclusions. A zero depth indicates that a
    -- file is a direct dependency, while a positive depth indicates that a file
    -- is an indirect dependency (i.e., it was included by an included file).
  } deriving stock (Show)

fromOriginalPoint :: Product Info -> Product ParsedInfo
fromOriginalPoint infos = PreprocessedRange (getRange infos) :> infos

insertPreprocessorRanges :: MonadIO m => ContractInfo -> m ParsedContractInfo
insertPreprocessorRanges = fmap fst . extractIncludedFiles AllInclusions

loopM_ :: (Applicative t, Foldable f) => (Cofree f a -> t ()) -> (Cofree f a -> t ())
loopM_ go = aux
  where
    aux (r :< fs) = Foldable.for_ fs aux *> go (r :< fs)

loopM
  :: (Monad m, Traversable f)
  => (Cofree f a -> m (Cofree f a)) -> (Cofree f a -> m (Cofree f a))
loopM go = aux
  where
    aux (r :< fs) = go . (r :<) =<< traverse aux fs

getMarkers :: forall xs. Contains [LineMarker] xs => LIGO xs -> [LineMarker]
getMarkers ligo = DList.toList $ snd $ execRWS (loopM_ collectMarkers ligo) () ()
  where
    collectMarkers :: LIGO xs -> RWS () (DList LineMarker) () ()
    collectMarkers (info :< _) = for_ (getElem @[LineMarker] info) (tell . pure)

-- | Extracts all file paths mentioned in line markers.
extractIncludes :: forall m. MonadFail m => Text -> m [LineMarker]
extractIncludes contents = do
  regex :: Regex <- makeRegexM source
  let
    matches = getAllTextMatches . match regex <$> lines contents
    markers = imapMaybe getFileName matches
  pure markers
  where
    source :: Text
    source = "^# [0-9]+ \".+\"( 1| 2)?$"

    getFileName :: Int -> [Text] -> Maybe LineMarker
    getFileName (fromIntegral -> l) [parseLineMarkerText -> Just (fp, ty, f)] =
      Just $ LineMarker fp ty f $ Range (LigoPosition l 0) (LigoPosition (l + 1) 0) fp
    getFileName _ _ = Nothing

getMarkerInfos
  :: MonadIO m
  => ExtractionDepth
  -> FilePath
  -> [LineMarker]
  -> m (IntMap MarkerInfo, DList (FilePath, FilePath))
getMarkerInfos directIncludes pwd markers =
  execRWST (collectMarkerInfos directIncludes pwd markers) () mempty

-- | Gets the amount of lines between the "new" line after preprocessing and the
-- "old" line before preprocessing.
rangeOffset :: LineMarker -> Int
rangeOffset LineMarker{lmLine, lmLoc} = lmLoc ^. finishLine - lmLine

collectMarkerInfos
  :: MonadIO m
  => ExtractionDepth
  -> FilePath
  -> [LineMarker]
  -> RWST () (DList (FilePath, FilePath)) (IntMap MarkerInfo) m ()
collectMarkerInfos directIncludes pwd markers =
  -- For the actual ranges, we pretend everything is fused into the line
  -- where the include would be located.
  -- For the preprocessed ranges, we use the line markers to map the range
  -- to the file and line they represent.
  for_ (sortOn (view startLine . lmLoc) markers) \lm@(LineMarker next f _ r) -> do
    let line = r ^. startLine . to fromIntegral
    n <- withPwd pwd next
    case f of
      RootFile     -> modify $ IntMap.insert line $ MarkerInfo lm r 0
      IncludedFile -> whenJustM (gets (IntMap.lookupLT line)) $
        \(_, MarkerInfo prev lr d) -> do
          modify $ IntMap.insert
            line
            (MarkerInfo lm (bool lr (r & startLine -~ rangeOffset prev) $ d == 0) (d + 1))
          when (shouldExtractInclusion directIncludes d) do
            p <- withPwd pwd (lmFile prev)
            tell [(p, n)]
      ReturnToFile -> modify $ IntMap.lookupLT line >>= \case
        Nothing -> id
        Just (_, MarkerInfo _ lr d) -> IntMap.insert line $ MarkerInfo lm lr (d - 1)
  where
    shouldExtractInclusion :: ExtractionDepth -> Int -> Bool
    shouldExtractInclusion AllInclusions    _ = True
    shouldExtractInclusion DirectInclusions d = d == 0

withPwd :: MonadIO m => FilePath -> FilePath -> m FilePath
withPwd pwd = canonicalizePath . (pwd </>)

-- | Given some contract A, returns a new version of that contract such that:
-- * When a line marker is found, all 'Range's will be adjusted in a manner such
--   that everything in the current document will stay as it is, but ranges from
--   included parts will be "squashed" to the 'Range' where the inclusion was
--   made. In other words, the ranges will be restored to the 'Range's that the
--   user sees.
-- * Additionally, 'PreprocessedRange's will be included, and they will have the
--   actual 'Range's of the files. That is, if A includes B then the ranges will
--   actually point to B. These are the actual ranges which are mapped according
--   to the line markers present in the document.
--
-- Also returns a difference list containing the edges of how to include files.
-- That is, if A includes B, then this list will contain a tuple (A, B).
extractIncludedFiles
  :: forall m. MonadIO m
  => ExtractionDepth  -- ^ Whether to only extract directly included files or all of them.
  -> ContractInfo  -- ^ The contract to scan for includes.
  -> m (ParsedContractInfo, DList (FilePath, FilePath))
extractIncludedFiles directIncludes (FindContract file (SomeLIGO dialect ligo) msgs) = do
  let markers = getMarkers ligo
  (markerInfos, edges) <- getMarkerInfos directIncludes pwd markers
  (info :< tree) <- loopM (go markerInfos) $ fromOriginalPoint <$> ligo
  let
    -- We still want RawContract to start at line 1:
    ligo' = bool (modElem (startLine +~ 1) info :< tree) (info :< tree) (IntMap.null markerInfos)
    msgs' = map (\msg -> msg {mRange = adjustRange markerInfos (mRange msg)}) msgs
  pure (FindContract file (SomeLIGO dialect ligo') msgs', edges)
  where
    pwd :: FilePath
    pwd = takeDirectory $ srcPath file

    go :: IntMap MarkerInfo -> LIGO ParsedInfo -> m (LIGO ParsedInfo)
    go markers (info :< tree) = do
      info' <- info
            &  adjustPreprocessedRange markers
           <&> modElem (adjustRange markers)
      pure (info' :< tree)

    adjustPreprocessedRange :: IntMap MarkerInfo -> Product ParsedInfo -> m (Product ParsedInfo)
    adjustPreprocessedRange markers i = case prev of
      Nothing -> pure i
      Just (_, MarkerInfo marker _ _) -> do
        normalized <- withPwd pwd (lmFile marker)
        let preRange = range
              & rangeLines -~ rangeOffset marker
              & rFile .~ normalized
        pure (putElem (PreprocessedRange preRange) i)
      where
        prev = IntMap.lookupLE (range ^. startLine . to fromIntegral) markers
        range = getRange i

    adjustSide :: Lens' Range LigoPosition
               -> IntMap MarkerInfo
               -> Range
               -> Range
    adjustSide side markers range = case prev of
      Nothing -> range
      Just (_, MarkerInfo marker lastRange depth) ->
        -- In case we are at depth 0, we need to subtract the accumulated range
        -- at both the start and finish, since we may see new includes while
        -- traversing the tree.
        let newRange = range &
              if depth > 0 then
                -- "Compress" the tree into the range provided by the latest
                -- line marker generated by a direct inclusion.
                side .~ lastRange ^. rStart
              else
                -- Adjust the range so it reflects its range before
                -- preprocessing.
                (side . lpLine) -~ rangeOffset marker
         in newRange
      where
        prev = IntMap.lookupLE (range ^. side . lpLine . to fromIntegral) markers

    adjustRange :: IntMap MarkerInfo -> Range -> Range
    adjustRange markers = adjustSide rFinish markers
                        . adjustSide rStart markers
