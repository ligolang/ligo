module Util
  ( groupByKey
  , readSemVerQ
  , resourcesFolder
  , rmode'semv
  ) where

import Data.Char qualified as Char
import Data.List (groupBy)
import Data.SemVer qualified as SemVer
import Data.Text qualified as Text
import Debug qualified
import Fmt (build)
import Language.Haskell.TH.Syntax (Code (..), Q, liftTyped)
import System.FilePath ((</>))
import TH.RelativePaths (qReadFileText)
import Text.Interpolation.Nyan.Core (RMode (..))

groupByKey :: Ord k => (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
groupByKey f g =
  extractGroup f g
  . groupBy ((==) `on` f)
  . sortBy (comparing f)

extractGroup :: (a -> k) -> (a -> v) -> [[a]] -> [(k, [v])]
extractGroup _ _ [] = []
extractGroup f g ([] : xs) = extractGroup f g xs
extractGroup f g (ys@(y : _) : xs) = (f y, g <$> ys) : extractGroup f g xs

-- | Read a 'SemVer.Version' from a file at compile time.
--
-- This allows comments in form of lines starting from @#@ sign.
readSemVerQ :: FilePath -> Code Q SemVer.Version
readSemVerQ path = Code do
  content <- qReadFileText path
  let clearContent = content
        & Text.lines
        & filter ((/= "#") . take 1 . toString . Text.dropWhile Char.isSpace)
        & Text.unlines
        & Text.strip

  ver <- SemVer.fromText clearContent
    & either (\_ -> fail $ "Invalid version constant in the file: " <> Debug.show clearContent) pure
  examineCode $ liftTyped ver

resourcesFolder :: FilePath
resourcesFolder = "src" </> "resources"

rmode'semv :: RMode SemVer.Version
rmode'semv = RMode (build . SemVer.toText)
