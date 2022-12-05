module Util
  ( groupByKey
  , readSemVerQ
  , resourcesFolder
  , rmode'semv
  , rmode'ansi
  , everywhereM'
  ) where

import Data.Char qualified as Char
import Data.List (groupBy, singleton)
import Data.SemVer qualified as SemVer
import Data.Text qualified as Text
import Debug qualified
import Fmt (build)
import Generics.SYB (Data (gmapM), GenericM)
import Language.Haskell.TH.Syntax (Code (..), Q, liftTyped)
import System.Console.ANSI (SGR, setSGRCode)
import System.FilePath ((</>))
import TH.RelativePaths (qReadFileText)
import Text.Interpolation.Nyan.Core (RMode (..))

groupByKey :: Ord k => (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
groupByKey f g =
  extractGroup f g
  . groupBy ((==) `on` f)
  . sortOn f

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
        & lines
        & filter ((/= "#") . take 1 . toString . Text.dropWhile Char.isSpace)
        & unlines
        & Text.strip

  version <-
    SemVer.fromText clearContent
    & either (\_ -> fail $ "Invalid version constant in the file: " <> Debug.show clearContent) pure

  examineCode . liftTyped $ version

resourcesFolder :: FilePath
resourcesFolder = "src" </> "resources"

rmode'semv :: RMode SemVer.Version
rmode'semv = RMode (build . SemVer.toText)

rmode'ansi :: RMode [SGR]
rmode'ansi = RMode (build . concatMap (setSGRCode . singleton))

-- | Monadic variation on everywhere'
everywhereM' :: forall m. Monad m => GenericM m -> GenericM m
everywhereM' f = go
  where
    -- Up-bottom order is also reflected in order of do-actions
    go :: GenericM m
    go x = do
      x' <- f x
      gmapM go x'
