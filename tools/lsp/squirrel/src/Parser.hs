
module Parser where

import Control.Arrow
import Control.Monad.Catch
import Control.Monad.RWS hiding (Product)
import Control.Monad.Trans.Maybe

import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text

import Duplo.Tree
import Duplo.Error
import Duplo.Pretty

import ParseTree
import Range
import Product

import Debug.Trace

{-
  Comment grabber has 2 buffers: 1 and 2.

  1) We collect all comments before the node and append them into buffer 1.
  2) We collect all comments after the node and put them into buffer 2.
  3) `grabComments` takes all comments from buffer 1.
  4) On leaving, move move comments from 2 to 1.
-}

runParserM :: ParserM a -> IO (a, [Err Text ()])
runParserM p = (\(a, _, errs) -> (a, errs)) <$> runRWST p () ([], [])

runParserM1 :: [RawTree] -> ParserM1 a -> ParserM (Maybe a)
runParserM1 cs p = do
  s <- get
  (a, s1, w) <- lift $ runRWST (runMaybeT p) cs s
  tell w
  put s1
  return a

type ParserM  =         RWST ()        [Err Text ()] ([Text], [Text]) IO
type ParserM1 = MaybeT (RWST [RawTree] [Err Text ()] ([Text], [Text]) IO)

data Failure = Failure String
  deriving stock (Show)
  deriving anyclass (Exception)

instance Scoped (Product [Range, Text]) ParserM RawTree ParseTree where
  enter (_ :> _ :> _) (ParseTree ty cs s) = do
    let (comms, rest) = allComments cs
    let (comms1, _)   = allComments $ reverse rest
    modify $ first  (++ comms)
    modify $ second (++ reverse comms1)

    let errs = allErrors   cs
    tell $ fmap Err errs

  leave _ _ = do
    modify \(x, y) -> (y, [])

grabComments :: ParserM [Text]
grabComments = do
  ls <- gets fst
  modify \(x, y) -> ([], y)
  return ls

allComments :: [RawTree] -> ([Text], [RawTree])
allComments = first (map getBody . filter isComment) . break isMeaningful
  where
    isMeaningful :: RawTree -> Bool
    isMeaningful (extract -> _ :> "" :> _) = False
    isMeaningful  _                        = True

    isComment :: RawTree -> Bool
    isComment (gist -> ParseTree ty _ _) = "comment" `Text.isSuffixOf` ty

allErrors :: [RawTree] -> [Text]
allErrors = map getBody . filter isUnnamedError
  where
    isUnnamedError :: RawTree -> Bool
    isUnnamedError tree = case only tree of
      (r :> "" :> _, ParseTree "ERROR" _ _) -> True
      _                                     -> False

getBody (gist -> f) = ptSource f

field :: Text -> ParserM1 RawTree
field name =
  fieldOpt name
    >>= maybe (throwM $ Failure [i|Cannot find field #{name}|]) return

fieldOpt :: Text -> ParserM1 (Maybe RawTree)
fieldOpt name = ask >>= go
  where
    go (tree@(extract -> _ :> n :> _) : rest)
      | n == name = return (Just tree)
      | otherwise = go rest

    go [] = return Nothing

fields :: Text -> ParserM1 [RawTree]
fields name = ask >>= go
  where
    go (tree@(extract -> _ :> n :> _) : rest) =
      (if n == name then ((tree :) <$>) else id)
        $ go rest
    go [] = return []

data ShowRange
  = Y | N
  deriving stock Eq

type Info    = Product [[Text], Range, ShowRange]
type PreInfo = Product [Range, ShowRange]

instance Modifies Info where
  ascribe (comms :> r :> pin :> _) = ascribeRange r pin . ascribeComms comms

ascribeComms comms
  | null comms = id
  | otherwise  = \d ->
      block $ map (pp . Text.init) comms ++ [d]

ascribeRange r Y = (pp r $$)
ascribeRange _ _ = id

withComments :: ParserM (Maybe (Product xs, a)) -> ParserM (Maybe (Product ([Text] : xs), a))
withComments act = do
  comms <- grabComments
  res   <- act
  return $ fmap (first (comms :>)) res

boilerplate
  :: (Text -> ParserM1 (f RawTree))
  -> (RawInfo, ParseTree RawTree)
  -> ParserM (Maybe (Info, f RawTree))
boilerplate f (r :> _, ParseTree ty cs _) = do
  withComments do
    mbf <- runParserM1 cs $ f ty
    return do
      f <- mbf
      return $ (r :> N :> Nil, f)

boilerplate'
  :: ((Text, Text) -> ParserM1 (f RawTree))
  -> (RawInfo, ParseTree RawTree)
  -> ParserM (Maybe (Info, f RawTree))
boilerplate' f (r :> _, ParseTree ty cs src) = do
  withComments do
    mbf <- runParserM1 cs $ f (ty, src)
    return do
      f <- mbf
      return $ (r :> N :> Nil, f)

fallthrough :: MonadFail m => m a
fallthrough = fail ""
