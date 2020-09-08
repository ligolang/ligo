{-# OPTIONS_GHC -Wno-orphans #-}

module Parser where

import Control.Arrow
import Control.Monad.Catch
import Control.Monad.RWS hiding (Product)

import Data.Functor
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text

import Duplo.Tree
import Duplo.Error
import Duplo.Pretty

import ParseTree
import Range
import Product

{-
  Comment grabber has 2 buffers: 1 and 2.

  1) We collect all comments before the node and append them into buffer 1.
  2) We collect all comments after the node and put them into buffer 2.
  3) `grabComments` takes all comments from buffer 1.
  4) On leaving, move move comments from 2 to 1.
-}

runParserM :: ParserM a -> IO (a, [Msg])
runParserM p = (\(a, _, errs) -> (a, errs)) <$> runRWST p [] ([], [])

type Msg      = (Range, Err Text ())
type ParserM  = RWST [RawTree] [Msg] ([Text], [Text]) IO

data Failure = Failure String
  deriving stock (Show)
  deriving anyclass (Exception)

instance Scoped (Product [Range, Text]) ParserM RawTree ParseTree where
  before (r :> _ :> _) (ParseTree _ cs _) = do
    let (comms, rest) = allComments cs
    let (comms1, _)   = allComments $ reverse rest
    modify $ first  (++ comms)
    modify $ second (++ reverse comms1)

    let errs = allErrors   cs
    tell $ fmap (\t -> (r, Err t)) errs

  after _ _ = do
    modify $ \(_, y) -> (y, [])

grabComments :: ParserM [Text]
grabComments = do
  ls <- gets fst
  modify \(_, y) -> ([], y)
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
      (_ :> "" :> _, ParseTree "ERROR" _ _) -> True
      _                                     -> False

getBody :: RawTree -> Text
getBody (gist -> f) = ptSource f

flag :: Text -> ParserM Bool
flag name = fieldOpt name <&> maybe False (const True)

field :: Text -> ParserM RawTree
field name =
  fieldOpt name
    >>= maybe (throwM $ Failure [i|Cannot find field #{name}|]) return

fieldOpt :: Text -> ParserM (Maybe RawTree)
fieldOpt name = ask >>= go
  where
    go (tree@(extract -> _ :> n :> _) : rest)
      | n == name = return (Just tree)
      | otherwise = go rest

    go _ = return Nothing

fields :: Text -> ParserM [RawTree]
fields name = ask >>= go
  where
    go (tree@(extract -> _ :> n :> _) : rest) =
      (if n == name then ((tree :) <$>) else id)
        $ go rest
    go _ = return []

data ShowRange
  = Y | N
  deriving stock Eq

instance Pretty ShowRange where
  pp Y = "Yau"
  pp N = "Nah"

newtype CodeSource = CodeSource { unCodeSource :: Text }
  deriving newtype (Eq, Ord, Show, Pretty)

type Info = [[Text], Range, ShowRange, CodeSource]

instance
  ( Contains Range xs
  , Contains [Text] xs
  , Contains ShowRange xs
  )
  => Modifies (Product xs)
  where
    ascribe xs
      = ascribeRange (getElem @Range xs) (getElem xs)
      . ascribeComms (getElem xs)

fillInfo :: Functor f => f (Product xs) -> f (Product ([Text] : Range : ShowRange : xs))
fillInfo = fmap \it -> [] :> point (-1) (-1) :> N :> it

ascribeComms :: [Text] -> Doc -> Doc
ascribeComms comms
  | null comms = id
  | otherwise  = \d ->
      block $ map (pp . Text.init) comms ++ [d]

ascribeRange :: Pretty p => p -> ShowRange -> Doc -> Doc
ascribeRange r Y = (pp r $$)
ascribeRange _ _ = id

withComments :: ParserM (Product xs, a) -> ParserM (Product ([Text] : xs), a)
withComments act = do
  comms <- grabComments
  res   <- act
  return $ first (comms :>) res

boilerplate
  :: (Text -> ParserM (f RawTree))
  -> (RawInfo, ParseTree RawTree)
  -> ParserM (Product Info, f RawTree)
boilerplate f (r :> _, ParseTree ty cs src) = do
  withComments do
    f' <- local (const cs) $ f ty
    return $ (r :> N :> CodeSource src :> Nil, f')

boilerplate'
  :: ((Text, Text) -> ParserM (f RawTree))
  -> (RawInfo, ParseTree RawTree)
  -> ParserM (Product Info, f RawTree)
boilerplate' f (r :> _, ParseTree ty cs src) = do
  withComments do
    f' <- local (const cs) $ f (ty, src)
    return $ (r :> N :> CodeSource src :> Nil, f')

fallthrough :: MonadThrow m => m a
fallthrough = throwM HandlerFailed
