{-# LANGUAGE OverloadedStrings #-}

module AST.Scope.Fallback where

import Control.Arrow ((&&&))
import Control.Lens ((%~), (&))
import Control.Monad.Catch.Pure
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State
import Control.Monad.Writer (Writer, WriterT, execWriterT, runWriter, tell)

import Data.Foldable (for_, toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)

import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree hiding (loop)

import AST.Pretty (TotalLPP, docToText, lppDialect)
import AST.Scope.Common
import AST.Skeleton
import Cli.Types
import Parser
import Product
import Range
import Util (foldMapM)

data Fallback

instance HasLigoClient m => HasScopeForest Fallback m where
  scopeForest _ (SomeLIGO dialect ligo) = pure (runReader (getEnv ligo) dialect)

type ScopeM = Reader Lang

addReferences :: LIGO Info -> ScopeForest -> ScopeForest
addReferences ligo = execState $ loopM_ addRef ligo
  where
    addRef :: LIGO Info -> State ScopeForest ()
    addRef = \case
      (match -> Just (r, Name     n)) -> addThisRef TermLevel (getRange r) n
      (match -> Just (r, NameDecl n)) -> addThisRef TermLevel (getRange r) n
      (match -> Just (r, TypeName n)) -> addThisRef TypeLevel (getRange r) n
      _                               -> return ()

    addThisRef cat' r n = do
      modify
        $ withScopeForest \(sf, ds) ->
          flip runState ds do
            let frameSet = Set.toList =<< spine r =<< sf
            walkScope cat' r n frameSet
            return sf

    walkScope _    _ _ [] = return ()
    walkScope level r n (declref : rest) = do
      decl <- gets (Map.! declref)
      if ofLevel level decl && (n == _sdName decl || r == _sdOrigin decl)
      then do
        modify $ Map.adjust (addRefToDecl r) declref
      else do
        walkScope level r n rest

    addRefToDecl r sd = sd { _sdRefs = r : _sdRefs sd }

getEnv :: LIGO Info -> ScopeM ScopeForest
getEnv tree
  = addReferences tree
  <$> extractScopeForest
  <$> compressScopeTree
  <$> extractScopeTree
  <$> prepareTree tree

prepareTree
  :: LIGO Info
  -> ScopeM (LIGO '[[ScopedDecl], Bool, Range, [Text], Range, ShowRange, CodeSource])
prepareTree
  = assignDecls
  . unSeq
  . unLetRec

loop :: Functor f => (Cofree f a -> Cofree f a) -> Cofree f a -> Cofree f a
loop go = aux
  where
    aux (r :< fs) = go $ r :< fmap aux fs

loopM_ :: (Monad m, Foldable f) => (Cofree f a -> m ()) -> (Cofree f a -> m ())
loopM_ go = aux
  where
    aux (r :< fs) = do
      for_ fs aux
      go $ r :< fs

loopM
  :: (Monad m, Traversable f)
  => (Cofree f a -> m (Cofree f a)) -> (Cofree f a -> m (Cofree f a))
loopM go = aux
  where
    aux (r :< fs) = go =<< ((r :<) <$> traverse aux fs)

unLetRec
  :: ( Contains  Range     xs
     , Contains [Text]     xs
     , Contains  ShowRange xs
     , Eq (Product xs)
     )
  => LIGO xs
  -> LIGO xs
unLetRec = loop go
  where
    go = \case
      (match -> Just (r, expr)) -> do
        case expr of
          Let (match -> Just (_, Seq decls)) body -> do
            foldr reLet body decls
          _ -> make (r, expr)

      -- TODO: somehow append Unit to the end
      (match -> Just (r, RawContract decls)) -> do
        case reverse decls of
          lst : (reverse -> ini) -> do
            foldr reLet lst ini
          [] -> make (r, RawContract [])

      it -> it

unSeq
  :: ( Contains  Range     xs
     , Contains [Text]     xs
     , Contains  ShowRange xs
     , Eq (Product xs)
     )
  => LIGO xs
  -> LIGO xs
unSeq = loop go
  where
    go = \case
      (match -> Just (r, Seq decls)) -> do
        case reverse decls of
          lst : (reverse -> ini) -> do
            foldr reLet lst ini
          [] -> make (r, Seq [])

      it -> it

reLet
  :: ( Contains Range xs
     , Apply Functor fs
     , Eq (Product xs)
     , Element Expr fs
     )
  => Cofree (Sum fs) (Product xs)
  -> Cofree (Sum fs) (Product xs)
  -> Tree fs (Product xs)
reLet decl body = make (r', Let decl body)
  where
    r' = putElem (getRange decl `merged` getRange body)
        $ extract body

assignDecls
  :: ( Contains  Range     xs
     , Contains [Text]     xs
     , Contains  ShowRange xs
     , Eq (Product xs)
     , Pretty (Product xs)
     )
  => LIGO xs
  -> ScopeM (LIGO ([ScopedDecl] : Bool : Range : xs))
assignDecls = loopM go . fmap (\r -> [] :> False :> getRange r :> r)
  where
    go = \case
      (match -> Just (r, Let decl body)) -> do
        imm <- getImmediateDecls decl
        let r' :< body' = body
        let l' :< decl' = decl
        let r'' = putElem (getRange body) $ putElem True $ modElem (imm <>) r'
        let l'' = putElem (getRange decl) $ putElem True l'
        pure (make (r, Let (l'' :< decl') (r'' :< body')))

      (match -> Just (r, Lambda args ty body)) -> do
        imms <- foldMapM getImmediateDecls args
        let r' :< body' = body
        let r'' = putElem True $ modElem (imms <>) r'
        pure (make (r, Lambda args ty (r'' :< body')))

      (match -> Just (r, Alt pat body)) -> do
        imms <- getImmediateDecls pat
        let r' :< body' = body
        let r'' = putElem True $ modElem (imms <>) r'
        pure (make (r, Alt pat (r'' :< body')))

      (match -> Just (r, BFunction True n params ty b)) -> do
        imms <- foldMapM getImmediateDecls params
        fDecl <- functionScopedDecl (getElem r) n params ty b
        let r' = putElem True $ modElem ((fDecl : imms) <>) r
        pure (make (r', BFunction True n params ty b))

      (match -> Just (r, BFunction False n params ty b)) -> do
        imms <- foldMapM getImmediateDecls params
        fDecl <- functionScopedDecl (getElem r) n params ty b
        let r' = putElem True (modElem (fDecl :) r)
        let b' = b & _extract %~ (putElem True . modElem (imms <>))
        pure (make (r', BFunction False n params ty b'))

      (match -> Just (r, node@BVar{})) -> markAsScope r node
      (match -> Just (r, node@BConst{})) -> markAsScope r node
      (match -> Just (r, node@BParameter{})) -> markAsScope r node
      (match -> Just (r, node@IsVar{})) -> markAsScope r node
      (match -> Just (r, node@BTypeDecl{})) -> markAsScope r node

      it -> pure it

    markAsScope range node = do
      imms <- getImmediateDecls (make (range, node))
      let range' = putElem True (modElem (imms <>) range)
      pure (make (range', node))

functionScopedDecl
  :: ( TotalLPP param
     , HasRange body
     , Eq (Product nameInfo)
     , Modifies (Product nameInfo)
     , Contains Range nameInfo
     , Contains ShowRange nameInfo
     )
  => [Text]
  -> LIGO nameInfo
  -> [param]
  -> Maybe (LIGO typ)
  -> body
  -> ScopeM ScopedDecl
functionScopedDecl docs nameNode paramNodes typ body = do
  dialect <- ask
  let params = map (Parameter . docToText . lppDialect dialect) paramNodes
  pure $ ScopedDecl
    { _sdName = name
    , _sdOrigin = origin
    , _sdBody = Just $ getRange body
    , _sdType = IsType . void' <$> typ
    , _sdRefs = []
    , _sdDoc = docs
    , _sdParams = Just params
    , _sdDialect = dialect
    }
  where
    (origin, name) = getName nameNode

valueScopedDecl
  :: ( HasRange body
     , Eq (Product nameInfo)
     , Modifies (Product nameInfo)
     , Contains Range nameInfo
     , Contains ShowRange nameInfo
     )
  => [Text]
  -> LIGO nameInfo
  -> Maybe (LIGO typ)
  -> Maybe body
  -> ScopeM ScopedDecl
valueScopedDecl docs nameNode typ body = do
  dialect <- ask
  pure $ ScopedDecl
    { _sdName = name
    , _sdOrigin = origin
    , _sdBody = getRange <$> body
    , _sdType = IsType . void' <$> typ
    , _sdRefs = []
    , _sdDoc = docs
    , _sdParams = Nothing
    , _sdDialect = dialect
    }
  where
    (origin, name) = getName nameNode

typeScopedDecl
  :: ( HasRange body
     , Eq (Product nameInfo)
     , Modifies (Product nameInfo)
     , Contains Range nameInfo
     , Contains ShowRange nameInfo)
  => [Text] -> LIGO nameInfo -> body -> ScopeM ScopedDecl
typeScopedDecl docs nameNode body = do
  dialect <- ask
  pure $ ScopedDecl
    { _sdName = name
    , _sdOrigin = origin
    , _sdBody = Just (getRange body)
    , _sdType = kind
    , _sdRefs = []
    , _sdDoc = docs
    , _sdParams = Nothing
    , _sdDialect = dialect
    }
  where
    (origin, name) = getTypeName nameNode

-- | Wraps a value into a list. Like 'pure' but perhaps with a more clear intent.
singleton :: a -> [a]
singleton x = [x]

extractScopeTree
  :: LIGO ([ScopedDecl] : Bool : Range : xs)
  -> Tree' '[[]] '[[ScopedDecl], Bool, Range]
extractScopeTree = go
  where
    go
      :: LIGO ([ScopedDecl] : Bool : Range : xs)
      -> Tree' '[[]] '[[ScopedDecl], Bool, Range]
    go ((decls :> visible :> r :> _) :< fs) = do
      let fs'  = toList fs
      let fs'' = map go fs'
      make (decls :> visible :> r :> Nil, fs'')

-- 'Bool' in the node list denotes whether this part of a tree is a scope
compressScopeTree
  :: Tree' '[[]] '[[ScopedDecl], Bool, Range]
  -> [Tree' '[[]] '[[ScopedDecl], Range]]
compressScopeTree = go
  where
    go
      :: Tree' '[[]] '[[ScopedDecl], Bool, Range]
      -> [Tree' '[[]] '[[ScopedDecl], Range]]
    go (only -> (_ :> False :> _ :> Nil, rest)) =
      rest >>= go

    go (only -> (decls :> True :> r :> Nil, rest)) =
      let rest' = rest >>= go
      in [ make (decls :> r :> Nil, rest')
         | not (null decls) || not (null rest')
         ]

    go _ = error "compressScopeTree: impossible"

extractScopeForest
  :: [Tree' '[[]] '[[ScopedDecl], Range]]
  -> ScopeForest
extractScopeForest = uncurry ScopeForest . runWriter . mapM go
  where
    go
      :: Tree' '[[]] '[[ScopedDecl], Range]
      -> Writer (Map DeclRef ScopedDecl) (Tree' '[[]] ScopeInfo)
    go (only -> (decls :> r :> Nil, ts)) = do
      let uName sd  = DeclRef (ppToText (_sdName sd)) (_sdOrigin sd)
      let extracted = Map.fromList $ map (uName &&& id) decls
      tell extracted
      let refs    = Map.keysSet extracted
      let r'      = refs :> r :> Nil
      ts' <- mapM go ts
      return $ make (r', ts')

    go _ = error "extractScopeForest: impossible"

getImmediateDecls
  :: ( Contains Range     xs
     , Contains [Text]     xs
     , Contains ShowRange xs
     , Eq (Product xs)
     )
  => LIGO xs
  -> ScopeM [ScopedDecl]
getImmediateDecls = \case
  (match -> Just (r, pat)) -> do
    case pat of
      IsVar v ->
        singleton <$> valueScopedDecl (getElem r) v Nothing (Nothing @Range)

      IsTuple    xs   -> foldMapM getImmediateDecls xs
      IsList     xs   -> foldMapM getImmediateDecls xs
      IsSpread   s    -> getImmediateDecls s
      IsWildcard      -> pure []
      IsAnnot    x _  -> getImmediateDecls x
      IsCons     h t  -> (<>) <$> getImmediateDecls h <*> getImmediateDecls t
      IsConstant _    -> pure []
      IsConstr   _ xs -> foldMapM getImmediateDecls xs

  (match -> Just (r, pat)) -> do
    case pat of
      BFunction _ f params t b ->
        singleton <$> functionScopedDecl (getElem r) f params t b

      BVar v t b -> singleton <$> valueScopedDecl (getElem r) v t b

      BConst name typ (Just (layer -> Just (Lambda params _ body))) ->
        singleton <$> functionScopedDecl (getElem r) name params typ body

      BConst c t b -> singleton <$> valueScopedDecl (getElem r) c t b

      BParameter n t
        -> singleton <$> valueScopedDecl (getElem r) n (Just t) (Nothing @Range)

      BTypeDecl t b -> singleton <$> typeScopedDecl (getElem r) t b

      BAttribute _ -> pure []
      BInclude _ -> pure []

  _ -> pure []

kind :: Maybe TypeOrKind
kind = Just $ IsKind Star

select
  :: ( Lattice  (Product info)
     , Contains  ShowRange info
     , Contains  Range info
     , Modifies (Product info)
     , Eq (Product info)
     )
  => Text
  -> [Visit RawLigoList (Product info) (WriterT [LIGO info] Catch)]
  -> LIGO info
  -> (Range, Text)
select what handlers t
  = maybe
      (error . show $ "Tree does not contain a" <+> pp what <.> ":" <+> pp t <+> pp (getRange $ extract t))
      (getElem . extract &&& ppToText)
  $ either (const Nothing) listToMaybe
  $ runCatch
  $ execWriterT
  $ visit handlers
    t

getName
  :: ( Lattice  (Product info)
     , Contains  ShowRange info
     , Contains  Range info
     , Modifies (Product info)
     , Eq (Product info)
     )
  => LIGO info
  -> (Range, Text)
getName = select "name"
  [ Visit \(r, NameDecl t) -> do
      tell [make (r, Name t)]
  ]

getTypeName
  :: ( Lattice  (Product info)
     , Contains  ShowRange info
     , Contains  Range info
     , Modifies (Product info)
     , Eq (Product info)
     )
  => LIGO info
  -> (Range, Text)
getTypeName = select "type name"
  [ Visit \(r, TypeName t) -> do
      tell [make (r, TypeName t)]
  ]
