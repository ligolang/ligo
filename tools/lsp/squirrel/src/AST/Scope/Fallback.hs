{-# LANGUAGE OverloadedStrings #-}

module AST.Scope.Fallback where

import Control.Arrow ((&&&))
import Control.Lens ((%~), (&))
import Control.Monad.Catch.Pure
import Control.Monad.State
import Control.Monad.Writer (Writer, WriterT, execWriterT, runWriter, tell)

import Data.Foldable (for_, toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)

import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree hiding (loop)

import AST.Skeleton
import Cli.Types
import Parser
import Product
import Range

import AST.Scope.Common

data Fallback

instance HasLigoClient m => HasScopeForest Fallback m where
  scopeForest _ = return . getEnv

addReferences :: LIGO Info -> ScopeForest -> ScopeForest
addReferences ligo = execState (loopM_ addRef ligo)
  where
    addRef :: LIGO Info -> State ScopeForest ()
    addRef = \case
      (match -> Just (r, Name     n)) -> addThisRef Variable (getRange r) n
      (match -> Just (r, NameDecl n)) -> addThisRef Variable (getRange r) n
      (match -> Just (r, TypeName n)) -> addThisRef Type     (getRange r) n
      _                               -> return ()

    addThisRef cat' r n = do
      modify
        $ withScopeForest \(sf, ds) ->
          flip runState ds do
            let frameSet = Set.toList =<< spine r =<< sf
            walkScope cat' r n frameSet
            return sf

    walkScope _    _ _ [] = return ()
    walkScope sort r n (declref : rest) = do
      decl <- gets (Map.! declref)
      if ofCategory sort decl && (n == _sdName decl || r == _sdOrigin decl)
      then do
        modify $ Map.adjust (addRefToDecl r) declref
      else do
        walkScope sort r n rest

    addRefToDecl r sd = sd { _sdRefs = r : _sdRefs sd }

getEnv :: LIGO Info -> ScopeForest
getEnv tree
  = addReferences tree
  $ extractScopeForest
  $ compressScopeTree
  $ extractScopeTree
  $ prepareTree tree

prepareTree
  :: LIGO Info
  -> LIGO '[[ScopedDecl], Bool, Range, [Text], Range, ShowRange, CodeSource]
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
  -> LIGO ([ScopedDecl] : Bool : Range : xs)
assignDecls = loop go . fmap (\r -> [] :> False :> getRange r :> r)
  where
    go = \case
      (match -> Just (r, Let decl body)) -> do
        let imm = getImmediateDecls decl
        let r' :< body' = body
        let l' :< decl' = decl
        let r'' = putElem (getRange body) $ putElem True $ modElem (imm <>) r'
        let l'' = putElem (getRange decl) $ putElem True l'
        make (r, Let (l'' :< decl') (r'' :< body'))

      (match -> Just (r, Lambda args ty body)) -> do
        let imms = getImmediateDecls =<< args
        let r' :< body' = body
        let r'' = putElem True $ modElem (imms <>) r'
        make (r, Lambda args ty (r'' :< body'))

      (match -> Just (r, Alt pat body)) -> do
        let imms = getImmediateDecls pat
        let r' :< body' = body
        let r'' = putElem True $ modElem (imms <>) r'
        make (r, Alt pat (r'' :< body'))

      (match -> Just (r, BFunction True n params ty b)) -> do
        let imms = getImmediateDecls =<< params
        let fDecl = functionScopedDecl (getElem r) n params ty b
        let r' = putElem True $ modElem ((fDecl : imms) <>) r
        make (r', BFunction True n params ty b)

      (match -> Just (r, BFunction False n params ty b)) -> do
        let imms = getImmediateDecls =<< params
        let fDecl = functionScopedDecl (getElem r) n params ty b
        let r' = putElem True (modElem (fDecl :) r)
        let b' = b & _extract %~ (putElem True . modElem (imms <>))
        make (r', BFunction False n params ty b')

      (match -> Just (r, node@BVar{})) -> markAsScope r node
      (match -> Just (r, node@BConst{})) -> markAsScope r node
      (match -> Just (r, node@BParameter{})) -> markAsScope r node
      (match -> Just (r, node@IsVar{})) -> markAsScope r node

      it@(match -> Just (r, TypeDecl n ty)) -> do
        let imms = getImmediateDecls it
        let r'   = putElem True $ modElem (imms <>) r
        make (r', TypeDecl n ty)

      it -> it

    markAsScope range node =
      let imms = getImmediateDecls (make (range, node))
          range' = putElem True (modElem (imms <>) range)
      in make (range', node)

functionScopedDecl
  :: ( HasRange param
     , HasRange body
     , Eq (Product nameInfo)
     , Modifies (Product nameInfo)
     , Contains Range nameInfo
     , Contains ShowRange nameInfo
     )
  => [Text]
  -> LIGO nameInfo
  -> [param]
  -> Maybe (LIGO whatever)
  -> body
  -> ScopedDecl
functionScopedDecl docs nameNode params typ body = ScopedDecl
  { _sdName   = name
  , _sdOrigin = origin
  , _sdBody   = Just $ getRange body
  , _sdType   = IsType . void' <$> typ
  , _sdRefs   = []
  , _sdDoc    = docs
  , _sdParams = Just (map (Parameter . getRange) params)
  }
  where
    (origin, name) = getName nameNode

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
  -> [ScopedDecl]
getImmediateDecls = \case
  (match -> Just (r, pat)) -> do
    case pat of
      IsVar v -> do
        let (r', name) = getName v
        [ScopedDecl name r' Nothing Nothing [] (getElem r) Nothing]

      IsTuple    xs   -> getImmediateDecls =<< xs
      IsList     xs   -> getImmediateDecls =<< xs
      IsSpread   s    -> getImmediateDecls s
      IsWildcard      -> []
      IsAnnot    x _  -> getImmediateDecls x
      IsCons     h t  -> getImmediateDecls h <> getImmediateDecls t
      IsConstant _    -> []
      IsConstr   _ xs -> getImmediateDecls =<< maybeToList xs

  (match -> Just (r, pat)) -> do
    case pat of
      BFunction _ f params t b -> [functionScopedDecl (getElem r) f params t b]

      BVar v t b -> do
        let (r', name) = getName v
        [ScopedDecl name r' (getRange <$> b) (IsType . void' <$> t) [] (getElem r) Nothing]

      BConst name typ (Just (layer -> Just (Lambda params _ body))) ->
        [functionScopedDecl (getElem r) name params typ body]

      BConst c t b -> do
        let (r', name) = getName c
        [ScopedDecl name r' (getRange <$> b) (IsType . void' <$> t) [] (getElem r) Nothing]

      BParameter n t -> do
        let (r', name) = getName n
        [ScopedDecl name r' Nothing (IsType . void' <$> Just t) [] (getElem r) Nothing]

      BTypeDecl t b -> do
        let (r', name) = getTypeName t
        [ScopedDecl name r' (Just $ getRange b) kind [] (getElem r) Nothing]

      BAttribute _ -> []
      BInclude _ -> []

  (match -> Just (_, Parameters ps)) -> do
    ps >>= getImmediateDecls

  _ -> []

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
