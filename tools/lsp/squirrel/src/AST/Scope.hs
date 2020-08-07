
{-# language Strict #-}

{- | /The/ scope resolution system.
-}

module AST.Scope
  -- ( HasLocalScope (..)
  -- , addLocalScopes
  -- , lookupEnv
  -- , Kind (..)
  -- , ScopedDecl (..)
  -- )
  where

import           Control.Arrow (second)
import           Control.Monad.State
import           Control.Monad.Catch
import           Control.Monad.Catch.Pure

import qualified Data.List   as List
import           Data.Map            (Map)
import qualified Data.Map    as Map
import           Data.Maybe          (listToMaybe)
import           Data.Text           (Text)

import           Duplo.Lattice
import           Duplo.Pretty
import           Duplo.Tree
import           Duplo.Error

-- import           AST.Parser
import           AST.Types
import           Parser
import           Product
import           Range

-- import           Debug.Trace

type CollectM = StateT (Product [FullEnv, [Range]]) Catch

type FullEnv = Product ["vars" := Env, "types" := Env]
type Env     = Map Range [ScopedDecl]

data Category = Variable | Type
  deriving Eq

-- | The type/value declaration.
data ScopedDecl = ScopedDecl
  { _sdName    :: LIGO ()
  , _sdOrigin  :: Range
  , _sdBody    :: Maybe Range
  , _sdType    :: Maybe (Either (LIGO ()) Kind)
  , _sdRefs    :: [Range]
  , _sdDoc     :: [Text]
  }
  deriving Show via PP ScopedDecl

instance Eq ScopedDecl where
  sd == sd1 = and
    [ pp (_sdName   sd) == pp (_sdName   sd1)
    ,     _sdOrigin sd  ==     _sdOrigin sd1
    ]

-- | The kind.
data Kind = Star
  deriving Show via PP Kind

instance {-# OVERLAPS #-} Pretty FullEnv where
  pp = block . map aux . Map.toList . mergeFE
    where
      aux (r, fe) =
        pp r `indent` block fe

      mergeFE fe = getTag @"vars" @Env fe Prelude.<> getTag @"types" fe

instance Pretty ScopedDecl where
  pp (ScopedDecl n o _ t refs doc) = color 3 (pp n) <+> pp o <+> ":" <+> color 4 (maybe "?" (either pp pp) t) <+> "=" <+> pp refs `indent` pp doc

instance Pretty Kind where
  pp _ = "TYPE"

instance Pretty Category where
  pp Variable = "Variable"
  pp Type     = "Type"

emptyEnv :: FullEnv
emptyEnv = Tag Map.empty :> Tag Map.empty :> Nil

with :: Category -> FullEnv -> (Env -> Env) -> FullEnv
with Variable env f = modTag @"vars"  f env
with Type     env f = modTag @"types" f env

ofCategory :: Category -> ScopedDecl -> Bool
ofCategory Variable ScopedDecl { _sdType = Just (Right Star) } = False
ofCategory Variable _                                          = True
ofCategory Type     ScopedDecl { _sdType = Just (Right Star) } = True
ofCategory _        _                                          = False

type Info' = Product [[ScopedDecl], Maybe Category, [Text], Range, ShowRange]

-- instance Modifies (Product '[[ScopedDecl], Maybe Category, [Text], Range, a]) where
--   ascribe (ds :> _ :> _ :> r :> _) d =
--     color 3 (fsep (map (pp . _sdName) ds))
--     $$ pp r
--     $$ d

addLocalScopes
  :: forall xs
  .  (Collectable xs, Eq (Product xs))
  => LIGO (Product xs)
  -> LIGO (Product ([ScopedDecl] : Maybe Category : xs))
addLocalScopes tree =
    fmap (\xs -> fullEnvAt envWithREfs (getRange xs) :> xs) tree1
  where
    tree0       = either (error . show) id $ runCatch $ unLetRec tree
    tree1       = addNameCategories tree0
    envWithREfs = getEnvTree tree0

unLetRec
  :: forall xs m
  .  ( MonadCatch m
     , Contains Range xs
     , Eq (Product xs)
     )
  => LIGO (Product xs)
  -> m (LIGO (Product xs))
unLetRec = descent leaveBe
  [ Descent
      \case
        (r, Let (layer -> Just (Seq xs)) b) -> maybe (throwM HandlerFailed) return $ convert (getElem r) b xs
        _                                   -> fallthrough
  ]
  where
    convert :: Range -> LIGO (Product xs) -> [LIGO (Product xs)] -> Maybe (Product xs, Expr (LIGO (Product xs)))
    convert r b = match @Expr . linearize r b

    linearize :: Range -> LIGO (Product xs) -> [LIGO (Product xs)] -> LIGO (Product xs)
    linearize r b [x]      = make (modElem @Range (delta r) $ extract x, Let x b)
    linearize r b (x : xs) = make (modElem @Range (delta r) $ extract x, Let x (linearize r b xs))
    linearize _ _ []       = error "empty Seq"

    delta (Range _ f _) (Range s _ t) = Range s f t

addNameCategories
  :: (Contains Range xs, Eq (Product xs))
  => LIGO (Product xs)
  -> LIGO (Product (Maybe Category : xs))
addNameCategories tree = evalCollectM do
  descent (changeInfo (Nothing :>))
    [ Descent
        \(r, Name t) -> do
        modify $ modElem $ getRange r `addRef` (Variable, t)
        return $ (Just Variable :> r, Name t)

    , Descent
        \(r, TypeName t) -> do
        modify $ modElem $ getRange r `addRef` (Type, t)
        return $ (Just Type :> r, TypeName t)
    ]
    tree

getEnvTree
  :: ( Apply (Scoped b CollectM (Tree fs b)) fs
     , Apply Foldable fs
     , Apply Functor fs
     , Apply Traversable fs
     , Lattice b
     , HasRange b
     , Element Name fs
     , Element TypeName fs
     )
  => Tree fs b
  -> FullEnv
getEnvTree tree = envWithREfs
  where
    envWithREfs = execCollectM' env do
      descent leaveBe
        [ Descent \(r, Name t) -> do
            modify $ modElem $ getRange r `addRef` (Variable, t)
            return (r, Name t)

        , Descent \(r, TypeName t) -> do
            modify $ modElem $ getRange r `addRef` (Type, t)
            return (r, TypeName t)
        ]
        tree

    env
      = execCollectM
      $ descent (usingScope' leaveBe) [] tree

fullEnvAt :: FullEnv -> Range -> [ScopedDecl]
fullEnvAt fe r
  = envAt (getTag @"types" fe) r
  `mappend` envAt (getTag @"vars" fe) r

envAt :: Env -> Range -> [ScopedDecl]
envAt env pos =
    Map.elems scopes
  where
    ranges = List.sortBy partOrder $ filter isCovering $ Map.keys env
    scopes = Map.unions $ (map.foldMap) toScopeMap $ map (env Map.!) ranges

    isCovering = (pos `leq`)
    toScopeMap sd@ScopedDecl {_sdName} = Map.singleton (ppToText _sdName) sd

addRef :: Range -> (Category, Text) -> FullEnv -> FullEnv
addRef r (categ, n) env =
  with categ env \slice ->
    Map.union
      (go slice $ range slice)
      slice
  where
    go slice (r' : rest) =
      let decls = slice Map.! r'
      in
        case updateOnly n r addRefToDecl decls of
          (True,  decls') -> Map.singleton r' decls'
          (False, decls') -> Map.insert    r' decls' (go slice rest)
    go _ [] = Map.empty

    range slice
      = List.sortBy partOrder
      $ filter (r `leq`)
      $ Map.keys slice

    addRefToDecl sd = sd
      { _sdRefs = r : _sdRefs sd
      }

updateOnly
  :: Text
  -> Range
  -> (ScopedDecl -> ScopedDecl)
  -> [ScopedDecl]
  -> (Bool, [ScopedDecl])
updateOnly name r f = go
  where
    go = \case
      d : ds
        | ppToText (_sdName d) == name ->
          if r == _sdOrigin d
          then         (True,   d : ds)
          else         (True, f d : ds)
        | otherwise -> second (d :) (go ds)

      [] -> (False, [])

enter :: Collectable xs => Product xs -> CollectM ()
enter r = do
  modify $ modElem (getElem @Range r :)

define :: Category -> ScopedDecl -> CollectM ()
define categ sd = do
  r <- gets (head . getElem @[Range])
  modify
    $ modElem @FullEnv \env ->
        with categ env
        $ Map.insertWith (++) r [sd]

leave :: CollectM ()
leave = modify $ modElem @[Range] tail

-- | Run the computation with scope starting from empty scope.
execCollectM :: CollectM a -> FullEnv
execCollectM = execCollectM' emptyEnv

execCollectM' :: FullEnv -> CollectM a -> FullEnv
execCollectM' env action
  = getElem
  $ either (error . show) id
  $ runCatch
  $ execStateT action
  $ env :> [] :> Nil

-- | Run the computation with scope starting from empty scope.
evalCollectM :: CollectM a -> a
evalCollectM  = evalCollectM' emptyEnv

-- | Run the computation with scope starting from empty scope.
evalCollectM' :: FullEnv -> CollectM a -> a
evalCollectM' env action
  = either (error . show) id
  $ runCatch
  $ evalStateT action
  $ env :> [] :> Nil

-- | Search for a name inside a local scope.
lookupEnv :: Text -> [ScopedDecl] -> Maybe ScopedDecl
lookupEnv name = listToMaybe . filter ((name ==) . ppToText . _sdName)

-- | Add a type declaration to the current scope.
defType :: HasRange a => LIGO a -> Kind -> LIGO a -> [Text] -> CollectM ()
defType name kind body doc = do
  define Type
    $ ScopedDecl
      (void name)
      (getRange $ extract name)
      (Just $ getRange $ extract body)
      (Just (Right kind))
      []
      doc

-- -- observe :: Pretty i => Pretty res => Text -> i -> res -> res
-- -- observe msg i res
-- --   = traceShow (pp msg, "INPUT", pp i)
-- --   $ traceShow (pp msg, "OUTPUT", pp res)
-- --   $ res

-- | Add a value declaration to the current scope.
def
  :: HasRange a
  => LIGO a
  -> Maybe (LIGO a)
  -> Maybe (LIGO a)
  -> [Text]
  -> CollectM ()
def name ty body doc = do
  define Variable
    $ ScopedDecl
      (void name)
      (getRange $ extract name)
      ((getRange . extract) <$> body)
      ((Left . void) <$> ty)
      []
      doc

type Collectable xs = (Contains Range xs, Contains [Text] xs)

instance Collectable xs => Scoped (Product xs) CollectM (LIGO (Product xs)) Contract where
  before r _ = enter r
  after  _ _ = skip

instance Collectable xs => Scoped (Product xs) CollectM (LIGO (Product xs)) RawContract where
  before r _ = enter r
  after  _ _ = skip

instance Collectable xs => Scoped (Product xs) CollectM (LIGO (Product xs)) Binding where
  before r = \case
    Function recur name _args ty body -> do
      when recur do
        def name (Just ty) (Just body) (getElem r)
      enter r

    TypeDecl ty body -> defType ty Star body (getElem r)
    _ -> enter r

  after r = \case
    Irrefutable name    body -> do leave; def name  Nothing  (Just body) (getElem r)
    Var         name ty body -> do leave; def name (Just ty) (Just body) (getElem r)
    Const       name ty body -> do leave; def name (Just ty) (Just body) (getElem r)

    Function recur name _args ty body -> do
      leave
      unless recur do
        def name (Just ty) (Just body) (getElem r)

    _ -> skip

instance Collectable xs => Scoped (Product xs) CollectM (LIGO (Product xs)) VarDecl where
  after r (Decl _ name ty) = def name (Just ty) Nothing (getElem r)

instance Scoped a CollectM (LIGO a) Mutable
instance Scoped a CollectM (LIGO a) Type
instance Scoped a CollectM (LIGO a) Variant
instance Scoped a CollectM (LIGO a) TField

instance Collectable xs => Scoped (Product xs) CollectM (LIGO (Product xs)) Expr where
  before r = \case
    Let    {} -> enter r
    Lambda {} -> enter r
    ForLoop k _ _ _ _ -> do
      enter r
      def k Nothing Nothing (getElem r)

    ForBox k mv _ _ _ -> do
      enter r
      def k Nothing Nothing (getElem r)
      maybe skip (\v -> def v Nothing Nothing (getElem r)) mv

    _ -> skip

  after _ = \case
    Let     {} -> leave
    Lambda  {} -> leave
    ForLoop {} -> leave
    ForBox  {} -> leave
    _ -> skip

instance Collectable xs => Scoped (Product xs) CollectM (LIGO (Product xs)) Alt where
  before r _ = enter r
  after  _ _ = leave

instance Scoped a CollectM (LIGO a) LHS
instance Scoped a CollectM (LIGO a) MapBinding
instance Scoped a CollectM (LIGO a) Assignment
instance Scoped a CollectM (LIGO a) FieldAssignment
instance Scoped a CollectM (LIGO a) Constant

instance Collectable xs => Scoped (Product xs) CollectM (LIGO (Product xs)) Pattern where
  before r = \case
    IsVar n -> def n Nothing Nothing (getElem r)
    _       -> skip

instance Scoped a CollectM (LIGO a) QualifiedName
instance Scoped a CollectM (LIGO a) Path
instance Scoped a CollectM (LIGO a) Name
instance Scoped a CollectM (LIGO a) TypeName
instance Scoped a CollectM (LIGO a) FieldName

instance Scoped a CollectM (LIGO a) (Err Text)
instance Scoped a CollectM (LIGO a) Language
instance Scoped a CollectM (LIGO a) Parameters
instance Scoped a CollectM (LIGO a) Ctor