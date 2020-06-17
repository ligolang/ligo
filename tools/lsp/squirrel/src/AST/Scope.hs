
{-# language Strict #-}

{- | /The/ scope resolution system.
-}

module AST.Scope
  -- ( -- * Monad
  --   CollectM
  -- , evalCollectM
  -- , collectEnv

  --   -- * Scope
  -- , Env(..)
  -- , ScopedDecl(..)
  -- , Kind(..)
  -- , HasEnv(..)
  -- , lookupEnv

  --   -- * Methods
  -- , enter
  -- , leave
  -- , define
  -- , defType
  -- , def
  -- )
  where

import Control.Arrow (second)
import Control.Monad.State
import Control.Monad.Writer.Strict hiding (Alt, Product)

import           Data.Function
import qualified Data.Map as Map
import           Data.Map   (Map)
import           Data.Text  (Text)
import qualified Data.Text as Text
import           Data.Maybe (fromJust, listToMaybe)
import qualified Data.List as List

import Range
import AST.Types
import AST.Parser
import Parser
import Tree
import Comment
import Pretty
import Product
import Lattice

import Debug.Trace

class HasLocalScope x where
  getLocalScope :: x -> [ScopedDecl]

instance Contains [ScopedDecl] xs => HasLocalScope (Product xs) where
  getLocalScope = getElem

type CollectM = State (Product [FullEnv, [Range]])

type AddRefsM = State FullEnv

type FullEnv = Map Range [ScopedDecl]

addLocalScopes
  :: HasRange (Product xs)
  => Pascal (Product xs)
  -> Pascal (Product ([ScopedDecl] : xs))
addLocalScopes tree =
    fmap (\xs -> Cons (envAt envWithREfs $ getRange xs) xs) tree
  where
    envWithREfs = flip execState env do
      flip traverseOnly tree \r (Name t) -> do
        modify $ addRef (getRange r) t
        return (Name t)

    env
      = execCollectM
      $ traverseTree pure tree

envAt :: FullEnv -> Range -> [ScopedDecl]
envAt env pos =
    Map.elems scopes
  where
    ranges = List.sortBy partOrder $ filter isCovering $ Map.keys env
    scopes = Map.unions $ (map.foldMap) toScopeMap $ map (env Map.!) ranges

    isCovering = (pos <?)
    toScopeMap sd@ScopedDecl {_sdName} = Map.singleton (ppToText _sdName) sd

addRef :: Range -> Text -> FullEnv -> FullEnv
addRef r n env = Map.union (go range) env
  where
    go (r' : rest) =
      let decls = env Map.! r'
      in
        case updateOnly n r addRefToDecl decls of
          (True,  decls) -> Map.singleton r' decls
          (False, decls) -> Map.insert r' decls (go rest)
    go [] = Map.empty

    range
      = List.sortBy partOrder
      $ filter (r <?)
      $ Map.keys env

    -- decls' list = do
    --   r'    <- range
    --   decls <- Map.lookup r' env
    --   return $ (r', updateOnly n r addRefToDecl decls)

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

enter :: Range -> CollectM ()
enter r = do
  modify $ modElem (r :)

define :: ScopedDecl -> CollectM ()
define sd = do
  r <- gets (head . getElem)
  modify
    $ modElem @FullEnv
    $ Map.insertWith (++) r [sd]

leave :: CollectM ()
leave = modify $ modElem @[Range] tail

-- | Run the computation with scope starting from empty scope.
execCollectM :: CollectM a -> FullEnv
execCollectM action = getElem $ execState action $ Cons Map.empty (Cons [] Nil)

instance {-# OVERLAPS #-} Pretty FullEnv where
  pp = block . map aux .  Map.toList
    where
      aux (r, decls) =
        pp r `indent` block decls

-- | The type/value declaration.
data ScopedDecl = ScopedDecl
  { _sdName    :: Pascal ()
  , _sdOrigin  :: Range
  , _sdBody    :: Maybe Range
  , _sdType    :: Maybe (Either (Pascal ()) Kind)
  , _sdRefs    :: [Range]
  }
  deriving Show via PP ScopedDecl

instance Pretty ScopedDecl where
  pp (ScopedDecl n o b t refs) = color 3 (pp n) <+> pp o <+> ":" <+> color 4 (maybe "?" (either pp pp) t) <+> "=" <+> pp refs

-- | The kind.
data Kind = Star
  deriving Show via PP Kind

instance Pretty Kind where
  pp _ = "TYPE"

-- observe :: String -> CollectM a -> CollectM a
-- observe what act = do
--   s <- get
--   traceShowM (what, "BEFORE", s)
--   a <- act
--   s1 <- get
--   traceShowM (what, "AFTER", s1)
--   return a

lookupEnv :: Text -> [ScopedDecl] -> Maybe ScopedDecl
lookupEnv name = listToMaybe . filter ((name ==) . ppToText . _sdName)

-- -- | Make a new scope out of enclosing parent one.
-- enter :: Range -> CollectM ()
-- enter r = observe "enter" do
--   modify \rest ->
--     mk r (ScopeTree Map.empty []) : rest

-- -- | Leave current scope, return to parent one.
-- leave :: CollectM ()
-- leave = observe "leave" do
--   modify \case
--     (a : parent : rest) ->
--       fromJust do
--         -- traceShowM ("MOVE", a)
--         -- traceShowM ("TO  ", parent)
--         (r, ScopeTree e cs) <- match parent
--         -- traceShowM ("==  ", mk r (ScopeTree e (a : cs)))
--         -- traceShowM ("--")
--         return $ mk r (ScopeTree e (a : cs)) : rest

--     [x] -> error $ "trying to leave \n" ++ show x

-- -- | Add a declaration to the current scope.
-- define :: Text -> ScopedDecl -> CollectM ()
-- define name d = observe "define" do
--   s <- get
--   traceShowM ("DEFINE", s)
--   modify \(top : rest) ->
--     fromJust do
--       (r, ScopeTree a cs) <- match top
--       return $ mk r (ScopeTree (Map.insert name d a) cs) : rest

-- | Add a type declaration to the current scope.
defType :: HasRange a => Pascal a -> Kind -> Pascal a -> CollectM ()
defType name kind body = do
  define
    $ ScopedDecl
      (void name)
      (getRange $ infoOf name)
      (Just $ getRange $ infoOf body)
      (Just (Right kind))
      []

observe :: Pretty i => Pretty res => Text -> i -> res -> res
observe msg i res
  = traceShow (pp msg, "INPUT", pp i)
  $ traceShow (pp msg, "OUTPUT", pp res)
  $ res

-- addRef
--   :: Pascal ()
--   -> Range
--   -> FullEnv
--   -> FullEnv
-- addRef name pos (AppendMap envs) =
--     AppendMap $ envs <> affected''
--   where
--     ranges = Map.keys envs
--     (affected, other) = List.partition (pos <?) ranges
--     affected' = foldMap (\r -> Map.singleton r (envs Map.! r)) affected
--     affected'' = Map.map (\decls -> observe "addRef" decls $ addRefScopedDecls decls) affected'

--     addRefScopedDecls :: [ScopedDecl] -> [ScopedDecl]
--     addRefScopedDecls decls =
--       case after of
--         decl : after -> before ++ [addRefScopedDecl decl] ++ after
--         [] -> before
--       where
--         (before, after) = break (\sd -> ppToText (_sdName sd) == ppName) decls

--     addRefScopedDecl :: ScopedDecl -> ScopedDecl
--     addRefScopedDecl decl = decl { _sdRefs = pos : _sdRefs decl }

--     ppName = ppToText name

-- | Add a value declaration to the current scope.
def
  :: HasRange a
  => Pascal a
  -> Maybe (Pascal a)
  -> Maybe (Pascal a)
  -> CollectM ()
def name ty body = do
  define
    $ ScopedDecl
      (void name)
      (getRange $ infoOf name)
      ((getRange . infoOf) <$> body)
      ((Left . void) <$> ty)
      []

instance UpdateOver CollectM Contract (Pascal a) where
  before r _ = enter r
  after  _ _ = skip

instance HasRange a => UpdateOver CollectM Declaration (Pascal a) where
  before _ = \case
    TypeDecl ty body -> defType ty Star body
    _ -> skip

instance HasRange a => UpdateOver CollectM Binding (Pascal a) where
  before r = \case
    Function recur name _args ty body -> do
      when recur do
        def name (Just ty) (Just body)
      enter r

    _ -> enter r

  after _ = \case
    Irrefutable name    body -> do leave; def name  Nothing  (Just body)
    Var         name ty body -> do leave; def name (Just ty) (Just body)
    Const       name ty body -> do leave; def name (Just ty) (Just body)
    Function recur name _args ty body -> do
      leave
      unless recur do
        def name (Just ty) (Just body)

instance HasRange a => UpdateOver CollectM VarDecl (Pascal a) where
  after _ (Decl _ name ty) = def name (Just ty) Nothing

instance UpdateOver CollectM Mutable (Pascal a)
instance UpdateOver CollectM Type    (Pascal a)
instance UpdateOver CollectM Variant (Pascal a)
instance UpdateOver CollectM TField  (Pascal a)

instance HasRange a => UpdateOver CollectM Expr (Pascal a) where
  before r = \case
    Let    {} -> enter r
    Lambda {} -> enter r
    ForLoop k _ _ _ -> do
      enter r
      def k Nothing Nothing

    ForBox k mv _ _ _ -> do
      enter r
      def k Nothing Nothing
      maybe skip (\v -> def v Nothing Nothing) mv

    _ -> skip

  after _ = \case
    Let     {} -> leave
    Lambda  {} -> leave
    ForLoop {} -> leave
    ForBox  {} -> leave
    _ -> skip

instance HasRange a => UpdateOver CollectM Alt (Pascal a) where
  before r _ = enter r
  after  _ _ = leave

instance UpdateOver CollectM LHS             (Pascal a)
instance UpdateOver CollectM MapBinding      (Pascal a)
instance UpdateOver CollectM Assignment      (Pascal a)
instance UpdateOver CollectM FieldAssignment (Pascal a)
instance UpdateOver CollectM Constant        (Pascal a)

instance HasRange a => UpdateOver CollectM Pattern (Pascal a) where
  before _ = \case
    IsVar n -> def n Nothing Nothing
    _       -> skip

instance UpdateOver CollectM QualifiedName (Pascal a)
instance UpdateOver CollectM Path          (Pascal a)
instance UpdateOver CollectM Name          (Pascal a) where
  before range (Name raw) = do
    -- traceShowM ("name", raw)
    skip
    -- modify $ modElem $ addRef range (mk () (Name raw))

-- class HasEnv a where
--   getEnv :: a -> Env

-- instance HasEnv Env where
--   getEnv = id

-- instance Contains Env xs => HasEnv (Product xs) where
--   getEnv = getElem

-- data Scope = Scope { unScope :: [Text] }

-- instance HasComments Scope where
--   getComments = unScope

-- -- pinEnv :: Product xs -> CollectM (Product (Env : xs))
-- -- pinEnv xs = (`Cons` xs) <$> gets head

-- collectEnv :: Contains Range xs => Product xs -> CollectM (Product (Scopes : xs))
-- collectEnv xs = do
--   gets \case
--     st : _ -> Cons st xs
--     []     -> Cons (mk (getRange xs) $ ScopeTree Map.empty []) xs

-- instance UpdateOver (State [Env]) ScopeTree Scopes where
--   before r (ScopeTree e _) = modify (e :)
--   after  r  _              = modify tail

-- distributeEnv :: ScopeTree Scopes -> State [Env] (ScopeTree Scopes)
-- distributeEnv (ScopeTree e' cs) = do
--   e <- gets (Map.unions . (e' :))
--   return $ ScopeTree e cs

-- pinEnv :: Contains Range xs => Scopes -> Product xs -> CollectM (Product (Env : xs))
-- pinEnv scopes info = do
--   let (_, ScopeTree e _) = fromJust $ match =<< lookupTree (getElem info) scopes
--   return (Cons e info)

-- instance HasComments Range where
--   getComments _ = []

-- instance Pretty (Product xs) => HasComments (Product xs) where
--   getComments xs = if Text.null $ Text.strip x then [] else [x]
--     where
--       x = ppToText $ color 3 $ pp $ xs

-- ascribeEnv :: (Contains Range xs, Pretty (Product xs)) => Pascal (Product xs) -> Scopes -- Pascal (Product (Env : xs))
-- ascribeEnv tree =
--   let
--     scopes =
--       evalCollectM do
--         traverseTree collectEnv tree
--         gets head

--     -- distributed   = evalState (traverseOnly distributeEnv scopes) []
--   in
--     scopes
--     -- distributed
--     -- evalCollectM $ traverseTree (pinEnv distributed) tree