
{- | /The/ scope resolution system.
-}

module AST.Scope
  -- ( -- * Monad
  --   ScopeM
  -- , evalScopeM
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

import Control.Monad.State

import qualified Data.Map as Map
import           Data.Map   (Map)
import           Data.Text  (Text)
import qualified Data.Text as Text
import           Data.Maybe (fromJust)

import Range
import AST.Types
import AST.Parser
import Parser
import Tree
import Comment
import Pretty
import Product

import Debug.Trace

-- | Scope-holding monad.
type ScopeM = State [Scopes]

-- | Run the computation with scope starting from empty scope.
evalScopeM :: ScopeM a -> a
evalScopeM action = evalState action []

type Scopes = Tree '[ScopeTree] Range

data ScopeTree it
  = ScopeTree Env [it]
  deriving stock (Functor, Foldable, Traversable)
  deriving stock Show

instance Pretty1 ScopeTree where
  pp1 (ScopeTree e cs) =
    pp e `indent` block cs

instance {-# OVERLAPS #-} Pretty (Map Text ScopedDecl) where
  pp = block . map aux .  Map.toList
    where
      aux (n, ScopedDecl o b t rs) =
        pp o <+> (pp n <> ":") <+> pp t <+> "=" <+> pp b <+> "/" <+> (list rs <> ";")

  -- | The environment.
type Env = Map Text ScopedDecl
  -- deriving Show via PP Env

-- instance Pretty Env where
--   pp = vcat . map pp . _eDecls

-- | The type/value declaration.
data ScopedDecl = ScopedDecl
  { _sdOrigin :: Range
  , _sdBody   :: Maybe Range
  , _sdType   :: Maybe (Either (Pascal ()) Kind)
  , _sdRefs   :: [Range]
  }
  deriving Show via PP ScopedDecl

instance Pretty ScopedDecl where
  pp (ScopedDecl o b t refs) = pp o <+> "-" <+> maybe "?" (either pp pp) t <+> "=" <+> pp o

-- | The kind.
data Kind = Star
  deriving Show via PP Kind

instance Pretty Kind where
  pp _ = "*"

lookupEnv :: Text -> Env -> Maybe ScopedDecl
lookupEnv name = Map.lookup name

-- | Make a new scope out of enclosing parent one.
enter :: Range -> ScopeM ()
enter r =
  modify \rest ->
    mk r (ScopeTree Map.empty []) : rest

-- | Leave current scope, return to parent one.
leave  :: ScopeM ()
leave =
  modify \(a : parent : rest) ->
    fromJust do
      (r, ScopeTree e cs) <- match parent
      return $ mk r (ScopeTree e (a : cs)) : rest

-- | Add a declaration to the current scope.
define :: Text -> ScopedDecl -> ScopeM ()
define name d =
  modify \(top : rest) ->
    fromJust do
      (r, ScopeTree a cs) <- match top
      return $ mk r (ScopeTree (Map.insert name d a) cs) : rest

-- | Add a type declaration to the current scope.
defType :: HasRange a => Pascal a -> Kind -> Pascal a -> ScopeM ()
defType name kind body = do
  define (ppToText $ void name)
    $ ScopedDecl
      (getRange $ infoOf name)
      (Just $ getRange $ infoOf body)
      (Just (Right kind))
      []

addRef :: Text -> Range -> [Scopes] -> [Scopes]
addRef _    _   []                  = error "addRef: empty env stack"
addRef name pos stack@(topmost : _)
  | Just (r, ScopeTree top cs) <- match topmost =
    case Map.lookup name top of
      Just ScopedDecl {_sdOrigin} -> go _sdOrigin stack
      Nothing                     -> stack

    where
      go _     []             = []
      go range initial@(topmost : rest)
        | Just (r, ScopeTree e cs) <- match topmost =
          case Map.lookup name e of
            Just it | _sdOrigin it == range ->
              mk r (ScopeTree (pushRef pos e) cs) : go range rest
            _ ->
              initial

      pushRef pos' = Map.adjust (\sd -> sd { _sdRefs = pos' : _sdRefs sd }) name

-- | Add a value declaration to the current scope.
def
  :: HasRange a
  => Pascal a
  -> Maybe (Pascal a)
  -> Maybe (Pascal a)
  -> ScopeM ()
def name ty body = do
  define (ppToText $ void name)
    $ ScopedDecl
      (getRange $ infoOf name)
      ((getRange . infoOf) <$> body)
      ((Left . void) <$> ty)
      []

instance UpdateOver ScopeM Contract (Pascal a) where
  before r _ = enter r
  after  _ _ = leave

instance HasRange a => UpdateOver ScopeM Declaration (Pascal a) where
  before _ = \case
    TypeDecl ty body -> defType ty Star body
    _ -> skip

instance HasRange a => UpdateOver ScopeM Binding (Pascal a) where
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

instance HasRange a => UpdateOver ScopeM VarDecl (Pascal a) where
  after _ (Decl _ name ty) = def name (Just ty) Nothing

instance UpdateOver ScopeM Mutable (Pascal a)
instance UpdateOver ScopeM Type    (Pascal a)
instance UpdateOver ScopeM Variant (Pascal a)
instance UpdateOver ScopeM TField  (Pascal a)

instance HasRange a => UpdateOver ScopeM Expr (Pascal a) where
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

instance HasRange a => UpdateOver ScopeM Alt (Pascal a) where
  before r _ = enter r
  after  _ _ = leave

instance UpdateOver ScopeM LHS             (Pascal a)
instance UpdateOver ScopeM MapBinding      (Pascal a)
instance UpdateOver ScopeM Assignment      (Pascal a)
instance UpdateOver ScopeM FieldAssignment (Pascal a)
instance UpdateOver ScopeM Constant        (Pascal a)

instance HasRange a => UpdateOver ScopeM Pattern (Pascal a) where
  before _ = \case
    IsVar n -> def n Nothing Nothing
    _       -> skip

instance UpdateOver ScopeM QualifiedName (Pascal a)
instance UpdateOver ScopeM Path          (Pascal a)
instance UpdateOver ScopeM Name          (Pascal a) where
  before range (Name raw) = do
    modify $ addRef raw range

class HasEnv a where
  getEnv :: a -> Env

instance HasEnv Env where
  getEnv = id

instance Contains Env xs => HasEnv (Product xs) where
  getEnv = getElem

data Scope = Scope { unScope :: [Text] }

instance HasComments Scope where
  getComments = unScope

-- pinEnv :: Product xs -> ScopeM (Product (Env : xs))
-- pinEnv xs = (`Cons` xs) <$> gets head

collectEnv :: Contains Range xs => Product xs -> ScopeM (Product (Scopes : xs))
collectEnv xs = do
  gets \case
    st : _ -> Cons st xs
    []     -> Cons (mk (getRange xs) $ ScopeTree Map.empty []) xs

instance UpdateOver (State [Env]) ScopeTree Scopes where
  before r (ScopeTree e _) = modify (e :)
  after  r  _              = modify tail

distributeEnv :: ScopeTree Scopes -> State [Env] (ScopeTree Scopes)
distributeEnv (ScopeTree e' cs) = do
  e <- gets (Map.unions . (e' :))
  traceShowM ("distribute", e', e)
  return $ ScopeTree e cs

pinEnv :: Contains Range xs => Scopes -> Product xs -> ScopeM (Product (Env : xs))
pinEnv scopes info = do
  let (_, ScopeTree e _) = fromJust $ match =<< lookupTree (getElem info) scopes
  return (Cons e info)

instance HasComments Range where
  getComments _ = []

instance Pretty (Product xs) => HasComments (Product xs) where
  getComments xs = if Text.null $ Text.strip x then [] else [x]
    where
      x = ppToText $ color 3 $ pp $ xs

ascribeEnv :: (Contains Range xs, Pretty (Product xs)) => Pascal (Product xs) -> Pascal (Product (Env : xs))
ascribeEnv tree =
  let
    Cons scopes _ = traceShowId $ infoOf $ evalScopeM $ traverseTree collectEnv tree
    distributed   = evalState (traverseOnly distributeEnv scopes) []
  in
    -- distributed
    evalScopeM $ traverseTree (pinEnv distributed) tree