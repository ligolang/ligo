{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Pretty printers for all 3 dialects and core s-expressions
-- and their corresponding `Show` instances for @AST.Sceleton@ types.

module AST.Pretty where

import Data.Sum
import qualified Data.Text as Text

import AST.Skeleton

import Data.Maybe (isJust)
import Duplo (Cofree ((:<)), Layers)
import Duplo.Error (Err)
import Duplo.Error (Err (Err))
import Duplo.Pretty
  (Doc, Modifies (..), PP (PP), Pretty (..), Pretty1 (..), above, brackets, color, empty, fsep,
  indent, parens, ppToText, punctuate, ($+$), (<+>), (<.>))
import Duplo.Tree (Tree)

----------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------

class
    (Pretty expr)
  =>
    LPP (dialect :: Lang) expr
  where
    lpp :: expr -> Doc
    lpp = pp

instance LPP dialect () where
instance LPP dialect Text.Text where
instance LPP dialect Doc where

class
    (Pretty1 expr)
  =>
    LPP1 (dialect :: Lang) (expr :: * -> *)
  where
  lpp1 :: expr Doc -> Doc
  lpp1 = pp1

instance LPP1 dialect [] where
  lpp1 = list

deriving instance LPP1 dialect Maybe

instance {-# OVERLAPPABLE #-}
    (LPP d a, LPP1 d p, Functor p)
  =>
    LPP d (p a)
  where
  lpp = lpp1 @d . fmap (lpp @d)

deriving via PP (Undefined it) instance Pretty it => Show (Undefined it)
deriving via PP (Contract it) instance Pretty it => Show (Contract it)
deriving via PP (RawContract it) instance Pretty it => Show (RawContract it)
deriving via PP (Binding it) instance Pretty it => Show (Binding it)
deriving via PP (Parameters it) instance Pretty it => Show (Parameters it)
deriving via PP (Type it) instance Pretty it => Show (Type it)
deriving via PP (Variant it) instance Pretty it => Show (Variant it)
deriving via PP (TField it) instance Pretty it => Show (TField it)
deriving via PP (Expr it) instance Pretty it => Show (Expr it)
deriving via PP (Alt it) instance Pretty it => Show (Alt it)
deriving via PP (MapBinding it) instance Pretty it => Show (MapBinding it)
deriving via PP (FieldAssignment it) instance Pretty it => Show (FieldAssignment it)
deriving via PP (Constant it) instance Pretty it => Show (Constant it)
deriving via PP (Pattern it) instance Pretty it => Show (Pattern it)
deriving via PP (QualifiedName it) instance Pretty it => Show (QualifiedName it)
deriving via PP (Path it) instance Pretty it => Show (Path it)
deriving via PP (Name it) instance Pretty it => Show (Name it)
deriving via PP (NameDecl it) instance Pretty it => Show (NameDecl it)
deriving via PP (TypeName it) instance Pretty it => Show (TypeName it)
deriving via PP (Ctor it) instance Pretty it => Show (Ctor it)
deriving via PP (FieldName it) instance Pretty it => Show (FieldName it)
deriving via PP (Error it) instance Pretty it => Show (Error it)

instance
    ( Apply (LPP1 d) layers
    , Apply Pretty1 layers
    )
  =>
    LPP1 d (Sum layers)
  where
  lpp1 = apply @(LPP1 d) (lpp1 @d)

instance
    ( Layers layers
    , Modifies info
    , Apply (LPP1 d) layers
    )
  =>
    LPP d (Tree layers info)
  where
  lpp (d :< f) = ascribe d $ lpp1 @d $ lpp @d <$> f

instance LPP d msg => LPP1 d (Err msg) where
  lpp1 (Err msg) = lpp @d msg

-- class LPPProd (dialect :: Lang) xs where
--   lppProd :: Product xs -> Doc

-- instance {-# OVERLAPS #-} LPP d x => LPPProd d '[x] where
--   lppProd (x :> Nil) = lpp @d x

-- instance (LPP d x, LPPProd d xs) => LPPProd d (x : xs) where
--   lppProd (x :> xs) = "testprod"

-- instance LPP d (Product '[]) where
--   lpp Nil = "emptyprod"

-- instance (LPPProd d xs, PrettyProd xs) => LPP d (Product xs) where
--   lpp _ = "lpprodi"

-- instance LPP d Range where
-- instance LPP d ShowRange where

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

sexpr :: Text.Text -> [Doc] -> Doc
sexpr header items = "(" <.> pp header `indent` foldr above empty items <.> ")"

sop :: Doc -> Text.Text -> [Doc] -> Doc
sop a op b = "(" <.> a `indent` pp op `indent` foldr above empty b <.> ")"

blockWith :: forall dialect p . LPP dialect p => (Doc -> Doc) -> [p] -> Doc
blockWith f = foldr indent empty . map (f . lpp @dialect)

block' :: [Doc] -> Doc
-- block' = foldr (<.>) empty . map ((<.> "\n") . lpp)
block' = foldr ($+$) empty . map ((<.> "\n") . lpp)

list :: forall dialect p . LPP dialect p => [p] -> Doc
list = brackets . train @dialect @p ";"

train :: forall dialect p . LPP dialect p => Doc -> [p] -> Doc
train sep' = fsep . punctuate sep' . map (lpp @dialect)

tuple :: forall dialect p . LPP dialect p => [p] -> Doc
tuple = parens . train @dialect @p ","

braces :: Doc -> Doc
braces p = "{" <+> p <+> "}"

----------------------------------------------------------------------------
-- Core sexpr
----------------------------------------------------------------------------

instance Pretty1 Undefined where
  pp1 = \case
    Undefined mess -> "{{{" <.> pp (Text.take 20 mess) <.> "}}}"

instance Pretty1 Contract where
  pp1 = \case
    ContractEnd -> "(endtract)"
    ContractCons x xs -> sexpr "contract" [x, xs]

instance Pretty1 RawContract where
  pp1 = \case
    RawContract xs -> sexpr "contract" xs

instance Pretty1 Binding where
  pp1 = \case
    TypeDecl     n    ty       -> sexpr "type"  [n, ty]
    Parameter    n    ty       -> sexpr "parameter"  [n, ty]
    Var          name ty value -> sexpr "var"   [name, pp ty, pp value]
    Const        name ty body  -> sexpr "const" [name, pp ty, pp body]
    Attribute    name          -> sexpr "attr"  [name]
    Include      fname         -> sexpr "#include" [fname]

    Function isRec name params ty body ->
      sexpr "fun" $ concat
        [ ["rec" | isRec]
        , [name]
        , params
        , [":", pp ty]
        , ["=", body]
        ]

instance Pretty1 Parameters where
  pp1 = \case
    Parameters them -> sexpr "params" them

instance Pretty1 Type where
  pp1 = \case
    TArrow    dom codom -> sop dom "->" [codom]
    TRecord   fields    -> sexpr "RECORD" fields
    TVar      name      -> name
    TSum      variants  -> sexpr "SUM" variants
    TProduct  elements  -> sexpr "PROD" elements
    TApply    f xs      -> sop f "$" [xs]
    TString   t         -> pp t
    TOr       l n r m   -> sexpr "OR"   [l, n, r, m]
    TAnd      l n r m   -> sexpr "AND" [l, n, r, m]
    TArgs     args      -> sexpr "TARGS" args

instance Pretty1 Variant where
  pp1 = \case
    Variant ctor ty -> sexpr "ctor" [ctor, pp ty]

instance Pretty1 Expr where
  pp1 = \case
    Let       decl body  -> "(let" `indent` decl `above` body <.> ")"
    Apply     f xs       -> sexpr "apply" (f : xs)
    Constant  constant   -> constant
    Ident     qname      -> qname
    BinOp     l o r      -> sop l (ppToText o) [r]
    UnOp        o r      -> sexpr (ppToText o) [r]
    Op          o        -> pp o
    Record    az         -> sexpr "record" az
    If        b t e      -> sexpr "if" [b, t, pp e]
    Assign    l r        -> sop l ":=" [r]
    List      l          -> sexpr "list" l
    ListAccess l ids     -> sexpr "get" (l : ids)
    Set       l          -> sexpr "set" l
    Tuple     l          -> sexpr "tuple" l
    Annot     n t        -> sop n ":" [t]
    Attrs     ts         -> sexpr "attrs" ts
    BigMap    bs         -> sexpr "big_map" bs
    Map       bs         -> sexpr "map" bs
    MapRemove k m        -> sexpr "remove_map" [k, m]
    SetRemove k s        -> sexpr "remove_set" [k, s]
    -- Indexing  a j        -> sexpr "index" [a, j]
    Case      s az       -> sexpr "case" (s : az)
    Skip                 -> "skip"
    ForLoop   j s f d b  -> sexpr "for" [j, s, f, pp d, b]
    ForBox    k mv t z b -> sexpr "for_box" [k, pp mv, pp t, z, b]
    WhileLoop f b        -> sexpr "while" [f, b]
    Seq       es         -> sexpr "seq" es
    Block     es         -> sexpr "block" es
    Lambda    ps ty b    -> sexpr "lam" $ concat [ps, [":", pp ty], ["=>", b]]
    MapPatch  z bs       -> sexpr "patch" (z : bs)
    SetPatch  z bs       -> sexpr "patch_set" (z : bs)
    RecordUpd r up       -> sexpr "update" (r : up)

instance Pretty1 Alt where
  pp1 = \case
    Alt p b -> sexpr "alt" [p, b]

instance Pretty1 MapBinding where
  pp1 = \case
    MapBinding k v -> sexpr "bind" [k, v]

instance Pretty1 FieldAssignment where
  pp1 = \case
    FieldAssignment n e -> sexpr ".=" [n, e]
    Spread n -> sexpr "..." [n]

instance Pretty1 Constant where
  pp1 = \case
    Int           z   -> pp z
    Nat           z   -> pp z
    String        z   -> pp z
    Float         z   -> pp z
    Bytes         z   -> pp z
    Tez           z   -> pp z

instance Pretty1 QualifiedName where
  pp1 = \case
    QualifiedName src path -> sexpr "." (src : path)

instance Pretty1 Pattern where
  pp1 = \case
    IsConstr     ctor arg  -> sexpr "ctor?" [ctor, pp arg]
    IsConstant   z         -> sexpr "is?" [z]
    IsVar        name      -> sexpr "?" [name]
    IsCons       h t       -> sop h "::?" [t]
    IsAnnot      s t       -> sexpr "type?" [s, t]
    IsWildcard             -> "_?"
    IsSpread     n         -> "..." <.> pp n
    IsList       l         -> sexpr "list?" l
    IsTuple      t         -> sexpr "tuple?" t

instance Pretty1 Name where
  pp1 = \case
    Name         raw -> pp raw

instance Pretty1 NameDecl where
  pp1 = \case
    NameDecl     raw -> pp raw

instance Pretty1 TypeName where
  pp1 = \case
    TypeName     raw -> pp raw

instance Pretty1 FieldName where
  pp1 = \case
    FieldName    raw -> color 4 $ pp raw

instance Pretty1 Ctor where
  pp1 = \case
    Ctor         raw -> color 5 $ pp raw

instance Pretty1 Path where
  pp1 = \case
    At n -> n
    Ix j -> pp j

instance Pretty1 TField where
  pp1 = \case
    TField      n t -> n <.> ":" `indent` t

instance Pretty1 Error where
  pp1 = \case
    Error       src children -> sexpr ("ERROR: " <> src) (map pp children)

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

instance LPP1 d QualifiedName where
  lpp1 = \case
    QualifiedName src path -> mconcat $ punctuate "." (src : path)

instance LPP1 d Contract where
  lpp1 = \case
    ContractEnd -> ""
    ContractCons x xs -> x <+> xs -- block' [x, xs]

instance LPP1 d RawContract where
  lpp1 = \case
    RawContract xs -> block' xs

instance LPP1 d Name where
  lpp1 = \case
    Name         raw -> lpp raw

instance LPP1 d NameDecl where
  lpp1 = \case
    NameDecl     raw -> lpp raw

instance LPP1 d TypeName where
  lpp1 = \case
    TypeName     raw -> lpp raw

instance LPP1 d FieldName where
  lpp1 = \case
    FieldName    raw -> lpp raw

instance LPP1 d Ctor where
  lpp1 = \case
    Ctor         raw -> lpp raw

instance LPP1 d Path where
  lpp1 = \case
    At n -> n
    Ix j -> lpp j

-- instances needed to pass instance resolution during compilation

instance LPP1 'Reason Parameters where
  lpp1 = error "unexpected `Parameters` node"

instance LPP1 'Caml Parameters where
  lpp1 = error "unexpected `Parameters` node"

instance LPP1 'Caml MapBinding where
  lpp1 = error "unexpected `MapBinding` node"

----------------------------------------------------------------------------
-- Pascal
----------------------------------------------------------------------------

instance LPP1 'Pascal Type where
  lpp1 = \case
    TVar      name      -> name
    TArrow    dom codom -> dom <+> "->" <+> codom
    TRecord   fields    -> "record [" `above` blockWith (<.> ";") fields `above` "]"
    TProduct  [element] -> element
    TProduct  elements  -> parens $ train "*" elements
    TSum      (x:xs)    -> x <.> blockWith ("|"<.>) xs
    TSum      []        -> error "looks like you've been given malformed AST" -- never called
    -- TApply    f [x]      -> f <.> x
    TApply    f xs      -> f <.> lpp xs -- TODO: does not work, elements are parsed as tuple rather than a list
    -- TApply'    f xs      -> f <.> lpp xs -- TODO: does not work, elements are parsed as tuple rather than a list
    TString   t         -> "\"" <.> lpp t <.> "\""
    TOr       l n r m   -> tuple [l, n, r, m]
    TAnd      l n r m   -> tuple [l, n, r, m]
    TArgs     args      -> tuple args

instance LPP1 'Pascal Binding where
  lpp1 = \case
    TypeDecl     n    ty       -> "type" <+> n <+> "is" <+> lpp ty
    Var          name ty value -> "var" <+> name <+> ":" <+> lpp ty <+> ":=" <+> lpp value
    Const        name ty body  -> "const" <+> name <+> ":" <+> lpp ty <+> "=" <+> lpp body
    Attribute    name          -> brackets ("@" <.> name)
    Include      fname         -> "#include" <+> pp fname
    Parameter    n t           -> "const" <+> n <+> ":" <+> lpp t

    Function _ name params ty body ->
      foldr (<+>) empty $ concat
        [ ["function"]
        , [name]
        , params
        , [":", lpp ty]
        , ["is", body]
        ]

instance LPP1 'Pascal Variant where
  lpp1 = \case -- We prepend "|" in sum type itself to be aware of the first one
    Variant ctor ty -> ctor <+> "of" <+> pp ty

instance LPP1 'Pascal Expr where
  lpp1 = \case
    Let       decl body  -> "block {" `above` decl `above` "}" <+> "with" <+> body
    Apply     f xs       -> f <+> tuple xs
    Constant  constant   -> constant
    Ident     qname      -> qname
    BinOp     l o r      -> l <+> o <+> r
    UnOp        o r      -> o <+> r
    Op          o        -> lpp o
    Record    az         -> "record [" `indent` blockWith (<.> ";") az `above` "]"
    -- TODO: context-dependent block expressions
    If        b t e      -> "if" <+> parens b <+> "then {" <+> lpp t <+> "} else {" <+> lpp e <+> "}"
    Assign    l r        -> l <+> ":=" <+> r
    List      []         -> "nil"
    List      l          -> lpp l
    ListAccess l ids     -> lpp l <.> fsep (brackets <$> ids)
    Set       l          -> "set [" <+> lpp l <+> "]"
    Tuple     l          -> tuple l
    Annot     n t        -> parens $ n <+> ":" <+> t
    BigMap    bs         -> "big_map [" `indent` train ";" bs `above` "]"
    Map       bs         -> "map [" `indent` train ";" bs `above` "]"
    MapRemove k m        -> "remove" <+> k <+> "from map" <+> m
    SetRemove k s        -> "remove" <+> k <+> "from set" <+> s
    Skip                 -> "skip"
    ForLoop   j s f d b  -> foldr (<+>) empty $
      [ "for", j, ":=", lpp s
      , "to", lpp f, "block' {", lpp d, "} with", lpp b
      ]
    ForBox    k mv t z b -> foldr (<+>) empty $
      [ "for", k
      , maybe empty ("->" <+>) mv
      , "in", lpp t
      , z, lpp b
      ]
    WhileLoop f b        -> "while" <+> lpp f <+> "block' {" `indent` lpp b `above` "}"
    Seq       es         -> block' $ map (<.>";") es
    Attrs     ts         -> mconcat $ (brackets . ("@"<+>)) <$> ts
    Lambda    ps ty b    -> "function" <+> lpp ps <+> ":" <+> lpp ty <+> "is" <+> lpp b
    MapPatch  z bs       -> "patch" <+> z <+> "with map" <+> lpp bs
    SetPatch  z bs       -> "patch" <+> z <+> "with set" <+> lpp bs
    RecordUpd r up       -> r <+> "with record" <+> lpp up
    Case      s az       -> foldr (<+>) empty $
      [ "case"
      , lpp s
      , "of\n"
      , foldr above empty $ lpp <$> az
      , "end"
      ]
    node                 -> error "unexpected `Expr` node failed with: " <+> pp node

instance LPP1 'Pascal Alt where
  lpp1 = \case
    Alt p b -> "|" <+> lpp p <+> "->" <+> lpp b

instance LPP1 'Pascal FieldAssignment where
  lpp1 = \case
    FieldAssignment n e -> lpp n <+> "=" <+> lpp e
    Spread n -> "..." <+> n

instance LPP1 'Pascal Constant where
  lpp1 = \case
    Int           z   -> lpp z
    Nat           z   -> lpp z
    String        z   -> lpp z
    Float         z   -> lpp z
    Bytes         z   -> lpp z
    Tez           z   -> lpp z

instance LPP1 'Pascal Pattern where
  lpp1 = \case
    IsConstr     ctor arg  -> ctor <+> lpp arg
    IsVar        name      -> name
    IsCons       h t       -> h <+> "::" <+> t
    IsAnnot      s t       -> parens (lpp s <+> ":" <+> lpp t)
    IsList       []        -> "nil"
    IsList       l         -> list l
    IsTuple      t         -> tuple t
    pat                    -> error "unexpected `Pattern` node failed with: " <+> pp pat

instance LPP1 'Pascal TField where
  lpp1 = \case
    TField      n t -> n <.> ":" `indent` t

instance LPP1 'Pascal MapBinding where
  lpp1 = \case
    MapBinding k v -> lpp k <+> "->" <+> lpp v

instance LPP1 'Pascal Parameters where
  lpp1 = \case
    Parameters them -> parens $ train ";" them

----------------------------------------------------------------------------
-- Reason
----------------------------------------------------------------------------

instance LPP1 'Reason Type where
  lpp1 = \case
    TVar      name      -> name
    TArrow    dom codom -> dom <+> "=>" <+> codom
    TRecord   fields    -> "{" `indent` blockWith (<.> ",") fields `above` "}"
    TProduct  elements  -> tuple elements
    TSum      (x:xs)    -> x <.> blockWith ("| "<.>) xs
    TSum      []        -> error "malformed TSum type" -- never called
    TApply    f xs      -> f <+> lpp xs
    TString   t         -> "\"" <.> lpp t <.> "\""
    TOr       l n r m   -> tuple [l, n, r, m]
    TAnd      l n r m   -> tuple [l, n, r, m]
    TArgs     args      -> train " " args

instance LPP1 'Reason Binding where
  lpp1 = \case
    TypeDecl     n    ty       -> "type" <+> n <+> "=" <+> lpp ty
    Const        name ty body  -> foldr (<+>) empty
      [ "let", name, if isJust ty then ":" <+> lpp ty else "", "=", lpp body, ";" ] -- TODO: maybe append ";" to *all* the expressions in the contract
    Attribute    name          -> brackets ("@" <.> name)
    Include      fname         -> "#include" <+> pp fname
    node                       -> error "unexpected `Binding` node failed with: " <+> pp node

instance LPP1 'Reason Variant where
  lpp1 = \case -- We prepend "|" in sum type itself to be aware of the first one
    Variant ctor ty -> ctor <+> parens (lpp ty)

instance LPP1 'Reason Expr where
  lpp1 = \case
    Let       decl body  -> "let" `indent` decl `above` "in" <+> body
    Apply     f xs       -> f <+> tuple xs
    Ident     qname      -> qname
    BinOp     l o r      -> l <+> o <+> r
    UnOp        o r      -> lpp o <+> lpp r
    Op          o        -> lpp o
    Record    az         -> "{" `indent` blockWith (<.> ",") az `above` "}"
    If        b t e      -> "if" <+> b <+> braces (lpp t) <+> "else" <+> lpp e -- TODO: primitive return values should be enclosed in braces
    List      l          -> lpp l
    ListAccess l ids     -> lpp l <.> fsep (brackets <$> ids)
    Tuple     l          -> tuple l
    Annot     n t        -> parens (n <+> ":" <+> t)
    Case      s az       -> foldr (<+>) empty $
      [ "switch"
      , lpp s
      , "{\n"
      , foldr above empty $ lpp <$> az
      , "\n}"
      ]
    Seq       es         -> train " " es
    Lambda    ps ty b    -> foldr (<+>) empty $
      [ train "," ps, if isJust ty then ":" <+> lpp ty else "", "=> {", lpp b, "}" ]
    node                 -> error "unexpected `Expr` node failed with: " <+> pp node

instance LPP1 'Reason Alt where
  lpp1 = \case
    Alt p b -> "|" <+> lpp p <+> "=>" <+> lpp b

instance LPP1 'Reason FieldAssignment where
  lpp1 = \case
    FieldAssignment n e -> lpp n <+> "=" <+> lpp e
    Spread n -> "..." <.> n

instance LPP1 'Reason Constant where
  lpp1 = \case
    Int           z   -> lpp z
    Nat           z   -> lpp z
    String        z   -> lpp z
    Float         z   -> lpp z
    Bytes         z   -> lpp z
    Tez           z   -> lpp z

instance LPP1 'Reason Pattern where
  lpp1 = \case
    IsConstr     ctor arg  -> ctor <+> lpp arg
    IsVar        name      -> name
    IsAnnot      s t       -> parens (lpp s <+> ":" <+> lpp t)
    IsWildcard             -> "_"
    IsSpread     n         -> "..." <.> lpp n
    IsList       l         -> brackets $ train "," l
    IsTuple      t         -> tuple t
    pat                    -> error "unexpected `Pattern` node failed with: " <+> pp pat

instance LPP1 'Reason TField where
  lpp1 = \case
    TField      n t -> n <.> ":" `indent` t

instance LPP1 'Reason MapBinding where
  lpp1 = \case
    MapBinding k v -> lpp k <+> "->" <+> lpp v

----------------------------------------------------------------------------
-- Caml
----------------------------------------------------------------------------

instance LPP1 'Caml Type where
  lpp1 = \case
    TVar      name      -> name
    TArrow    dom codom -> dom <+> "->" <+> codom
    TRecord   fields    -> "{" `indent` blockWith (<.> ";") fields `above` "}"
    TProduct  elements  -> tuple elements
    TSum      (x:xs)    -> x <.> blockWith ("| "<.>) xs
    TSum      []        -> error "malformed TSum type" -- never called
    TApply    f xs      -> f <+> lpp xs
    TString   t         -> "\"" <.> lpp t <.> "\""
    TOr       l n r m   -> tuple [l, n, r, m]
    TAnd      l n r m   -> tuple [l, n, r, m]
    TArgs     args      -> train " " args

instance LPP1 'Caml Binding where
  lpp1 = \case
    TypeDecl     n    ty       -> "type" <+> n <+> "=" <+> lpp ty
    Const        name ty body  -> "let" <+> name <+> ":" <+> lpp ty <+> lpp body
    Include      fname         -> "#include" <+> pp fname

    Function isRec name params ty body ->
      foldr (<+>) empty $ concat
        [ ["let"]
        , ["rec" | isRec]
        , [name]
        , params
        , [if isJust ty then ":" <+> lpp ty else empty]
        , ["=", body]
        ]
    node                      -> error "unexpected `Binding` node failed with: " <+> pp node

instance LPP1 'Caml Variant where
  lpp1 = \case -- We prepend "|" in sum type itself to be aware of the first one
    Variant ctor ty -> ctor <+> ":" <+> pp ty

instance LPP1 'Caml Expr where
  lpp1 = \case
    Let       decl body  -> "let" `indent` decl `above` "in" <+> body
    Apply     f xs       -> f <+> train " " xs
    Ident     qname      -> qname
    BinOp     l o r      -> l <+> o <+> r
    UnOp        o r      -> lpp o <+> lpp r
    Op          o        -> lpp o
    Record    az         -> "{" `indent` blockWith (<.> ";") az `above` "}"
    If        b t e      -> "if" <+> b <+> "then" <+> lpp t <+> "else" <+> lpp e
    List      l          -> lpp l
    ListAccess l ids     -> lpp l <.> fsep (brackets <$> ids)
    Tuple     l          -> tuple l
    Annot     n t        -> parens (n <+> ":" <+> t)
    Case      s az       -> foldr (<+>) empty $
      [ "match"
      , lpp s
      , "with\n"
      , foldr above empty $ lpp <$> az
      ]
    Seq       es         -> train " " es
    Lambda    ps ty b    -> foldr (<+>) empty $
      [ train "," ps, if isJust ty then ":" <+> lpp ty else "", "=>", lpp b ]
    RecordUpd r with     -> r <+> "with" <+> train ";" with
    node                 -> error "unexpected `Expr` node failed with: " <+> pp node

instance LPP1 'Caml Alt where
  lpp1 = \case
    Alt p b -> "|" <+> lpp p <+> "->" <+> lpp b

instance LPP1 'Caml FieldAssignment where
  lpp1 = \case
    FieldAssignment n e -> lpp n <+> "=" <+> lpp e
    Spread n -> "..." <.> n

instance LPP1 'Caml Constant where
  lpp1 = \case
    Int           z   -> lpp z
    Nat           z   -> lpp z
    String        z   -> lpp z
    Float         z   -> lpp z
    Bytes         z   -> lpp z
    Tez           z   -> lpp z

instance LPP1 'Caml Pattern where
  lpp1 = \case
    IsConstr     ctor arg  -> ctor <+> lpp arg
    IsVar        name      -> name
    IsAnnot      s t       -> parens (lpp s <+> ":" <+> lpp t)
    IsWildcard             -> "_"
    IsSpread     n         -> "..." <.> lpp n
    IsList       l         -> list l
    IsTuple      t         -> tuple t
    IsCons       h t       -> h <+> "::" <+> t
    pat                    -> error "unexpected `Pattern` node failed with:" <+> pp pat

instance LPP1 'Caml TField where
  lpp1 = \case
    TField      n t -> n <.> ":" `indent` t
