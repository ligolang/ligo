
module AST.StubErrors where

-- import Data.Functor ((<&>))
-- import Data.Text (Text)

-- import Duplo.Tree
-- import Duplo.Error
-- import Duplo.Lattice
-- import Duplo.Pretty

-- import AST.Skeleton
-- import Product

-- stubErrors
--   :: forall xs
--   .  ( Lattice  (Product xs)
--      , Modifies (Product xs)
--      )
--   => Product xs
--   -> LIGO xs
--   -> LIGO xs
-- stubErrors stub = go transform
--   where
--     go f (a :< fs) = f (a :< fmap (go f) fs)

--     transform (match -> Just (r, RawContract decls)) =
--       make (r, RawContract $ filter isNotAnError decls)

--     transform tree@(match -> Just (r, binding)) = case binding of
--       Irrefutable p expr ->
--         make (r, Irrefutable
--           (p    ? IsWildcard)
--           (expr ? Nat "1")
--         )

--       Function recur name args ty body ->
--         name ! make (r, Function recur name
--           (filter isNotAnError args)
--           (ty <&> (? unitTy))
--           (body    ? unit)
--         )

--       Var      name ty value -> name ! make (r, Var      name (ty <&> (? unitTy)) (value <&> (? unit)))
--       Const    name ty value -> name ! make (r, Const    name (ty <&> (? unitTy)) (value <&> (? unit)))
--       TypeDecl name ty       -> name ! make (r, TypeDecl name (ty ? unitTy))

--       Attribute attr -> attr ! tree
--       Include   name -> name ! tree

--     transform tree@(match -> Just (r, Parameters xs)) =
--       make (r, Parameters (filter isNotAnError xs))

--     transform tree@(match -> Just (r, ty)) = case ty of
--       TArrow   dom codom -> make (r, TArrow   (dom ? TypeName "__domain") (codom ? TypeName "__image"))
--       TRecord  fields    -> make (r, TRecord  (filter isNotAnError fields))
--       TVar     name      -> name ! tree
--       TSum     variants  -> make (r, TSum     (filter isNotAnError variants))
--       TProduct elements  -> make (r, TProduct (filter isNotAnError elements))
--       TApply   f xs      -> f ! make (r, TApply f (filter isNotAnError xs))
--       TTuple   xs        -> make (r, TTuple   (filter isNotAnError xs))
--       TString  t         -> tree
--       TOr      a b c d   -> a ! c ! make (r, TOr a (b ? unitTy) c (d ? unitTy))
--       TAnd     a b c d   -> a ! c ! make (r, TOr a (b ? unitTy) c (d ? unitTy))

--     transform tree@(match -> Just (r, Variant cName ty)) = cName ! make (r, Variant cName (ty <&> (? unitTy)))
--     transform tree@(match -> Just (r, TField  fname ty)) = fname ! make (r, TField  fname (ty      ? unitTy))

--     transform tree@(match -> Just (r, expr)) = case expr of
--       Let      decl body -> if isError decl then body else tree
--       Apply    f xs      -> make (r, Apply (f ? Name "__function") (filter isNotAnError xs))
--       Constant c         -> make (r, Constant (c ? Nat "1"))
--       Ident    i         -> make (r, Name "__ident")
--       BinOp    l op r'   -> make (r, BinOp (l ? Name "__left_arg") (op ? Op "+") (r' ? Name "__right_arg"))
--       UnOp     op r'     -> make (r, UnOp (op ? Op "+") (r' ? Name "__arg"))
--       Op       t         -> tree
--       Record   xs        -> make (r, Record (filter isNotAnError xs))
--       If       i t e     -> make (r, If (i ? Name "__cond") (t ? Name "__then") (e <&> (? Name "__else")))
--       Assign   lhs rhs   -> make (r, Assign (lhs ? Name "__lhs") (rhs ? Name "__rhs"))
--       List     xs        -> make (r, List (filter isNotAnError xs))
--       ListAccess l ixs   -> make (r, ListAccess (l ? Name "__list") (filter isNotAnError ixs))
--       Set      xs        -> make (r, Set   (filter isNotAnError xs))
--       Tuple    xs        -> make (r, Tuple (filter isNotAnError xs))
--       Annot    e t       -> make (r, Annot (e ? Name "__expr") (t ? TypeName "__type"))
--       Attrs    attrs     -> make (r, Attrs  (filter isNotAnError attrs))
--       BigMap   xs        -> make (r, BigMap (filter isNotAnError xs))
--       Map      xs        -> make (r, Map    (filter isNotAnError xs))
--       MapRemove m k      -> make (r, MapRemove (m ? Name "__map") (k ? Name "__key"))
--       SetRemove m k      -> make (r, SetRemove (m ? Name "__set") (k ? Name "__key"))
--       Indexing  q n      -> make (r, Indexing (q ? Name "__source") (n ? Name "__index"))
--       Case      s alts   -> make (r, Case (s ? Name "__subject") (filter isNotAnError alts))
--       Skip               -> tree
--       ForLoop k v f d b  -> make (r, ForLoop (k ? Name "__key") (v ? Name "__from") (f ? Name "__to") (d <&> (? Name "__step")) (b ? Name "__body"))
--       WhileLoop c b      -> make (r, WhileLoop (c ? Name "__cond") (b ? Name "__body"))
--       Seq       xs       -> make (r, Seq (filter isNotAnError xs))
--       Lambda    xs ty b  -> make (r, Lambda (filter isNotAnError xs) (ty <&> (? TypeName "__type")) (b ? Name "__body"))
--       ForBox    k mv f d b -> make (r, ForBox (k ? Name "__key") (mv <&> (? Name "__value")) (f ? Name "__kind") (d ? Name "map") (b ? Name "__body"))
--       MapPatch  m bs     -> make (r, MapPatch (m ? Name "__map") (filter isNotAnError bs))
--       SetPatch  m bs     -> make (r, SetPatch (m ? Name "__set") (filter isNotAnError bs))
--       RecordUpd r' bs    -> make (r, RecordUpd (r' ? Name "__rec") (filter isNotAnError bs))

--     transform tree@(match -> Just (r, Alt p b))  = make (r, Alt (p ? IsWildcard) (b ? Name "__body"))
--     transform tree@(match -> Just (r, MapBinding k v)) = make (r, MapBinding (k ? Name "__key") (v ? Name "__value"))
--     transform tree@(match -> Just (r, fa)) = case fa of
--       FieldAssignment n e -> make (r, FieldAssignment (n ? Name "__field") (e ? Name "__assigned"))
--       Spread s            -> make (r, Spread (s ? Name "__spread"))

--     transform tree@(match -> Just (r, pat)) = case pat of
--       IsConstr c mp -> make (r, IsConstr (c ? Name "Constr__") (mp <&> (? IsWildcard)))
--       IsConstant c  -> make (r, IsConstant (c ? Nat "1"))
--       IsVar      n  -> make (r, IsVar (n ? Name "__is_var"))
--       IsCons    h t -> make (r, IsCons (h ? IsWildcard) (t ? IsWildcard))
--       IsAnnot   p t -> make (r, IsAnnot (p ? IsWildcard) (t ? TypeName "__type"))
--       IsWildcard    -> tree
--       IsSpread n    -> make (r, IsSpread (n ? Name "__is_spread"))
--       IsList   xs   -> make (r, IsList (filter isNotAnError xs))
--       IsTuple  xs   -> make (r, IsTuple (filter isNotAnError xs))

--     transform tree@(match -> Just (r, QualifiedName n ixs)) =
--       make (r, QualifiedName (n ? Name "__box") (filter isNotAnError ixs))

--     transform tree@(match -> Just (r, path)) = case path of
--       At n -> make (r, At (n ? Name "__ix"))
--       _    -> tree

--     transform tree@(match -> Just (r, TypeName t)) = tree
--     transform tree@(match -> Just (r, Name t)) = tree
--     transform tree@(match -> Just (r, Ctor t)) = tree
--     transform tree@(match -> Just (r, FieldName t)) = tree

--     transform tree = error $ show $ "transform: " <> ppToText tree

--     isNotAnError = not . isError

--     isError (match -> Just (_, Err t)) = const True (t :: Text)
--     isError  _                         = False

--     unit   = Tuple []
--     unitTy = TTuple []

--     (?)
--       :: (Element f RawLigoList, Foldable f)
--       => LIGO xs
--       -> f (LIGO xs)
--       -> LIGO xs
--     x ? def = if isError x then make (stub, def) else x

--     (!) :: LIGO xs -> LIGO xs -> LIGO xs
--     (!) x rest = if isError x then x else rest
