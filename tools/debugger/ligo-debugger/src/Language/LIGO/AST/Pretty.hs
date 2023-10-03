{-# LANGUAGE PolyKinds, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Pretty printers for all 4 dialects and core s-expressions
-- and their corresponding `Show` instances for @AST.Skeleton@ types.

module Language.LIGO.AST.Pretty
  ( module DPretty
  , LPP (..)
  , Pretty (..)
  , TotalLPP
  , lppDialect
  , sexpr
  , blockComment
  ) where

import Prelude hiding (Alt, Sum)

import Data.Sum
import Duplo (Cofree ((:<)), Layers)
import Duplo.Pretty as DPretty
  (Doc, Modifies (..), PP (PP), Pretty (..), Pretty1 (..), above, brackets, empty, fsep, hsep,
  indent, parens, ppToText, punctuate, ($+$), (<+>), (<.>))
import Duplo.Tree (Tree)

import Language.LIGO.AST.Skeleton hiding (Type)
import Language.LIGO.AST.Skeleton qualified as AST
import Language.LIGO.Range (Range)

----------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------

class (Pretty expr) => LPP (dialect :: Lang) expr where
    lpp :: expr -> Doc
    lpp = pp

instance LPP dialect () where
instance LPP dialect Text where
instance LPP dialect Doc where

class (Pretty1 expr) => LPP1 (dialect :: Lang) (expr :: Type -> Type) where
  lpp1 :: expr Doc -> Doc
  lpp1 = pp1

instance LPP1 dialect [] where
  lpp1 = list

deriving anyclass instance LPP1 dialect Maybe

instance {-# OVERLAPPABLE #-}
    (LPP d a, LPP1 d p, Functor p)
  =>
    LPP d (p a)
  where
  lpp = lpp1 @d . fmap (lpp @d)

deriving via PP (RawContract it) instance Pretty it => Show (RawContract it)
deriving via PP (Binding it) instance Pretty it => Show (Binding it)
deriving via PP (AST.Type it) instance Pretty it => Show (AST.Type it)
deriving via PP (Variant it) instance Pretty it => Show (Variant it)
deriving via PP (TField it) instance Pretty it => Show (TField it)
deriving via PP (Expr it) instance Pretty it => Show (Expr it)
deriving via PP (Alt it) instance Pretty it => Show (Alt it)
deriving via PP (FieldAssignment it) instance Pretty it => Show (FieldAssignment it)
deriving via PP (Constant it) instance Pretty it => Show (Constant it)
deriving via PP (Pattern it) instance Pretty it => Show (Pattern it)
deriving via PP (ModuleAccess it) instance Pretty it => Show (ModuleAccess it)
deriving via PP (QualifiedName it) instance Pretty it => Show (QualifiedName it)
deriving via PP (Name it) instance Pretty it => Show (Name it)
deriving via PP (NameDecl it) instance Pretty it => Show (NameDecl it)
deriving via PP (ModuleName it) instance Pretty it => Show (ModuleName it)
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

instance LPP1 'Caml   Error where lpp1 (Error msg _) = blockComment Caml   $ pp msg
instance LPP1 'Js     Error where lpp1 (Error msg _) = blockComment Js     $ pp msg

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

sexpr :: Text -> [Doc] -> Doc
sexpr header [] = "(" <.> pp header <.> ")"
sexpr header items = "(" <.> pp header `indent` foldr above DPretty.empty items <.> ")"

sop :: Doc -> Text -> [Doc] -> Doc
sop a op b = "(" <.> a `indent` pp op `indent` foldr above DPretty.empty b <.> ")"

blockWith :: forall dialect p . LPP dialect p => (Doc -> Doc) -> [p] -> Doc
blockWith f = foldr (indent . f . lpp @dialect) DPretty.empty

block' :: [Doc] -> Doc
-- block' = foldr (<.>) empty . map ((<.> "\n") . lpp)
block' = foldr (($+$) . (<.> "\n") . lpp) DPretty.empty

list :: forall dialect p . LPP dialect p => [p] -> Doc
list = brackets . train @dialect @p ";"

train :: forall dialect p . LPP dialect p => Doc -> [p] -> Doc
train sep' = hsep . punctuate sep' . map (lpp @dialect)

tuple :: forall dialect p . LPP dialect p => [p] -> Doc
tuple = parens . train @dialect @p ","

braces :: Doc -> Doc
braces p = "{" <+> p <+> "}"

----------------------------------------------------------------------------
-- Core sexpr
----------------------------------------------------------------------------

instance Pretty1 RawContract where
  pp1 = \case
    RawContract xs -> sexpr "contract" xs

instance Pretty1 ModuleExpr where
  pp1 = \case
    ModuleExpr xs -> sexpr "module_expr" xs

instance Pretty1 Binding where
  pp1 = \case
    BTypeDecl     n    tys ty   -> sexpr "type_decl"  [n, pp tys, ty]
    BParameter    n    ty       -> sexpr "parameter"  [n, pp ty]
    BVar          name tys ty value ->
      sexpr "var"   $ concat [[name], [sexpr "type" tys | not (null tys)], [pp ty], [pp value]]
    BConst        isRec name tys ty value ->
      sexpr "const" $ concat
        [ ["rec" | isRec]
        , [name]
        , [sexpr "type" tys | not (null tys)]
        , [pp ty]
        , [pp value]
        ]
    BInclude      fname         -> sexpr "#include" [fname]
    BImport       fname vars alias   -> sexpr "#import" [pp fname, list vars, alias]
    BModuleDecl   mname _    body    -> sexpr "module" [mname, body]
    BModuleAlias  mname _    alias   -> sexpr "module" [mname, alias]

    BFunction isRec name tys params ty body ->
      sexpr "fun" $ concat
        [ ["rec" | isRec]
        , [name]
        , [sexpr "type" tys | not (null tys)]
        , params
        , [":", pp ty]
        , ["=", body]
        ]

    BSignature name sig   -> sexpr "signature" [name, sig]
    BExport decl          -> sexpr "export" [decl]
    BDeclarationSeq decls -> sexpr "declarations" decls

instance Pretty1 QuotedTypeParams where
  pp1 = \case
    QuotedTypeParam  t  -> sexpr "tparameter" [t]
    QuotedTypeParams ts -> sexpr "tparameters" ts

instance Pretty1 AST.Type where
  pp1 = \case
    TArrow     dom      codom  -> sop dom "->" [codom]
    TRecord    _        fields -> sexpr "RECORD" fields
    TSum _     variants        -> sexpr "SUM" (toList variants)
    TProduct   elements        -> sexpr "PROD" elements
    TApply     f        xs     -> sop f "$" xs
    TString    t               -> sexpr "TSTRING" [pp t]
    TWildcard                  -> "_"
    TVariable  v               -> sexpr "'" [v]
    TParen     t               -> sexpr "par" [t]
    TInt       t               -> sexpr "TINT" [pp t]
    TParameter t               -> sexpr "TPARAMETER" [pp t]

instance Pretty1 Signature where
  pp1 (Signature elements) = block' elements

instance Pretty1 SigItem where
  pp1 = \case
    SValue name typ -> sexpr "SVALUE" [name, typ]
    SType name typMb -> sexpr "STYPE" [name, fromMaybe "" typMb]

instance Pretty1 Variant where
  pp1 = \case
    Variant ctor ty -> sexpr "ctor" [ctor, pp ty]

instance Pretty1 Expr where
  pp1 = \case
    Let       decl body  -> "(let" `indent` decl `above` body <.> ")"
    Apply     f xs       -> sexpr "apply" (f : xs)
    Constant  constant   -> sexpr "constant" [constant]
    BinOp     l o r      -> sop l (ppToText o) [r]
    UnOp        o r      -> sexpr (ppToText o) [r]
    Op          o        -> pp o
    Record    az         -> sexpr "record" az
    If        b t e      -> sexpr "if" [b, t, pp e]
    Ternary   b t e      -> "(" <> pp b <> "?" <> pp t <> ":" <> pp e <> ")"
    List      l          -> sexpr "list" l
    ListAccess l ids     -> sexpr "get" (l : ids)
    Tuple     l          -> sexpr "tuple" l
    Annot     n t        -> sop n ":" [t]
    Case      s az       -> sexpr "case" (s : az)
    Return    e          -> sexpr "return" [pp e]
    Break                -> "break"
    Continue             -> "continue"
    ForOfLoop v c b      -> sexpr "for_of" [v, c, b]
    WhileLoop f b        -> sexpr "while" [f, b]
    Seq       es         -> sexpr "seq" es
    Lambda    ps tys ty b -> sexpr "lam" $ concat [ps, [sexpr "type" tys | not (null tys)], [":", pp ty], ["=>", b]]
    RecordUpd r up       -> sexpr "update" (r : up)
    CodeInj   l e        -> sexpr "%" [l, e]
    Paren     e          -> "(" <> pp e <> ")"
    SwitchStm s cs       -> sexpr "switch" (s : cs)
    AssignOp  l o r      -> sop l (ppToText o) [r]
    ForLoop   i c a b    -> sexpr "for" [pp i, pp c, pp a, pp b]
    Contract  p          -> sexpr "contract_of" [p]
    EFalse               -> "false"
    ETrue                -> "true"
    EDo       b          -> sexpr "do" b

instance Pretty1 Verbatim where
  pp1 = \case
    Verbatim v -> pp v

instance Pretty1 Alt where
  pp1 = \case
    Alt p b -> sexpr "alt" [p, b]
    Default b -> sexpr "default" [b]

instance Pretty1 FieldAssignment where
  pp1 = \case
    FieldAssignment accessors e -> sexpr ".=" (accessors <> [e])
    Spread n -> sexpr "..." [n]
    Capture accessors -> sexpr ".=" [accessors]

instance Pretty1 Constant where
  pp1 = \case
    CInt           z   -> pp z
    CNat           z   -> pp z
    CString        z   -> pp z
    CFloat         z   -> pp z
    CBytes         z   -> pp z
    CTez           z   -> pp z

instance Pretty1 ModuleAccess where
  pp1 = \case
    ModuleAccess path field -> sexpr "::" (path <> [field])

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
    IsRecord     xs        -> sexpr "record?" xs
    IsParen      x         -> "(?" <> pp x <> ")"
    IsFalse                -> "false"
    IsTrue                 -> "true"

instance Pretty1 RecordFieldPattern where
  pp1 = \case
    IsRecordField l b -> sexpr "rec_field?" [l, b]
    IsRecordCapture l -> sexpr "rec_capture?" [l]

instance Pretty1 Preprocessor where
  pp1 = \case
    Preprocessor p -> sexpr "preprocessor" [p]

instance Pretty1 PreprocessorCommand where
  pp1 = \case
    PreprocessorCommand command -> pp command

instance Pretty1 Name where
  pp1 = \case
    Name         raw -> pp raw

instance Pretty1 NameDecl where
  pp1 = \case
    NameDecl     raw -> pp raw

instance Pretty1 ModuleName where
  pp1 = \case
    ModuleName   raw -> pp raw

instance Pretty1 TypeName where
  pp1 = \case
    TypeName     raw -> pp raw

instance Pretty1 TypeVariableName where
  pp1 = \case
    TypeVariableName raw -> pp raw

instance Pretty1 FieldName where
  pp1 = \case
    FieldName    raw -> pp raw

instance Pretty1 Ctor where
  pp1 = \case
    Ctor         raw -> pp raw

instance Pretty1 Attr where
  pp1 = \case
    Attr         raw -> pp raw

instance Pretty1 TField where
  pp1 = \case
    TField      n t -> n <.> maybe "" (":" `indent`) t

instance Pretty1 Error where
  pp1 = \case
    Error src children -> sexpr "ERROR" ["\"" <> pp src <> "\"", pp children]

instance Pretty1 CaseOrDefaultStm where
  pp1 = \case
    CaseStm  c s -> sexpr "case"    [c, pp s]
    DefaultStm s -> sexpr "default" [pp s]

instance Pretty1 Direction where
  pp1 = \case
    Upto   -> sexpr "Upto"   []
    Downto -> sexpr "Downto" []

-- Orphans
type instance PrettyShow Int = ()
type instance PrettyShow Doc = ()
type instance PrettyShow Range = ()

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

instance LPP1 d ModuleAccess where
  lpp1 = \case
    ModuleAccess path field -> mconcat (punctuate "." path) <> "." <> lpp field

instance LPP1 d QualifiedName where
  lpp1 = \case
    QualifiedName src path -> mconcat $ punctuate "." (src : path)

instance LPP1 d RawContract where
  lpp1 = \case
    RawContract xs -> block' xs

instance LPP1 d ModuleExpr where
  lpp1 = \case
    ModuleExpr xs -> block' xs

instance LPP1 d Name where
  lpp1 = \case
    Name         raw -> lpp raw

instance LPP1 d NameDecl where
  lpp1 = \case
    NameDecl     raw -> lpp raw

instance LPP1 d ModuleName where
  lpp1 = \case
    ModuleName   raw -> lpp raw

instance LPP1 d TypeName where
  lpp1 = \case
    TypeName     raw -> lpp raw

instance LPP1 d FieldName where
  lpp1 = \case
    FieldName    raw -> lpp raw

instance LPP1 d Ctor where
  lpp1 = \case
    Ctor         raw -> lpp raw

instance LPP1 d Preprocessor where
  lpp1 = pp1

instance LPP1 d PreprocessorCommand where
  lpp1 = pp1

instance LPP1 d Verbatim where
  lpp1 = pp

instance LPP1 d Attr where
  lpp1 = pp1

-- instances needed to pass instance resolution during compilation

----------------------------------------------------------------------------
-- Js
----------------------------------------------------------------------------

tupleJsLIGO :: forall p . LPP 'Js p => [p] -> Doc
tupleJsLIGO = brackets . train @'Js @p ","

prettyTyVarsJs :: [Doc] -> Doc
prettyTyVarsJs []  = DPretty.empty
prettyTyVarsJs tys = "<" <.> train ", " tys <.> ">"

instance LPP1 'Js AST.Type where
  lpp1 = \case
    TArrow  dom    codom  -> dom <+> "=>" <+> codom
    TRecord layout fields ->
      let record = "{" `indent` blockWith (<.> ",") fields `above` "}" in
        case layout of
          Tree -> record
          Comb -> "/* @layout comb */" `indent` record

    TProduct  [element]           -> element
    TProduct  elements            -> tupleJsLIGO elements
    TSum      layout    (x :| xs) ->
      let sum' = x `indent` blockWith ("| "<.>) xs in
        case layout of
          Tree -> sum'
          Comb -> "/* @layout comb */ |" `indent` sum'

    TApply    f xs      -> f <+> tuple xs
    TString   t         -> lpp t
    TWildcard           -> "_"
    TVariable v         -> v
    TParen    t         -> "(" <+> lpp t <+> ")"
    TInt       t        -> t
    TParameter _t       -> error "implement"

instance LPP1 'Js Signature where
  lpp1 (Signature elements) = braces $ block' elements

instance LPP1 'Js SigItem where
  lpp1 = \case
    SValue name typ -> "const" <+> name <+> ":" <+> typ
    SType name typMb ->
      let pref = "type" <+> name in
      case typMb of
        Nothing -> pref
        Just typ -> pref <+> "=" <+> typ

instance LPP1 'Js TypeVariableName where
  lpp1 = \case
    TypeVariableName raw -> lpp raw

instance LPP1 'Js Binding where
  lpp1 = \case
    BTypeDecl     n    tys ty   -> "type" <+> lpp tys <+> n <+> "=" <+> lpp ty
    BConst _isRec name tys ty body -> foldr (<+>) DPretty.empty
      [ "let", name
      , maybe DPretty.empty (((":" <+> prettyTyVarsJs tys) <+>) . lpp) ty
      , "=", lpp body, ";"
      ]
    BInclude      fname         -> "#include" <+> pp fname
    BImport       fname _ alias -> "#import" <+> pp fname <+> pp alias
    BParameter    name ty       -> pp name <> maybe "" ((":" <+>) . lpp) ty
    BModuleDecl   mname _ body    -> "export namespace" <+> lpp mname <+> brackets (lpp body) <+> ";" -- TODO: later add information about export in AST
    BModuleAlias  mname _ alias   -> "import" <+> lpp mname <+> " = "<+> lpp alias <+> ";"
    BSignature    sname sig     -> "interface" <+> sname <+> braces sig
    node                        -> error "unexpected `Binding` node failed with: " <+> pp node

instance LPP1 'Js QuotedTypeParams where
  lpp1 = \case
    QuotedTypeParam  t  -> "<" <.> pp t <.> ">"
    QuotedTypeParams ts -> "<" <.> train "," ts <.> ">"

instance LPP1 'Js Variant where
  lpp1 = \case -- We prepend "|" in sum type itself to be aware of the first one
    Variant ctor typs -> brackets $ case typs of
      [] -> ctrInQuotes
      _ -> train "," (ctrInQuotes : typs)
      where
        ctrInQuotes = "\"" <.> lpp ctor <.> "\""

instance LPP1 'Js Expr where
  lpp1 = \case
    Apply     f xs       -> f <+> tuple xs
    BinOp     l o r      -> l <+> o <+> r
    UnOp        o r      -> lpp o <+> lpp r
    Op          o        -> lpp o
    Record    az         -> "{" `indent` blockWith (<.> ",") az `above` "}"
    If        b t e      -> "if" <+> parens b <+> braces (lpp t) <+> "else" <+> braces (lpp e)
    Ternary   b t e      -> lpp b <+> "?" <+> lpp t <+> ":" <+> lpp e
    List      l          -> lpp l
    ListAccess l ids     -> lpp l <.> fsep (brackets <$> ids)
    Tuple     l          -> tupleJsLIGO l
    Annot     n t        -> parens (n <+> ":" <+> t)
    Break                -> "break"
    Continue             -> "continue"
    Case      s az       -> foldr (<+>) DPretty.empty
      [ "match("
      , lpp s
      , ","
      , "{\n"
      , foldr above DPretty.empty $ lpp <$> az
      , "\n}"
      ]
    Seq       es         -> train ";" es
    Lambda    ps tys ty b -> foldr (<+>) DPretty.empty
      [ tuple ps
      , maybe "" (((":" <+> prettyTyVarsJs tys) <+>) . lpp) ty
      , "=> {", lpp b, "}"
      ]
    Paren     e          -> "(" <+> lpp e <+> ")"
    ForOfLoop v c b      -> "for" <+> "(const" <+> v <+> "of" <+> c <+> ") {" `indent` lpp b `above` "}"
    AssignOp l o r       -> l <+> o <+> r
    WhileLoop f b        -> "while (" <+> f <+> ") {" `indent` lpp b `above` "}"
    SwitchStm c cs       -> "switch (" <+> c <+> ") {" `indent` lpp cs `above` "}"
    Return    e          -> "return " <+> lpp e
    RecordUpd s fs       -> lpp s <+> train "," fs
    CodeInj   l e        -> "(" <+> l <+> " `" <+> e <+> ")"
    node                 -> error "unexpected `Expr` node failed with: " <+> pp node

instance LPP1 'Js Alt where
  lpp1 = \case
    Alt p b -> "(" <+> lpp p <+> ")" <+> "=>" <+> lpp b <+> ","
    Default b -> "default:" <+> b

instance LPP1 'Js FieldAssignment where
  lpp1 = \case
    FieldAssignment n e -> lpp n <+> ":" <+> lpp e
    Spread n -> "..." <.> n
    Capture n -> lpp n

instance LPP1 'Js Constant where
  lpp1 = \case
    CInt           z   -> lpp z
    CNat           z   -> lpp z <+> "as nat"
    CString        z   -> lpp z
    CFloat         z   -> lpp z
    CBytes         z   -> lpp z
    CTez           z   -> lpp z <+> "as tez"

instance LPP1 'Js Pattern where
  lpp1 = \case
    IsConstr     ctor arg  -> ctor <+> lpp arg
    IsVar        name      -> name
    IsAnnot      s t       -> parens (lpp s <+> ":" <+> lpp t)
    IsWildcard             -> "_"
    IsSpread     n         -> "..." <.> lpp n
    IsList       l         -> brackets $ train "," l
    IsTuple      t         -> brackets $ train "," t
    IsRecord     fields    -> "{" <+> train "," fields <+> "}"
    pat                    -> error "unexpected `Pattern` node failed with: " <+> pp pat

instance LPP1 'Js RecordFieldPattern where
  lpp1 = \case
    IsRecordField name body -> name <+> ":" <+> body
    IsRecordCapture name -> name

instance LPP1 'Js TField where
  lpp1 = \case
    TField      n t -> n <.> maybe "" (":" `indent`) t

instance LPP1 'Js CaseOrDefaultStm where
  lpp1 = \case
    CaseStm c  b -> "case " <+> c <+> ": " <+> lpp b
    DefaultStm b -> "default: " <+> lpp b

instance LPP1 'Js Direction where
  lpp1 = error "implement"

----------------------------------------------------------------------------
-- Caml
----------------------------------------------------------------------------

tupleCameLIGO :: LPP 'Caml p => [p] -> Doc
tupleCameLIGO = \case
  [x] -> lpp @'Caml x
  xs  -> tuple @'Caml xs

prettyTyVarsCaml :: [Doc] -> Doc
prettyTyVarsCaml []  = DPretty.empty
prettyTyVarsCaml tys = parens ("type" <+> train ", " tys)

instance LPP1 'Caml AST.Type where
  lpp1 = \case
    TArrow  dom    codom  -> dom <+> "->" <+> codom
    TRecord layout fields ->
      let record = "{" `indent` blockWith (<.> ";") fields `above` "}" in
        case layout of
          Comb -> "[@layout comb]" `indent` record
          Tree -> record

    TProduct [element]          -> element
    TProduct elements           -> parens (train " *" elements)
    TSum     layout   (x :| xs) ->
      let sum' = x `indent` blockWith ("| "<.>) xs in parens
        case layout of
          Comb -> "[@layout comb] |" `indent` sum'
          Tree -> sum'

    TApply     f xs      -> tupleCameLIGO xs <+> f
    TString    t         -> lpp t
    TWildcard            -> "_"
    TVariable  v         -> v
    TParen     t         -> parens (lpp t)
    TInt       t         -> t
    TParameter p         -> p <+> "parameter_of"

instance LPP1 'Caml Signature where
  lpp1 (Signature elements) = block' elements

instance LPP1 'Caml SigItem where
  lpp1 = \case
    SValue name typ -> "val" <+> name <+> ":" <+> typ
    SType name typMb ->
      let pref = "type" <+> name in
      case typMb of
        Nothing -> pref
        Just typ -> pref <+> "=" <+> typ

instance LPP1 'Caml TypeVariableName where
  lpp1 = \case
    TypeVariableName raw -> "'" <.> lpp raw

instance LPP1 'Caml Binding where
  lpp1 = \case
    BTypeDecl     n    tys ty   -> "type" <+> lpp tys <+> n <+> "=" <+> lpp ty
    BConst isRec name tys ty body ->
      "let" <+> bool DPretty.empty "rec" isRec <+> name <+> prettyTyVarsCaml tys <+> ":" <+> lpp ty <+> "=" <+> lpp body
    BInclude      fname         -> "#include" <+> pp fname
    BImport       fname _ alias -> "#import" <+> pp fname <+> pp alias

    BFunction isRec name tys params ty body ->
      foldr (<+>) DPretty.empty $ concat
        [ ["let"]
        , ["rec" | isRec]
        , [name]
        , [prettyTyVarsCaml tys]
        , params
        , [maybe DPretty.empty ((":" <+>) . lpp) ty]
        , ["=", body]
        ]

    BSignature sname sig      -> "module type" <+> sname <+> "= sig" <+> sig <+> "end"
    node                      -> error "unexpected `Binding` node failed with: " <+> pp node

instance LPP1 'Caml QuotedTypeParams where
  lpp1 = \case
    QuotedTypeParam  t  -> t
    QuotedTypeParams ts -> tuple ts

instance LPP1 'Caml Variant where
  lpp1 = \case -- We prepend "|" in sum type itself to be aware of the first one
    Variant ctor typs -> case typs of
      [] -> ctor
      [ty] -> ctor <+> "of" <+> parens (pp ty)
      _ -> error "Expected zero or one arg in variant type"

instance LPP1 'Caml Expr where
  lpp1 = \case
    Let       decl body  -> "let" `indent` decl `above` "in" <+> body
    Apply     f xs       -> f <+> train " " xs
    BinOp     l o r      -> l <+> o <+> r
    UnOp        o r      -> lpp o <+> lpp r
    Op          o        -> lpp o
    Record    az         -> "{" `indent` blockWith (<.> ";") az `above` "}"
    If        b t e      -> "if" <+> b <+> "then" <+> lpp t <+> "else" <+> lpp e
    List      l          -> lpp l
    ListAccess l ids     -> lpp l <.> fsep (brackets <$> ids)
    Tuple     l          -> tuple l
    Annot     n t        -> parens (n <+> ":" <+> t)
    Case      s az       -> foldr (<+>) DPretty.empty
      [ "match"
      , lpp s
      , "with\n"
      , foldr above DPretty.empty $ lpp <$> az
      ]
    Seq       es         -> train " " es
    Lambda    ps tys ty b -> foldr (<+>) DPretty.empty
      [ "fun"
      , prettyTyVarsCaml tys
      , train "," ps
      , maybe "" ((":" <+>) . lpp) ty
      , "=>", lpp b
      ]
    RecordUpd r with     -> r <+> "with" <+> train ";" with
    Paren     e          -> "(" <+> lpp e <+> ")"
    Contract  m          -> "contract_of" <+> m
    EFalse               -> "false"
    ETrue                -> "true"
    node                 -> error "unexpected `Expr` node failed with: " <+> pp node

instance LPP1 'Caml Alt where
  lpp1 = \case
    Alt p b -> "|" <+> lpp p <+> "->" <+> lpp b
    _ -> error "Default is not supported in CameLIGO"

instance LPP1 'Caml FieldAssignment where
  lpp1 = \case
    FieldAssignment n e -> lpp n <+> "=" <+> lpp e
    Spread n -> "..." <.> n
    Capture n -> lpp n

instance LPP1 'Caml Constant where
  lpp1 = \case
    CInt           z   -> lpp z
    CNat           z   -> lpp z
    CString        z   -> lpp z
    CFloat         z   -> lpp z
    CBytes         z   -> lpp z
    CTez           z   -> lpp z

instance LPP1 'Caml Pattern where
  lpp1 = \case
    IsConstr     ctor arg  -> ctor <+> lpp arg
    IsVar        name      -> name
    IsAnnot      s t       -> parens (lpp s <+> ":" <+> lpp t)
    IsWildcard             -> "_"
    IsSpread     n         -> "..." <.> lpp n
    IsList       l         -> list l
    IsTuple      t         -> train "," t
    IsCons       h t       -> h <+> "::" <+> t
    IsRecord     fields    -> "{" <+> train "," fields <+> "}"
    IsParen      x         -> parens x
    IsFalse                -> "false"
    IsTrue                 -> "true"
    pat                    -> error "unexpected `Pattern` node failed with:" <+> pp pat

instance LPP1 'Caml RecordFieldPattern where
  lpp1 = \case
    IsRecordField name body -> name <+> "=" <+> body
    IsRecordCapture name -> name

instance LPP1 'Caml TField where
  lpp1 = \case
    TField      n t -> n <.> maybe "" (":" `indent`) t

instance LPP1 'Caml CaseOrDefaultStm where
  lpp1 = \case
    CaseStm _ _  -> error "unexpected `CaseStm` node"
    DefaultStm _ -> error "unexpected `DefaultStm` node"

instance LPP1 'Caml Direction where
  lpp1 = \case
    Upto   -> "upto"
    Downto -> "downto"

----------------------------------------------------------------------------
-- General utilities
----------------------------------------------------------------------------

type TotalLPP expr = (LPP 'Caml expr, LPP 'Js expr)

lppDialect :: TotalLPP expr => Lang -> expr -> Doc
lppDialect dialect = case dialect of
  Caml -> lpp @'Caml
  Js   -> lpp @'Js

blockComment :: Lang -> Doc -> Doc
blockComment dialect contents = case dialect of
  Caml -> "(*" <+> contents <+> "*)"
  Js   -> "/*" <+> contents <+> "*/"
