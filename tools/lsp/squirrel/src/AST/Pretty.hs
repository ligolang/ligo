
module AST.Pretty () where

import AST.Types
import PrettyPrint
import Parser

instance Pretty (Contract i) where
  pp = \case
    Contract _ decls ->
      hang "(* contract *)" 2 do
        vcat $ map (($$ empty) . pp) decls

    WrongContract err ->
      pp err

instance Pretty (Declaration i) where
  pp = \case
    ValueDecl _ binding -> pp binding
    WrongDecl err       -> pp err

instance Pretty (Binding i) where
  pp = \case
    Irrefutable  _ pat expr -> error "irrefs in pascaligo?"
    Function     _ isRec name params ty body ->
      hang
        ( fsep
          [ if isRec then "recursive" else empty
          , "function"
          , pp name
          , parens $ fsep $ punctuate "," $ map pp params
          , ":"
          , pp ty
          , "is"
          ]
        )
        2
        (pp body)
    WrongBinding err ->
      pp err

instance Pretty (VarDecl i) where
  pp = \case
    Decl _ mutability name ty -> fsep
      [ pp mutability
      , pp name
      , ":"
      , pp ty
      ]
    WrongVarDecl err ->
      pp err

instance Pretty (Mutable i) where
  pp = \case
    Mutable      _   -> "var"
    Immutable    _   -> "const"
    WrongMutable err -> pp err

instance Pretty (Type i) where
  pp = \case
    TArrow    _ dom codom -> parens (pp dom <+> "->" <+> pp codom)
    Record    _ fields    -> wrap ["record [", "]"] $ vcat $ map ppField fields
    TVar      _ name      -> pp name
    Sum       _ variants  -> vcat $ map ppCtor variants
    Product   _ elements  -> fsep $ punctuate " *" $ map pp elements
    TApply    _ f xs      -> pp f <> parens (fsep $ punctuate "," $ map pp xs)
    WrongType   err       -> pp err
    where
      ppField (name, ty) = pp name <> ": " <> pp ty <> ";"
      ppCtor  (ctor, fields) =
        "|" <+> pp ctor <+> parens (fsep $ punctuate "," $ map pp fields)

instance Pretty (Expr i) where
  pp = \case
    Let       _ decls body -> hang "let" 2 (vcat $ map pp decls)
                           <> hang "in"  2 (pp body)
    Apply     _ f xs       -> pp f <> parens (fsep $ punctuate "," $ map pp xs)
    Constant  _ constant   -> pp constant
    Ident     _ qname      -> pp qname
    WrongExpr   err        -> pp err


instance Pretty (Constant i) where
  pp = \case
    Int           _ c   -> int c
    String        _ c   -> doubleQuotes (pp c)
    Float         _ c   -> double c
    Bytes         _ c   -> pp c
    WrongConstant   err -> pp err

instance Pretty (QualifiedName i) where
  pp = \case
    QualifiedName _ src path -> pp src <> cat (map (("." <>) . pp) path)
    WrongQualifiedName err   -> pp err


instance Pretty (Name i) where
  pp = \case
    Name      _ raw -> pp raw
    WrongName err   -> pp err

wrap [l, r] a = hang (hang l 2 r) 0 r