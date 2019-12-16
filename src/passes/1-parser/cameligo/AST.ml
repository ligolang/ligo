(* Abstract Syntax Tree (AST) for CameLIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30-40-42"]

(* Utilities *)

open Utils

(* Regions

   The AST carries all the regions where tokens have been found by the
   lexer, plus additional regions corresponding to whole subtrees
   (like entire expressions, patterns etc.). These regions are needed
   for error reporting and source-to-source transformations. To make
   these pervasive regions more legible, we define singleton types for
   the symbols, keywords etc. with suggestive names like "kwd_and"
   denoting the _region_ of the occurrence of the keyword "and".
*)

type 'a reg = 'a Region.reg

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nsepseq_to_region to_region (hd,tl) =
  let reg (_, item) = to_region item in
  Region.cover (to_region hd) (last reg tl)

(* Keywords of OCaml *)

type keyword   = Region.t
type kwd_and   = Region.t
type kwd_begin = Region.t
type kwd_else  = Region.t
type kwd_end   = Region.t
type kwd_false = Region.t
type kwd_fun   = Region.t
type kwd_if    = Region.t
type kwd_in    = Region.t
type kwd_let   = Region.t
type kwd_match = Region.t
type kwd_mod   = Region.t
type kwd_not   = Region.t
type kwd_of    = Region.t
type kwd_or    = Region.t
type kwd_then  = Region.t
type kwd_true  = Region.t
type kwd_type  = Region.t
type kwd_with  = Region.t
type kwd_let_entry = Region.t

(* Data constructors *)

type c_None  = Region.t
type c_Some  = Region.t

(* Symbols *)

type arrow    = Region.t  (* "->" *)
type cons     = Region.t  (* "::" *)
type cat      = Region.t  (* "^"  *)
type append   = Region.t  (* "@"  *)
type dot      = Region.t  (* "."  *)

(* Arithmetic operators *)

type minus    = Region.t  (* "-" *)
type plus     = Region.t  (* "+" *)
type slash    = Region.t  (* "/" *)
type times    = Region.t  (* "*" *)

(* Boolean operators *)

type bool_or  = Region.t  (* "||" *)
type bool_and = Region.t  (* "&&" *)

(* Comparisons *)

type equal = Region.t  (* "="  *)
type neq   = Region.t  (* "<>" *)
type lt    = Region.t  (* "<"  *)
type gt    = Region.t  (* ">"  *)
type leq   = Region.t  (* "=<" *)
type geq   = Region.t  (* ">=" *)

(* Compounds *)

type lpar     = Region.t  (* "(" *)
type rpar     = Region.t  (* ")" *)
type lbracket = Region.t  (* "[" *)
type rbracket = Region.t  (* "]" *)
type lbrace   = Region.t  (* "{" *)
type rbrace   = Region.t  (* "}" *)

(* Separators *)

type comma = Region.t  (* "," *)
type semi  = Region.t  (* ";" *)
type vbar  = Region.t  (* "|" *)
type colon = Region.t  (* ":" *)

(* Wildcard *)

type wild = Region.t  (* "_" *)

(* Virtual tokens *)

type eof = Region.t

(* Literals *)

type variable    = string reg
type fun_name    = string reg
type type_name   = string reg
type field_name  = string reg
type type_constr = string reg
type constr      = string reg

(* Parentheses *)

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

type the_unit = lpar * rpar

(* The Abstract Syntax Tree *)

type t = {
  decl : declaration nseq;
  eof  : eof
}

and ast = t

and declaration =
  Let      of (kwd_let * let_binding) reg
| TypeDecl of type_decl reg

(* Non-recursive values *)

and let_binding = {
  binders  : pattern nseq;
  lhs_type : (colon * type_expr) option;
  eq       : equal;
  let_rhs  : expr
}

(* Type declarations *)

and type_decl = {
  kwd_type   : kwd_type;
  name       : type_name;
  eq         : equal;
  type_expr  : type_expr
}

and type_expr =
  TProd   of cartesian
| TSum    of (variant reg, vbar) nsepseq reg
| TRecord of field_decl reg ne_injection reg
| TApp    of (type_constr * type_tuple) reg
| TFun    of (type_expr * arrow * type_expr) reg
| TPar    of type_expr par reg
| TVar    of variable

and cartesian = (type_expr, times) nsepseq reg

and variant = {
  constr : constr;
  arg    : (kwd_of * type_expr) option
}

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr
}

and type_tuple = (type_expr, comma) nsepseq par reg

and pattern =
  PConstr of constr_pattern
| PUnit   of the_unit reg
| PFalse  of kwd_false
| PTrue   of kwd_true
| PVar    of variable
| PInt    of (Lexer.lexeme * Z.t) reg
| PNat    of (Lexer.lexeme * Z.t) reg
| PBytes  of (Lexer.lexeme * Hex.t) reg
| PString of string reg
| PWild   of wild
| PList   of list_pattern
| PTuple  of (pattern, comma) nsepseq reg
| PPar    of pattern par reg
| PRecord of field_pattern reg ne_injection reg
| PTyped  of typed_pattern reg

and constr_pattern =
  PNone      of c_None
| PSomeApp   of (c_Some * pattern) reg
| PConstrApp of (constr * pattern option) reg

and list_pattern =
  PListComp of pattern injection reg
| PCons     of (pattern * cons * pattern) reg

and typed_pattern = {
  pattern   : pattern;
  colon     : colon;
  type_expr : type_expr
}

and field_pattern = {
  field_name : field_name;
  eq         : equal;
  pattern    : pattern
}

and expr =
  ECase   of expr case reg
| ECond   of cond_expr reg
| EAnnot  of (expr * colon * type_expr) par reg
| ELogic  of logic_expr
| EArith  of arith_expr
| EString of string_expr
| EList   of list_expr
| EConstr of constr_expr
| ERecord of field_assign reg ne_injection reg
| EProj   of projection reg
| EVar    of variable
| ECall   of (expr * expr nseq) reg
| EBytes  of (string * Hex.t) reg
| EUnit   of the_unit reg
| ETuple  of (expr, comma) nsepseq reg
| EPar    of expr par reg
| ELetIn  of let_in reg
| EFun    of fun_expr reg
| ESeq    of expr injection reg

and 'a injection = {
  compound   : compound;
  elements   : ('a, semi) sepseq;
  terminator : semi option
}

and 'a ne_injection = {
  compound    : compound;
  ne_elements : ('a, semi) nsepseq;
  terminator  : semi option
}

and compound =
  BeginEnd of kwd_begin * kwd_end
| Braces   of lbrace * rbrace
| Brackets of lbracket * rbracket

and list_expr =
  ECons     of cons bin_op reg
| EListComp of expr injection reg
  (*| Append of (expr * append * expr) reg*)

and string_expr =
  Cat    of cat bin_op reg
| String of string reg

and constr_expr =
  ENone      of c_None
| ESomeApp   of (c_Some * expr) reg
| EConstrApp of (constr * expr option) reg

and arith_expr =
  Add   of plus bin_op reg
| Sub   of minus bin_op reg
| Mult  of times bin_op reg
| Div   of slash bin_op reg
| Mod   of kwd_mod bin_op reg
| Neg   of minus un_op reg
| Int   of (string * Z.t) reg
| Nat   of (string * Z.t) reg
| Mutez of (string * Z.t) reg

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or    of kwd_or bin_op reg
| And   of kwd_and bin_op reg
| Not   of kwd_not un_op reg
| True  of kwd_true
| False of kwd_false

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

and comp_expr =
  Lt    of lt    bin_op reg
| Leq   of leq   bin_op reg
| Gt    of gt    bin_op reg
| Geq   of geq   bin_op reg
| Equal of equal bin_op reg
| Neq   of neq   bin_op reg

and projection = {
  struct_name : variable;
  selector    : dot;
  field_path  : (selection, dot) nsepseq
}

and selection =
  FieldName of variable
| Component of (string * Z.t) reg

and field_assign = {
  field_name : field_name;
  assignment : equal;
  field_expr : expr
}

and 'a case = {
  kwd_match : kwd_match;
  expr      : expr;
  kwd_with  : kwd_with;
  lead_vbar : vbar option;
  cases     : ('a case_clause reg, vbar) nsepseq reg
}

and 'a case_clause = {
  pattern : pattern;
  arrow   : arrow;
  rhs     : 'a
}

and let_in = {
  kwd_let : kwd_let;
  binding : let_binding;
  kwd_in  : kwd_in;
  body    : expr
}

and fun_expr = {
  kwd_fun  : kwd_fun;
  binders  : pattern nseq;
  lhs_type : (colon * type_expr) option;
  arrow    : arrow;
  body     : expr
}

and cond_expr = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  ifso     : expr;
  kwd_else : kwd_else;
  ifnot    : expr
}

(* Projecting regions of the input source code *)

let type_expr_to_region = function
  TProd {region; _}
| TSum {region; _}
| TRecord {region; _}
| TApp {region; _}
| TFun {region; _}
| TPar {region; _}
| TVar {region; _} -> region

let list_pattern_to_region = function
  PListComp {region; _} | PCons {region; _} -> region

let constr_pattern_to_region = function
  PNone region | PSomeApp {region;_}
| PConstrApp {region;_} -> region

let pattern_to_region = function
| PList p -> list_pattern_to_region p
| PConstr c -> constr_pattern_to_region c
| PUnit {region;_}
| PTrue region | PFalse region
| PTuple {region;_} | PVar {region;_}
| PInt {region;_}
| PString {region;_} | PWild region
| PPar {region;_}
| PRecord {region; _} | PTyped {region; _}
| PNat {region; _} | PBytes {region; _}
  -> region

let bool_expr_to_region = function
  Or {region;_} | And {region;_}
| True region | False region
| Not {region;_} -> region

let comp_expr_to_region = function
  Lt {region;_} | Leq {region;_}
| Gt {region;_} | Geq {region;_}
| Neq {region;_} | Equal {region;_} -> region

let logic_expr_to_region = function
  BoolExpr e -> bool_expr_to_region e
| CompExpr e -> comp_expr_to_region e

let arith_expr_to_region = function
  Add {region;_} | Sub {region;_} | Mult {region;_}
| Div {region;_} | Mod {region;_} | Neg {region;_}
| Int {region;_} | Mutez {region; _}
| Nat {region; _} -> region

let string_expr_to_region = function
  String {region;_} | Cat {region;_} -> region

let list_expr_to_region = function
  ECons {region; _} | EListComp {region; _}
(* | Append {region; _}*) -> region

and constr_expr_to_region = function
  ENone region
| EConstrApp {region; _}
| ESomeApp   {region; _} -> region

let expr_to_region = function
  ELogic e -> logic_expr_to_region e
| EArith e -> arith_expr_to_region e
| EString e -> string_expr_to_region e
| EList e -> list_expr_to_region e
| EConstr e -> constr_expr_to_region e
| EAnnot {region;_ } | ELetIn {region;_} | EFun {region;_}
| ECond {region;_} | ETuple {region;_} | ECase {region;_}
| ECall {region;_} | EVar {region; _} | EProj {region; _}
| EUnit {region;_} | EPar {region;_} | EBytes {region; _}
| ESeq {region; _} | ERecord {region; _} -> region

let selection_to_region = function
  FieldName f -> f.region
| Component c -> c.region
