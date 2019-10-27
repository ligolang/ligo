[@@@warning "-30-40-42"]

(* Abstract Syntax Tree (AST) for Mini-ML *)

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
  decl : declaration Utils.nseq;
  eof  : eof
}

and ast = t

and declaration =
  Let      of (kwd_let * let_binding) reg
| LetEntry of (kwd_let_entry * let_binding) reg
| TypeDecl of type_decl reg

(* Non-recursive values *)

and let_binding = {
  bindings : pattern list;
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
| TSum    of (variant reg, vbar) Utils.nsepseq reg
| TRecord of record_type
| TApp    of (type_constr * type_tuple) reg
| TFun    of (type_expr * arrow * type_expr) reg
| TPar    of type_expr par reg
| TAlias  of variable

and cartesian = (type_expr, times) Utils.nsepseq reg

and variant = {
  constr : constr;
  args   : (kwd_of * cartesian) option
}

and record_type = field_decl reg injection reg

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr
}

and type_tuple = (type_expr, comma) Utils.nsepseq par reg

and pattern =
  PTuple  of (pattern, comma) Utils.nsepseq reg
| PList   of list_pattern
| PVar    of variable
| PUnit   of the_unit reg
| PInt    of (string * Z.t) reg
| PTrue   of kwd_true
| PFalse  of kwd_false
| PString of string reg
| PWild   of wild
| PPar    of pattern par reg
| PConstr of (constr * pattern option) reg
| PRecord of record_pattern
| PTyped  of typed_pattern reg

and list_pattern =
  Sugar of pattern injection reg
| PCons of (pattern * cons * pattern) reg

and typed_pattern = {
  pattern   : pattern;
  colon     : colon;
  type_expr : type_expr
}

and record_pattern = field_pattern reg injection reg

and field_pattern = {
  field_name : field_name;
  eq         : equal;
  pattern    : pattern
}

and expr =
  ECase   of expr case reg
| EAnnot  of annot_expr reg
| ELogic  of logic_expr
| EArith  of arith_expr
| EString of string_expr
| EList   of list_expr
| EConstr of constr_expr reg
| ERecord of record_expr
| EProj   of projection reg
| EVar    of variable
| ECall   of (expr * expr Utils.nseq) reg
| EBytes  of (string * Hex.t) reg
| EUnit   of the_unit reg
| ETuple  of (expr, comma) Utils.nsepseq reg
| EPar    of expr par reg
| ELetIn  of let_in reg
| EFun    of fun_expr reg
| ECond   of conditional reg
| ESeq    of sequence

and constr_expr = constr * expr option

and annot_expr = expr * type_expr

and 'a injection = {
  opening    : opening;
  elements   : ('a, semi) Utils.sepseq;
  terminator : semi option;
  closing    : closing
}

and opening =
  Begin    of kwd_begin
| With     of kwd_with
| LBrace   of lbrace
| LBracket of lbracket

and closing =
  End      of kwd_end
| RBrace   of rbrace
| RBracket of rbracket

and list_expr =
  Cons   of cons bin_op reg
| List   of expr injection reg
  (*| Append of (expr * append * expr) reg*)

and string_expr =
  Cat    of cat bin_op reg
| String of string reg

and arith_expr =
  Add  of plus bin_op reg
| Sub  of minus bin_op reg
| Mult of times bin_op reg
| Div  of slash bin_op reg
| Mod  of kwd_mod bin_op reg
| Neg  of minus un_op reg
| Int  of (string * Z.t) reg
| Nat  of (string * Z.t) reg
| Mutez  of (string * Z.t) reg

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or       of kwd_or bin_op reg
| And      of kwd_and bin_op reg
| Not      of kwd_not un_op reg
| True     of kwd_true
| False    of kwd_false

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
  field_path  : (selection, dot) Utils.nsepseq
}

and selection =
  FieldName of variable
| Component of (string * Z.t) reg par reg

and record_expr = field_assign reg injection reg

and field_assign = {
  field_name : field_name;
  assignment : equal;
  field_expr : expr
}

and sequence = expr injection reg

and 'a case = {
  kwd_match : kwd_match;
  expr      : expr;
  opening   : opening;
  lead_vbar : vbar option;
  cases     : ('a case_clause reg, vbar) Utils.nsepseq reg;
  closing   : closing
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
  kwd_fun : kwd_fun;
  params  : pattern list;
  p_annot : (colon * type_expr) option;
  arrow   : arrow;
  body    : expr
}

and conditional = {
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
| TAlias {region; _} -> region

let list_pattern_to_region = function
  Sugar {region; _} | PCons {region; _} -> region

let pattern_to_region = function
  PList p -> list_pattern_to_region p
| PTuple {region;_} | PVar {region;_}
| PUnit {region;_} | PInt {region;_}
| PTrue region | PFalse region
| PString {region;_} | PWild region
| PConstr {region; _} | PPar {region;_}
| PRecord {region; _} | PTyped {region; _} -> region

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
  Cons {region; _} | List {region; _}
(* | Append {region; _}*) -> region

let expr_to_region = function
  ELogic e -> logic_expr_to_region e
| EArith e -> arith_expr_to_region e
| EString e -> string_expr_to_region e
| EList e -> list_expr_to_region e
| EAnnot {region;_ } | ELetIn {region;_} | EFun {region;_}
| ECond {region;_} | ETuple {region;_} | ECase {region;_}
| ECall {region;_} | EVar {region; _} | EProj {region; _}
| EUnit {region;_} | EPar {region;_} | EBytes {region; _}
| ESeq {region; _} | ERecord {region; _}
| EConstr {region; _} -> region

let rec unpar = function
  EPar {value={inside=expr;_}; _} -> unpar expr
| e -> e
